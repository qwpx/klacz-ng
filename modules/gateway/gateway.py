from twisted.internet import protocol, defer, task
from twisted.application import service
from twisted.words.protocols import irc

import pika

from pika.adapters import twisted_connection

from klaczng.irc_pb2 import UserCommand, GatewayCommand

class IRC(irc.IRCClient, object):
    def connectionMade(self):
        irc.IRCClient.connectionMade(self)

    def connectionLost(self, reason):
        irc.IRCClient.connectionLost(self, reason)

    def signedOn(self):
        self.msg("NickServ", "IDENTIFY %s" % self.nickserv_password)
        for channel in self.factory.channels:
            self.join(channel.encode('ascii'))

    def privmsg(self, user, channel, body):
        if body.startswith(','):
            user_command = UserCommand()
            user_command.caller = user.split('!', 1)[0]
            user_command.replyTo = user if self.nickname == channel else channel
            command_and_args = body[1:].split(' ', 1)
            command = command_and_args[0]
            args = "" if len(command_and_args) == 1 else command_and_args[1]
            user_command.command = command
            user_command.args = args
            self.factory.service.pub('klacz.command', user_command.SerializeToString())

    def handleCommand(self, command, prefix, params):
        super(IRC, self).handleCommand(command, prefix, params)

class IRCFactory(protocol.ReconnectingClientFactory):
    def __init__(self, svc, settings):
        #protocol.ReconnectingClientFactory.__init__(self)
        self.service = svc
        self.settings = settings
        self.channels = self.settings['channels']
        self.nickname = self.settings['nickname'].encode('utf-8')
        self.nickserv_password = self.settings['nickserv_password'].encode('utf-8')

    def buildProtocol(self, addr):
        self.resetDelay()
        gateway = IRC()
        gateway.factory = self
        gateway.nickname = self.nickname
        gateway.nickserv_password = self.nickserv_password
        self.instance = gateway

        return gateway

    #def clientConnectionLost(self, connector, reason):
        #connector.connect()

    #def clientConnectionFailed(self, connector, reason):
        #print "connection failed:", reason
        #reactor.stop()

class RMQFactory(protocol.ReconnectingClientFactory):
    def __init__(self, svc, params):
        #protocol.ClientFactory.__init__(self)
        self.service = svc
        self.params = params

    @defer.inlineCallbacks
    def _run(self, connection):
        self.channel = yield connection.channel()
        yield self.channel.exchange_declare(exchange='events',type='topic')
        yield self.channel.queue_declare(queue='klacz.gateway', auto_delete=False, exclusive=False)
        #TODO: why, wut
        #yield channel.queue_bind(exchange='topic_link',queue='hello',routing_key='hello.world')
        #yield channel.basic_qos(prefetch_count=1)
        queue_object, consumer_tag = yield self.channel.basic_consume(queue='klacz.gateway',no_ack=True)

        @defer.inlineCallbacks
        def _loop(q):
            ch,method,properties,body = yield q.get()
            gateway_command = GatewayCommand()
            gateway_command.ParseFromString(body)
            yield self.service.handle_gateway_command(gateway_command)

        l = task.LoopingCall(_loop, queue_object)
        l.start(0.1) #lel

    def _ready_err(self, reason):
        print "Connection to RabbitMQ failed, reason: %s" % reason

    def buildProtocol(self, addr):
        self.resetDelay()
        proto = twisted_connection.TwistedProtocolConnection(self.params)
        proto.ready.addCallback(self._run)
        proto.ready.addErrback(self._ready_err)
        proto.factory = self
        return proto

class IrcBot(service.Service):
    def __init__(self, settings, uri):
        self.settings = settings
        self.params = pika.URLParameters(uri)

    def handle_gateway_command(self, gateway_command):
        print "got command: %s" % gateway_command
        line = gateway_command.command
        if len(gateway_command.params) > 0:
            line += " %s" % ' '.join(gateway_command.params)
        if gateway_command.HasField('rest'):
            line += " :%s" % gateway_command.rest

        self.irc_factory.instance.sendLine(line.encode("utf-8"))

    def pub(self, key, body):
        #TODO: wait for rmq connection
        return self.rmq_factory.channel.basic_publish('events', key, body)

    def makeIRC(self):
        f = IRCFactory(self, self.settings)
        self.irc_factory = f
        return f

    def makeRMQ(self):
        f = RMQFactory(self, self.params)
        self.rmq_factory = f
        return f
