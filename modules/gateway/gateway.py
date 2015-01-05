from twisted.internet import protocol, defer, task
from twisted.application import service
from twisted.words.protocols import irc
import json
import pika
#from pika import exceptions
from pika.adapters import twisted_connection

class IRC(irc.IRCClient):
    def connectionMade(self):
        irc.IRCClient.connectionMade(self)

    def connectionLost(self, reason):
        irc.IRCClient.connectionLost(self, reason)

    def signedOn(self):
        for channel in self.factory.channels:
            self.join(channel.encode('ascii'))

    def privmsg(self, user, channel, body):
        user = user.split('!', 1)[0]

        msg = {
            'replyTo': user if self.nickname == channel else channel,
            'from': user
        }

        if body.startswith(','):
            words = body.split(' ')
            command = words[0][1:]
            msg["content"] = " ".join(words[1:])

            self.factory.service.pub('klacz.privmsg.' + command,
                                              json.dumps(msg))
        else:
            msg["content"] = body
            self.factory.service.pub('klacz.privmsg', json.dumps(msg))
        #self.msg(channel, "test")

class IRCFactory(protocol.ReconnectingClientFactory):
    def __init__(self, svc, channels, nickname):
        #protocol.ReconnectingClientFactory.__init__(self)
        self.service = svc
        self.channels = channels
        self.nickname = nickname.encode('utf-8')

    def buildProtocol(self, addr):
        gateway = IRC()
        gateway.factory = self
        gateway.nickname = self.nickname
        self.instance = gateway

        return gateway

    #def clientConnectionLost(self, connector, reason):
        #connector.connect()

    #def clientConnectionFailed(self, connector, reason):
        #print "connection failed:", reason
        #reactor.stop()
class RMQFactory(protocol.ClientFactory):
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
            print 'loop'
            ch,method,properties,body = yield q.get()
            yield self.service.msg(json.loads(body))
        l = task.LoopingCall(_loop, queue_object)
        l.start(0.1) #lel


    def buildProtocol(self, addr):
        proto = twisted_connection.TwistedProtocolConnection(self.params)
        proto.ready.addCallback(self._run)
        proto.factory = self
        return proto

class IrcBot(service.Service):
    def __init__(self, settings, uri):
        self.settings = settings
        self.params = pika.URLParameters(uri)

    def msg(self, data):
        print data
        self.irc_factory.instance.msg(data['replyTo'].encode('utf-8'),
                data['content'].encode('utf-8'))

    def pub(self, key, body):
        #TODO: wait for rmq connection
        return self.rmq_factory.channel.basic_publish('events', key, body)

    def makeIRC(self):
        channels = self.settings['channels']
        nickname = self.settings['nickname']
        f = IRCFactory(self, channels, nickname)
        self.irc_factory = f
        return f

    def makeRMQ(self):
        f = RMQFactory(self, self.params)
        self.rmq_factory = f
        return f
