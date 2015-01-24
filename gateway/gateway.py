from twisted.internet import protocol, defer, task
from twisted.application import service
from twisted.words.protocols import irc

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
            # TODO: publish message

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

class IrcBot(service.Service):
    def __init__(self, settings):
        self.settings = settings

    def makeIRC(self):
        f = IRCFactory(self, self.settings)
        self.irc_factory = f
        return f
