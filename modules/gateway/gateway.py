from twisted.internet import reactor, protocol
from twisted.words.protocols import irc

class Gateway(irc.IRCClient):
    def connectionMade(self):
        irc.IRCClient.connectionMade(self)

    def connectionLost(self, reason):
        irc.IRCClient.connectionLost(self, reason)

    def signedOn(self):
        for channel in self.factory.channels:
            self.join(channel.encode('ascii'))

    def privmsg(self, user, channel, body):
        self.msg(channel, "test")

class GatewayFactory(protocol.ClientFactory):
    def __init__(self, channels, nickname, uri):
        self.channels = channels
        self.nickname = nickname.encode('utf-8')

    def buildProtocol(self, addr):
        gateway = Gateway()
        gateway.factory = self
        gateway.nickname = self.nickname

        return gateway

    def clientConnectionLost(self, connector, reason):
        connector.connect()

    def clientConnectionFailed(self, connector, reason):
        print "connection failed:", reason
        reactor.stop()


class IrcBot(object):
    def __init__(self, settings, uri):
        self.server = settings['server']
        self.port = settings['port']
        self.channels = settings['channels']
        self.nickname = settings['nickname']
        self.uri = uri

    def run(self):
        self.f = GatewayFactory(self.channels, self.nickname, self.uri)
        reactor.connectTCP(self.server, self.port, self.f)

        print "Starting reactor"
        reactor.run()
