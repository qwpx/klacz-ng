from twisted.words.protocols import irc
from threading import Thread
from twisted.internet import reactor, protocol
import time, sys, json, rmq

class Rabbit(rmq.Rmq):
    def callback(self, ch, method, props, body):
        data = json.loads(body)
        to = data['replyTo']
        content = data['content']

        self.ircbot.msg(to.encode('utf-8'), content.encode('utf-8'))

class Gateway(irc.IRCClient):
    def connectionMade(self):
        irc.IRCClient.connectionMade(self)

    def connectionLost(self, reason):
        irc.IRCClient.connectionLost(self, reason)

    def signedOn(self):
        # TODO: send event to an exchange
        for channel in self.factory.channels:
            self.join(channel.encode('ascii'))

    def joined(self, channel):
        # TODO: send event to an exchange
        pass

    def privmsg(self, user, channel, body):
        user = user.split('!', 1)[0]

        msg = {
            'replyTo' : user if self.nickname == channel else channel,
            'from' : user
            }

        if body.startswith(','):
            words = body.split(' ')
            command = words[0][1:]
            msg["content"] = " ".join(words[1:])

            self.factory.events_queue.publish('klacz.privmsg.' + command, json.dumps(msg), 'events')
        else:
            msg["content"] = body
            self.factory.events_queue.publish('klacz.privmsg', json.dumps(msg), 'events')

    def action(self, user, channel, msg):
        # TODO: send event to an exchange
        pass

    def irc_NICK(self, prefix, params):
        # TODO: send event to an exchange
        pass

class GatewayFactory(protocol.ClientFactory):
    def __init__(self, channels, nickname):
        self.channels     = channels
        self.nickname     = nickname.encode('utf-8')
        self.reciever     = Rabbit()
        self.events_queue = rmq.Rmq()

    def buildProtocol(self, addr):
        # TODO: wtf I don't even
        p = Gateway()
        p.factory = self
        p.nickname = self.nickname

        self.events_queue.exchange_declare('topic', 'events')
        self.reciever.queue_declare('klacz.gateway')
        self.reciever.ircbot = p
        self.thr = Thread(target=lambda: self.reciever.start_recieving('klacz.gateway'))
        self.thr.start()

        return p

    def clientConnectionLost(self, connector, reason):
        connector.connect()

    def clientConnectionFailed(self, connector, reason):
        print "connection failed:", reason
        reactor.stop()

class IrcBot:
    def __init__(self, settings):
        self.server   = settings["server"]
        self.port     = settings["port"]
        self.channels = settings["channels"]
        self.nickname = settings["nickname"]

    def run(self):
        self.f = GatewayFactory(self.channels, self.nickname)
        reactor.connectTCP(self.server, self.port, self.f)

        print "Starting reactor"
        reactor.run()

if __name__ == '__main__':
    data = open("settings.json").read()
    IrcBot(json.loads(data)).run()
