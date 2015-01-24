from zope.interface import implements

from twisted.python import usage, log
from twisted.plugin import IPlugin
from twisted.application.service import IServiceMaker
from twisted.application import internet, service
import json

from gateway import IrcBot

class Options(usage.Options):
    optParameters = [
        ["config", "c", 'config/gateway.json',
        "Location of the configuration file"],
    ]


class KlaczServiceMaker(object):
    implements(IServiceMaker, IPlugin)
    tapname = "gateway"
    description = "Klacz-ng IRC gateway"
    options = Options

    def makeService(self, options):
        with open(options['config'], 'r') as f:
            cfg = json.loads(f.read())

        mult = service.MultiService()
        svc = IrcBot(cfg)
        svc.setServiceParent(mult)
        internet.TCPClient(cfg['server'], cfg['port'],
                svc.makeIRC()).setServiceParent(mult)
        return mult

# Now construct an object which *provides* the relevant interfaces
# The name of this variable is irrelevant, as long as there is *some*
# name bound to a provider of IPlugin and IServiceMaker.
serviceMaker = KlaczServiceMaker()
