import logging
import time

import argparse
import pika


_LOG_LEVELS = {
    'debug': logging.DEBUG,
    'info': logging.INFO,
    'warning': logging.WARNING,
    'error': logging.ERROR,
    'critical': logging.CRITICAL
}


def _default_setup(cli_args, args, kwargs):
    args.append(cli_args.uri)


class KlaczNGModule(object):
    """Helps you spawn a Python klaczng module"""
    def __init__(self, name, target, setup=_default_setup):
        self._name = name
        self._target = target
        self._setup = setup
        self._log = logging.getLogger('klaczng.modules.' + name)
        self.parser = argparse.ArgumentParser()
        self.parser.add_argument('-u', '--uri', default='amqp://guest:guest'
                                 '@127.0.0.1:5672/', help="AMQP host URI")
        self.parser.add_argument('-l', '--log-level', default='WARNING',
                                 help="Log level")

    def run(self, *args, **kwargs):
        cli_args = self.parser.parse_args()
        level = cli_args.log_level.lower()
        if level not in _LOG_LEVELS:
            self._log.critical("Incorrect log level given. Defaulting to"
                               "WARNING")
            level = 'warning'
        level = _LOG_LEVELS[level]
        self._log.addHandler(logging.StreamHandler())
        self._log.setLevel(level)

        args = list(args)
        self._setup(cli_args, args, kwargs)

        i = 1
        while True:
            try:
                self._log.info("Starting module {}...".format(self._name))
                self._target(*args, **kwargs)
            except pika.exceptions.AMQPConnectionError as e:
                msg = ("Failed to start due to AMQP error: {}. Restarting"
                       " in {} seconds...".format(e, i))
                self._log.info(msg)
                time.sleep(i)
                if i < 60:
                    i *= 2
                else:
                    i = 60
