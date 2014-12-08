import json

from klaczng import rmq


class Hello(rmq.Rmq):
    def callback(self, ch, method, props, body):
        body = json.loads(body)
        body['content'] = 'Hello!'
        self.publish('klacz.gateway', body=json.dumps(body))
