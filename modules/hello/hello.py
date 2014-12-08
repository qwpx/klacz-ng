import json
import uuid

import rmq


class Hello(rmq.Rmq):
    def callback(self, ch, method, props, body):
        body = json.loads(body)
        body['content'] = 'Hello!'
        self.publish('klacz.gateway', body=json.dumps(body))

if __name__ == '__main__':
    t = Hello()
    queue = str(uuid.uuid4())
    t.queue_bind(queue, 'events', 'klacz.privmsg.hello')
    t.start_recieving(queue)
