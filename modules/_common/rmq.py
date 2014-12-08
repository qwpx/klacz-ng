import pika, threading

class Rmq:
    def __init__(self):
        self.connection  = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
        self.ex_channel  = None
        self.pub_channel = None
        self.que_channel = None

    def exchange_declare(self, e_type, e_name):
        if self.ex_channel == None:
            self.ex_channel = self.connection.channel()

        self.ex_channel.exchange_declare(exchange=e_name, type=e_type)

    def queue_declare(self, name=None, exclusive=False):
        if self.que_channel == None:
            self.que_channel = self.connection.channel()

        self.que_channel.queue_declare(queue=name, exclusive=exclusive)

    def queue_bind(self, queue, exchange, routing_key):
        self.queue_declare(queue)
        self.que_channel.queue_bind(exchange=exchange, queue=queue, routing_key=routing_key)

    def publish(self, routing_key, body, exchange='', properties=None):
        if self.pub_channel == None:
            self.pub_channel = self.connection.channel()

        self.pub_channel.basic_publish(exchange=exchange, routing_key=routing_key, body=body, properties=properties)

    def callback(self, ch, method, props, body):
        pass

    def run_reciever(self, queue, no_ack):
        self.que_channel.basic_consume(self.callback, queue=queue, no_ack=no_ack)
        self.que_channel.start_consuming()

    def start_recieving(self, queue, no_ack=True):
        self.queue_declare(queue)
        self.run_reciever(queue, no_ack)
