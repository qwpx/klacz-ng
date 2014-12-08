# klacz-ng

## requirements

 + pika (pip install pika)
 + rabbitmq-server

## guidelines

 + always add start script
 + gateway module is a main irc communication module, let's keep it this way
 + Subscribe to an exchange `events` with topic `klacz.privmsg.<command>` if you are going to handle specific commands (i.e. `klacz.privmsg.pick`) or use `klacz.privmsg` topic to receive all irc privmsgs. If you want to publish something to irc, send messages to `klacz.gateway` queue (default `''` exchange).
 + `gateway->module` and `module->gateway` data schema
 
  >  {
  >     'from' : string,
  >     'replyTo' : string,
  >     'content' : string,
  >  }

