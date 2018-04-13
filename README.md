# ch/otp
As the problem requires that each node send messages to each other node,
and I don't know up front what failure maps I should expect,
program implements peer to peer communication.

To further limit impact of any loosing any node,
program implements sort of a blockchain.
With each message node sends out result of its message chain.
Other nodes will append this message only if their results
matches that received in the message.

In case of a mismatch node will try to update it's chain from peer,
before attempting any other actions,
but only if result of a peer chain is higher.

## Running program
Executable is called `node`.
Beside required set of parameters it can additionally take these parameters.
```shell
--host HOST              Bind to HOST
-p,--port P              Port number P
-t,--throttle            Limit message sending to 1 per second
```
Program takes at least one peer address as an argument.
For example:
```shell
stack exec node -- \
  --send-for 60 \
  --wait-for 10 \
  --host 127.0.0.1 \
  --port 9501 \
  127.0.0.1:9500 127.0.0.1:9502
```

## Todo
Right now there is no `reconnect`ion mechanism.
However as this program does not rely on CH message ordering,
it could use unsafe send to pass messages.
This would require to use custom backend instead of distributed-process-p2p.

## Remarks
As I was concerned with keeping consensus between nodes at all the time,
the program is not fastest and spend most time synchronizing between nodes.
