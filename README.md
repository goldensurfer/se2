This is student's SE2 project.

Server is done as an Erlang application. It can handle multiple players and
gms at the same time.

To compile:
`make all`

To run server do:
`make serv1`

To run GM do:
`./gm.sh "hostname" PORT GmID`

To run player do:
`./player.sh "hostname" PORT "user_nick"

Definition of the player:
A process that knows:
-server IP
-server port
-socket for communication with the server

Player does:
-parsing incoming XML data
-reacting for known messages
-sending correct messages as responses for particular incoming messages
-connecting to a given server

Communication with server:
-adding incoming stream to a buffer until a correct XML message is recognized
-otherwise fire an error and crash
