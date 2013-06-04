This is students' Paweł Michna and Paweł Peregud (group no. 3) SE2 project.


Server is done as an Erlang application. It can handle multiple players and GMs at the same time.


To compile:

`make all`


To run server in championshp do:

`./server.sh --championship "5-in-line-tic-tac-toe" XX --port ZZZZ`

where XX is the number of players who must connect before championship may start and ZZZZ is the port number to listen on.


To run GM do:

`./master.sh --connect_to IP:PORT`

where IP:PORT is the address of the game server.


To run player do:

`./player.sh --connect_to hostname IP:PORT --nick NICK`


Definition of the player - a process that knows:

- server IP,
- server port,
- socket for communication with the server.

Player does:

- parsing incoming XML data,
- reacting for known messages,
- sending correct messages as responses for particular incoming messages,
- connecting to a given server.

Communication with server:

- adding incoming stream to a buffer until a correct XML message is recognized,

- otherwise fire an error and crash.


For further reading refer to apps/play/src/gamer.erl - there are comments describing particular functions.
