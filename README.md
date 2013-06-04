This is students' Paweł Michna and Paweł Peregud (group no. 3) SE2 project.


Server is done as an Erlang application. It can handle multiple players and GMs at the same time.


To compile:

`make all`

To run server in championshp do:

`./bin/server --championship "5-in-line-tic-tac-toe" XX --port ZZZZ`

where XX is the number of players who must connect before championship may start
and ZZZZ is the port number to listen on.


To run GM do:

`./bin/master --connect_to HOST:PORT`

where IP:PORT is the address of the game server. Note, port at which server is
listening for game master is defined in serv.config and is set to 2091 by default.


To run player do:

`./bin/player --connect_to HOST:PORT --nick NICK`

===REPL===
All three applications accept following command:
% to set verbosity of logging into console do
lager:set_loglevel(lager_console_backend, Level). %% where Level is debug, info, notice, error, alert
Server:
% to display debug information about games of championship do
game_host:list().

Note: . (dot) at the end of commands is obligatory!

==Player==

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

==Server==
Server is implemented as Erlang/OTP application. It should be easy to make it
very reliable, however no special effort was undertaken.
* it can host very many games and clients simultaniously
* it is functional, uses actor model
* it is multi-threaded (VM level processes, 300b per process)
* it's processes use preemptive scheduling
* it is soft-realtime
* all errors are detected early - offenders crash, restarted, application continues to work
* it makes heavy use of Ulf Wieger's gproc for process registration.

Modules:
* game_host - creation of games, championship management
* room - manages single game, player move's timeouts
* client - manages TCP connection of a player. This process also does parsing
* gm - like "client", but handles communication with game master
* sxml - helpers for XML parsing and message construction
* serv_acceptor - tcp listener/acceptor, uses Ranch
* *_sup modules - OTP supervisors, for supervision trees

==Game master==
Uses separate module for tic-tac-toe logic, otherwise it is pretty simplistic.
Can host multiple games at the same time.

Modules:
* gm_client - handles communication with server
* ttt - 5-in-line-tick-tack-toe game logic
