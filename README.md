This is student's SE2 project.

Participants:
host
gm
player1
player2
..
playerN

Definition of the player:
socket

definition of the host:
some process that knows:
* game name
* basic rules (no of required players)
* list of players
* the gm
* state of the room

Host does:
parsing of all xml
execution of commands that are known to him
broadcasts of player's moves?
relays unknown commands to GM

Handling communication with player - The Idea:
socket foldls over stream of events
wait for recognition
otherwise fire command when it's spotted
otherwise fire up error handling
