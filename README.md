Purpose:
  The goal of this app is to enable a user to explore some of the Chess data provided by the Chess.com API. The API contains both leaderboard data, listing top rated players in various categories, and player data, to look up stats and games from specific accounts. The current default account for the player-level data is my personal chess.com account, tolkienatic, but some famous accounts of note that could be fun to explore are: Hikaru Nakamura (username: Hikaru), Magnus Carlsen (username: MagnusCarlsen), and Arjun Erigaisi (username: GHANDEEVAM2003).


Needed Packages:
  The packages needed for this app to run are "shiny" "png" "tidyverse" "httr" "jsonlite" and "DT". There are also two additional packages called "chess" and "rchess" that I considered including for chessboard visualization, but ultimatley decided against as chess only printed correctly in unicode and I was umimpressed, and rchess has unforunately not been updated for the current version of R. If/when rchess is updated there is a ggplot-type object "ggchessboard" that would be perfect for visualizing further.


Package Install Code Snippet:
install.packages(c("shiny","png","tidyverse","httr","jsonlite","DT"))


GitHub run command:
shiny::runGitHub(repo = "ST-558-Project-2",username = "bulickta")
