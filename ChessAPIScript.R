###Script File to test and create the API functions###
library(tidyverse)
library(httr)
library(jsonlite)
URL <- "https://api.chess.com/pub/leaderboards"
data <- GET(URL)
parsed <- fromJSON(rawToChar(data$content))
chessInfo <- as_tibble(parsed$live_rapid)


URL <- "https://api.chess.com/pub/player/tolkienatic/stats"
data <- GET(URL)
parsed <- fromJSON(rawToChar(data$content))
chessInfo <- as_tibble(parsed$chess_rapid)

queryChess <- function(category,subcategory){
  URL <- paste("https://api.chess.com/pub/",category,"",subcategory,"",sep='')
  data <- GET(URL)
  parsed <- fromJSON(rawToChar(news$content))
  chessInfo <- as_tibble(parsed$chess_blitz)
  chessInfo
}
