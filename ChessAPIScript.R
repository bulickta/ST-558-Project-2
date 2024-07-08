###Script File to test and create the API functions###
library(tidyverse)
library(httr)
library(jsonlite)

queryChess <- function(category,chessType,username,year,month,subcategory=NULL){
  if(category=="leaderboards"){
    URL <- "https://api.chess.com/pub/leaderboards"
    data <- GET(URL)
    parsed <- fromJSON(rawToChar(data$content))
    if(chessType=="Rapid"){
      chessInfo <- as_tibble(parsed$live_rapid)
      chessInfo
    }
    else if(chessType=="Blitz"){
      chessInfo <- as_tibble(parsed$live_blitz)
      chessInfo
    }
    else if(chessType=="Bullet"){
      chessInfo <- as_tibble(parsed$live_bullet)
      chessInfo
    }
    else {
      "Error, function input misspecified."
    }
  }
  else if(category=="player"&&subcategory=="stats"){
    URL <- paste("https://api.chess.com/pub/",category,"/",username,"/",subcategory,sep='')
    data <- GET(URL)
    parsed <- fromJSON(rawToChar(data$content))
    if(chessType=="Rapid"){
      chessInfo <- as_tibble_row(unlist(parsed$chess_rapid)) |>
        mutate(last.rating= as.integer(last.rating),last.date= as.integer(last.date),last.rd= as.integer(last.rd),best.rating= as.integer(best.rating),best.date= as.integer(best.date),record.win= as.integer(record.win),record.loss= as.integer(record.loss),record.draw= as.integer(record.draw))
      chessInfo  
    }
    else if(chessType=="Blitz"){
      chessInfo <- as_tibble_row(unlist(parsed$chess_blitz)) |>
        mutate(last.rating= as.integer(last.rating),last.date= as.integer(last.date),last.rd= as.integer(last.rd),best.rating= as.integer(best.rating),best.date= as.integer(best.date),record.win= as.integer(record.win),record.loss= as.integer(record.loss),record.draw= as.integer(record.draw))
      chessInfo  
    }
    else if(chessType=="Bullet"){
      chessInfo <- as_tibble_row(unlist(parsed$chess_bullet)) |>
        mutate(last.rating= as.integer(last.rating),last.date= as.integer(last.date),last.rd= as.integer(last.rd),best.rating= as.integer(best.rating),best.date= as.integer(best.date),record.win= as.integer(record.win),record.loss= as.integer(record.loss),record.draw= as.integer(record.draw))
      chessInfo  
    }
    else {
      "Error, function input misspecified."
    }
  }
  else if(category=="player"&&subcategory=="games"){
    URL <- paste("https://api.chess.com/pub/",category,"/",username,"/",subcategory,"/",year,"/",month,sep='')
    data <- GET(URL)
    parsed <- fromJSON(rawToChar(data$content))
    chessInfo <- as_tibble(parsed$games) |>
      mutate(white.user=white$username,black.user=black$username,white.rating=white$rating,black.rating=black$rating,white.result=white$result,black.result=black$result) |>
      select(url:rules,white.user:black.result)
    chessInfo
  }
  else {
    "Error, a function input has been misspecified"
  }
}

test1 <- queryChess(category="leaderboards",chessType="Rapid")
test2 <- queryChess(category = "player", subcategory = "stats", username = "tolkienatic", chessType = "Blitz")
test3 <- queryChess(category = "player", subcategory = "games", username = "tolkienatic", year="2024", month="01")
test1
test2
test3
