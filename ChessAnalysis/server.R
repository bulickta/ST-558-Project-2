#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(png)
library(tidyverse)
library(httr)
library(jsonlite)
library(chess)

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
    chessInfo <- as_tibble(parsed$games) 
    if (nrow(chessInfo) != 0){
      chessInfo <- chessInfo |>
        filter(time_class==tolower(chessType)) |>
        mutate(white.user=white$username,black.user=black$username,white.rating=white$rating,black.rating=black$rating,white.result=white$result,black.result=black$result) |>
        select(url:rules,white.user:black.result)
      chessInfo
    }else chessInfo <- tibble()
  }
  else {
    "Error, a function input has been misspecified"
  }
}

# Define server logic required to draw a histogram
function(input, output, session) {
    
    output$image1 <- renderImage({
      return(list(
        src="Logo.png",
        filetype = "image/png",
        alt = "Chess.com Logo"
      ))
    },deleteFile=FALSE)
}
