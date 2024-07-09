###Script File to test and create the API functions###
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
      chessInfo <- as_tibble(parsed$live_rapid) |>
        mutate(country=substr(country,nchar(country)-1,nchar(country))) |>
        select(username,score,rank,country,title,win_count,loss_count,draw_count)
      chessInfo
    }
    else if(chessType=="Blitz"){
      chessInfo <- as_tibble(parsed$live_blitz) |>
        mutate(country=substr(country,nchar(country)-1,nchar(country))) |>
        select(username,score,rank,country,title,win_count,loss_count,draw_count)
      chessInfo
    }
    else if(chessType=="Bullet"){
      chessInfo <- as_tibble(parsed$live_bullet) |>
        mutate(country=substr(country,nchar(country)-1,nchar(country))) |>
        select(username,score,rank,country,title,win_count,loss_count,draw_count)
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
        mutate(last.rating= as.integer(last.rating),last.date= as.integer(last.date),last.rd= as.integer(last.rd),best.rating= as.integer(best.rating),best.date= as.integer(best.date),record.win= as.integer(record.win),record.loss= as.integer(record.loss),record.draw= as.integer(record.draw)) |>
        select(best.rating,record.win,record.loss,record.draw)
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
        mutate(white.user=white$username,black.user=black$username,white.rating=white$rating,black.rating=black$rating,white.result=white$result,black.result=black$result)
      chessInfo
    }else chessInfo <- tibble()
  }
  else {
    "Error, a function input has been misspecified"
  }
}

test3 <- queryChess(category = "player", subcategory = "games", username = "tolkienatic", year="2023", chessType="Rapid",month="01")


###Freq table Games as win/loss/tie
tableOut1 <- test3 |>
  mutate(userSide = ifelse(white.user=="tolkienatic","White","Black")) |>
  mutate(userResult = ifelse(userSide=="White",white.result,black.result)) |>
  mutate(userResult = ifelse(userResult=="win","Win",
                             ifelse(userResult=="agreed","Draw",
                                    ifelse(userResult=="stalemate","Draw",
                                           ifelse(userResult=="insufficient","Draw",
                                              ifelse(userResult=="50move","Draw",
                                                  "Loss")))))) |>
  group_by(userSide,userResult) |>
  select(userSide,userResult) |>
  summarize(count=n()) |>
  pivot_wider(names_from = userResult,values_from = count) |>
  select(userSide,Win,Loss,Draw) |>
  arrange(desc(userSide))

###Freq table leaderboard by country and title
tableOut2 <- test1 |>
  select(title,country) |>
  drop_na() |>
  mutate(country=substr(country,nchar(country)-1,nchar(country))) |>
  group_by(title,country) |>
  summarize(count=n()) |>
  pivot_wider(names_from = country,values_from = count) |>
  mutate(across(everything(),~replace_na(.,0))) |>
  mutate(title = factor(title,levels=c("GM","IM","FM","CM"))) |>
  arrange(title)

###Summary of user ELO across side
tableOut3 <- test3 |>
  mutate(userSide = ifelse(white.user=="tolkienatic","White","Black")) |>
  mutate(userELO = ifelse(userSide=="White",white.rating,black.rating)) |>
  select(userSide,userELO) |>
  group_by(userSide) |>
  summarize(meanELO = mean(userELO),sdELO = sd(userELO),medianELO = median(userELO),iqrELO=IQR(userELO)) |>
  arrange(desc(userSide)) |>
  mutate(userSide = factor(userSide))

###Summary of opponent's ELO across side
tableOut4 <- test3 |>
  mutate(userSide = ifelse(white.user!="tolkienatic","White","Black")) |>
  mutate(userELO = ifelse(userSide=="White",white.rating,black.rating)) |>
  select(userSide,userELO) |>
  group_by(userSide) |>
  summarize(meanELO = mean(userELO),sdELO = sd(userELO),medianELO = median(userELO),iqrELO=IQR(userELO)) |>
  arrange(desc(userSide)) |>
  mutate(userSide = factor(userSide))

###Summary of leaderboard ELO across types
arrange(chessType)

visout4 <- test3 |>
  arrange(desc(white.rating)) |>
  slice(1) |>
  select(pgn)
file.create("test.pgn")
write(visout4$pgn,"test.pgn")
game <- read_game("test.pgn")
for(i in 1:1000){
  if (is.null(game |> forward(i))){
    print(game|>forward(i-1),unicode = TRUE)
    break
  }
}

visout5 <- test3 |>
  mutate(userSide = ifelse(white.user!="tolkienatic","White","Black")) |>
  mutate(userELO = ifelse(userSide=="White",white.rating,black.rating), oppELO = ifelse(userSide!="White",white.rating,black.rating),end_time=as.Date(as.POSIXct(end_time, origin="1970-01-01"))) |>
  select(userELO,oppELO,end_time) |>
  group_by(end_time) |>
  summarize(meanuserELO = mean(userELO),meanoppELO=mean(oppELO))

j <- ggplot(visout5,aes(x=end_time)) 
j+geom_line(aes(y=meanuserELO,color="User")) + geom_line(aes(y=meanoppELO,color="Opponent"))+labs(x="Date",y="ELO",colour="Player")
