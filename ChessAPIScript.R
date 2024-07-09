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
      mutate(white.user=white$username,black.user=black$username,white.rating=white$rating,black.rating=black$rating,white.result=white$result,black.result=black$result) |>
      select(url:rules,white.user:black.result)
    chessInfo
    }else chessInfo <- tibble()
  }
  else {
    "Error, a function input has been misspecified"
  }
}

test1 <- queryChess(category="leaderboards",chessType="Rapid")
test2 <- queryChess(category = "player", subcategory = "stats", username = "tolkienatic", chessType = "Blitz")
test3 <- queryChess(category = "player", subcategory = "games", username = "tolkienatic", year="2024", month="01")
test4 <- queryChess(category="leaderboards",chessType="Blitz")
test5 <- queryChess(category="leaderboards",chessType="Bullet")
test1
test2 
test3
test4
test5

###Lifetime user record visual
visout1 <- test2 |> 
  rename(Win=record.win,Loss=record.loss,Draw=record.draw) |>
  pivot_longer(cols=c(Win,Loss,Draw),names_to = "Record") |>
  mutate(Record = as_factor(Record))
g <- ggplot(visout1,aes(x=Record,y=value))
  g+geom_col(aes(fill=Record))+labs(x="Record",y="Count",title="Lifetime Statistics")+theme(legend.position = "none")+theme(plot.title = element_text(hjust=0.5))

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
test1 <- test1 |>
  mutate(chessType = "Rapid")
test4 <- test4 |>
  mutate(chessType = "Blitz")
test5 <- test5 |>
  mutate(chessType = "Bullet")

allLeaders <- bind_rows(test1,test4,test5) |>
  select(chessType,score) |>
  group_by(chessType) |>
  summarize(meanELO = mean(score),sdELO = sd(score),medianELO = median(score),iqrELO=IQR(score)) |>
  mutate(chessType = factor(chessType,levels=c("Rapid","Blitz","Bullet"))) |>
  arrange(chessType)

###Visual of ELO variation in a month
visout2 <- test3 |>
  mutate(userSide = ifelse(white.user!="tolkienatic","White","Black")) |>
  mutate(userELO = ifelse(userSide=="White",white.rating,black.rating)) |>
  select(userSide,userELO) |>
  group_by(userSide)

h <- ggplot(visout2,aes(x=userSide,y=userELO))
h + geom_boxplot(aes(fill=userSide)) + labs(x="Side Played",y="ELO",title="Ranking by Side Played")+theme(legend.position = "none")+theme(plot.title = element_text(hjust=0.5))

###Distribution of leaderboard ELO
visout3 <- test1 |>
  select(title,score) |>
  filter(!is.na(title)) |>
  mutate(title = factor(title,levels=c("IM","FM","CM","GM"))) |>
  arrange(title)

i <- ggplot(visout3,aes(x=score))
i + geom_histogram(aes(fill=title),binwidth = 50) + labs(x="ELO",y="Count",title="Leaderboard Ranking Distribution")+scale_fill_discrete(name = "Chess Title")+theme(plot.title = element_text(hjust=0.5))

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
