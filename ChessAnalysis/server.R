library(shiny)
library(png)
library(tidyverse)
library(httr)
library(jsonlite)
library(DT)

queryChess <- function(category,chessType,username,year,month,chessTitle="All",subcategory=NULL){
  if(category=="leaderboards"){
    URL <- "https://api.chess.com/pub/leaderboards"
    data <- GET(URL)
    parsed <- fromJSON(rawToChar(data$content))
    if(chessType=="All"){
      chessInfo1 <- as_tibble(parsed$live_rapid) |> mutate(type="Rapid")
      chessInfo2 <- as_tibble(parsed$live_blitz)|> mutate(type="Blitz")
      chessInfo3 <- as_tibble(parsed$live_bullet)|> mutate(type="Bullet")
      chessInfo <- bind_rows(chessInfo1,chessInfo2,chessInfo3) |>
        mutate(country=substr(country,nchar(country)-1,nchar(country))) |>
        select(username,score,rank,country,title,win_count,loss_count,draw_count,type)
    if(chessTitle!="All"){
      chessInfo <- chessInfo |> filter(title==chessTitle)
    }
      chessInfo
    }
    else if(chessType=="Rapid"){
      chessInfo <- as_tibble(parsed$live_rapid) |>
        mutate(country=substr(country,nchar(country)-1,nchar(country))) |>
        select(username,score,rank,country,title,win_count,loss_count,draw_count)
      if(chessTitle!="All"){
        chessInfo <- chessInfo |> filter(title==chessTitle)
      }
      chessInfo
    }
    else if(chessType=="Blitz"){
      chessInfo <- as_tibble(parsed$live_blitz) |>
        mutate(country=substr(country,nchar(country)-1,nchar(country))) |>
        select(username,score,rank,country,title,win_count,loss_count,draw_count)
      if(chessTitle!="All"){
        chessInfo <- chessInfo |> filter(title==chessTitle)
      }
      chessInfo
    }
    else if(chessType=="Bullet"){
      chessInfo <- as_tibble(parsed$live_bullet) |>
        mutate(country=substr(country,nchar(country)-1,nchar(country))) |>
        select(username,score,rank,country,title,win_count,loss_count,draw_count)
      if(chessTitle!="All"){
        chessInfo <- chessInfo |> filter(title==chessTitle)
      }
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
    if(chessType=="All"){
      chessInfo1 <- as_tibble_row(unlist(parsed$chess_rapid)) |> mutate(type="Rapid")
      chessInfo2 <- as_tibble_row(unlist(parsed$chess_blitz)) |> mutate(type="Blitz")
      chessInfo3 <- as_tibble_row(unlist(parsed$chess_bullet))|> mutate(type="Bullet") 
      chessInfo <- bind_rows(chessInfo1,chessInfo2,chessInfo3) |>
        mutate(last.rating= as.integer(last.rating),last.date= as.integer(last.date),last.rd= as.integer(last.rd),best.rating= as.integer(best.rating),best.date= as.integer(best.date),record.win= as.integer(record.win),record.loss= as.integer(record.loss),record.draw= as.integer(record.draw)) |>
        select(best.rating,record.win,record.loss,record.draw,type)
      chessInfo
    }
    else if(chessType=="Rapid"){
      chessInfo <- as_tibble_row(unlist(parsed$chess_rapid)) |>
        mutate(type="Rapid",last.rating= as.integer(last.rating),last.date= as.integer(last.date),last.rd= as.integer(last.rd),best.rating= as.integer(best.rating),best.date= as.integer(best.date),record.win= as.integer(record.win),record.loss= as.integer(record.loss),record.draw= as.integer(record.draw)) |>
        select(best.rating,record.win,record.loss,record.draw,type)
      chessInfo  
    }
    else if(chessType=="Blitz"){
      chessInfo <- as_tibble_row(unlist(parsed$chess_blitz)) |>
        mutate(type="Blitz",last.rating= as.integer(last.rating),last.date= as.integer(last.date),last.rd= as.integer(last.rd),best.rating= as.integer(best.rating),best.date= as.integer(best.date),record.win= as.integer(record.win),record.loss= as.integer(record.loss),record.draw= as.integer(record.draw)) |>
        select(best.rating,record.win,record.loss,record.draw,type)
      chessInfo  
    }
    else if(chessType=="Bullet"){
      chessInfo <- as_tibble_row(unlist(parsed$chess_bullet)) |>
        mutate(type="Bullet",last.rating= as.integer(last.rating),last.date= as.integer(last.date),last.rd= as.integer(last.rd),best.rating= as.integer(best.rating),best.date= as.integer(best.date),record.win= as.integer(record.win),record.loss= as.integer(record.loss),record.draw= as.integer(record.draw)) |>
        select(best.rating,record.win,record.loss,record.draw,type)
      chessInfo  
    }
    else {
      "Error, function input misspecified."
    }
  }
  else if(category=="player"&&subcategory=="games"){
    m <- month
    if (nchar(m)==1) {
      m = paste("0",m,sep='')
    }
    URL <- paste("https://api.chess.com/pub/",category,"/",username,"/",subcategory,"/",year,"/",m,sep='')
    data <- GET(URL)
    parsed <- fromJSON(rawToChar(data$content))
    chessInfo <- as_tibble(parsed$games) 
    if (nrow(chessInfo) != 0){
      chessInfo <- chessInfo |>
        mutate(type=time_class,end_time=as.Date(as.POSIXct(end_time, origin="1970-01-01"))) |>
        mutate(white.user=white$username,black.user=black$username,white.rating=white$rating,black.rating=black$rating,white.result=white$result,black.result=black$result)
      if(chessType != "All") {
        chessInfo <- chessInfo |> filter(type==tolower(chessType))
      }
      chessInfo
    }else chessInfo <- tibble()
  }
  else {
    "Error, a function input has been misspecified"
  }
} 

function(input, output, session) {
  output$table1 <- renderTable({
    queryChess(category = "player", chessType =input$gameType2, username = input$user2, subcategory = "stats")
  })
  
  output$table2 <- renderDataTable({
    queryChess(category = "player", chessType = input$gameType3, username = input$user3, year = input$year3, month = input$month3, subcategory = "games") |>
      select(white.user:black.result,end_time,time_class)
  })
  
  output$table3 <- renderDataTable({
    queryChess(category = "leaderboards", chessType = input$gameType1,chessTitle=input$chessTitle1)
  })
  
  output$downloadLead <- downloadHandler(
    filename="Leaderboard.csv",
    content=function(file){
    write.csv(queryChess(category = "leaderboards", chessType = input$gameType1,chessTitle=input$chessTitle1),file,row.names=FALSE)
    })
  
  output$downloadLife <- downloadHandler(
    filename="UserLifetime.csv",
    content=function(file){
      write.csv(queryChess(category = "player", chessType =input$gameType2, username = input$user2, subcategory = "stats"),file,row.names=FALSE)
    })
  
  output$downloadGames <- downloadHandler(
    filename="UserGames.csv",
    content=function(file){
      write.csv(queryChess(category = "player", chessType = input$gameType3, username = input$user3, year = input$year3, month = input$month3, subcategory = "games") |>
                  select(white.user:black.result,end_time,time_class),file,row.names=FALSE)
    })
  
  g  <- reactive({g <- ggplot(queryChess(category = "player", chessType =input$gameType2, username = input$user2, subcategory = "stats") |> 
    rename(Win=record.win,Loss=record.loss,Draw=record.draw) |>
    pivot_longer(cols=c(Win,Loss,Draw),names_to = "Record") |>
    mutate(Record = as_factor(Record)),aes(x=Record,y=value))})
  
  output$table6 <- renderTable({queryChess(category = "player", chessType ="All", username = input$user2, subcategory = "stats") |> 
    rename(Win=record.win,Loss=record.loss,Draw=record.draw) |>
    select(type,Win,Loss,Draw)
  })
  
  output$plot1 <- renderPlot(
    g()+geom_col(aes(fill=Record))+labs(x="Record",y="Count",title="Lifetime Statistics")+theme(legend.position = "none")+theme(plot.title = element_text(hjust=0.5))
  )
  
  h <- reactive({h <- ggplot(queryChess(category = "player", chessType =input$gameType3, username = input$user3, year =input$year3, month = input$month3, subcategory = "games") |>
    mutate(userSide = ifelse(white.user==input$user3,"White","Black")) |>
    mutate(userELO = ifelse(userSide=="White",white.rating,black.rating)) |>
    select(userSide,userELO) |>
    group_by(userSide),aes(x=userSide,y=userELO))})
  
  output$plot2 <- renderPlot(
    h() + geom_boxplot(aes(fill=userSide)) + labs(x="Side Played",y="ELO",title="ELO Ranking by Side Played")+theme(legend.position = "none",plot.title = element_text(hjust=0.5))
  )

  i <- reactive({i <- ggplot(queryChess(category = "leaderboards", chessType = input$gameType1,chessTitle=input$chessTitle1)|>
    select(title,score) |>
    filter(!is.na(title)) |>
    mutate(title = factor(title,levels=c("CM","FM","IM","GM"))) |>
    arrange(title),aes(x=score))})
  
  output$plot3 <- renderPlot(
    i() + geom_histogram(aes(fill=title),bins = input$bins) + labs(x="ELO",y="Count",title="Leaderboard Ranking Distribution")+scale_fill_discrete(name = "Chess Title")+theme(plot.title = element_text(hjust=0.5))+scale_fill_manual(values=c("GM"="blue","IM"="purple","FM"="red","CM"="pink"))
  )
   
  j <- reactive({i <- ggplot(queryChess(category = "player", chessType =input$gameType3, username = input$user3, year =input$year3, month = input$month3, subcategory = "games")|>
      mutate(userSide = ifelse(white.user==input$user3,"White","Black")) |>
      mutate(userELO = ifelse(userSide=="White",white.rating,black.rating), oppELO = ifelse(userSide!="White",white.rating,black.rating)) |>
      select(userELO,oppELO,end_time) |>
      group_by(end_time) |>
      summarize(meanuserELO = mean(userELO),meanoppELO=mean(oppELO)),aes(x=end_time))}) 
  
  output$plot4 <- renderPlot(
    j()+geom_line(aes(y=meanuserELO,color="User")) + geom_line(aes(y=meanoppELO,color="Opponent"))+labs(x="Date",y="ELO",colour="Player",title="Comparison of ELO Fluctuation of Player and Opponents")+theme(plot.title = element_text(hjust=0.5))
  )
  
  output$table5 <- renderTable(
    if(input$freqVar1=="title"){
      queryChess(category = "leaderboards", chessType ="All",chessTitle="All")|>
      select(type,title) |>
      filter(!is.na(title)) |>
      group_by(type,title) |>
      summarize(count=n()) |>
      pivot_wider(names_from=title,values_from = count) |>
      mutate(across(everything(),~replace_na(.,0)))
    }
    else if (input$freqVar1=="country"){
      queryChess(category = "leaderboards", chessType ="All",chessTitle="All")|>
        select(type,country) |>
        group_by(type,country) |>
        filter(!is.na(country)) |>
        summarize(count=n()) |>
        pivot_wider(names_from=country,values_from = count) |>
        mutate(across(everything(),~replace_na(.,0)))
    }
  )
  
  output$table7 <- renderTable({
    if(input$freqVar2=="side"){
      queryChess(category = "player", chessType ="All", username = input$user3, year =input$year3, month = input$month3, subcategory = "games") |>
        mutate(userSide = ifelse(white.user==input$user3,"White","Black")) |>
        mutate(userResult = ifelse(userSide=="White",white.result,black.result)) |>
        mutate(userResult = ifelse(userResult=="win","Win",
                                   ifelse(userResult=="agreed","Draw",
                                          ifelse(userResult=="stalemate","Draw",
                                                 ifelse(userResult=="insufficient","Draw",
                                                        ifelse(userResult=="50move","Draw",
                                                               "Loss")))))) |>
        group_by(type,userSide) |>
        select(type,userSide) |>
        summarize(count=n()) |>
        pivot_wider(names_from = userSide,values_from = count) |>
        select(type,White,Black) |>
        arrange(desc(type))|>
        mutate(across(everything(),~replace_na(.,0)))
    }
    else if (input$freqVar2=="outcome"){
      queryChess(category = "player", chessType ="All", username = input$user3, year =input$year3, month = input$month3, subcategory = "games") |>
        mutate(userSide = ifelse(white.user==input$user3,"White","Black")) |>
        mutate(userResult = ifelse(userSide=="White",white.result,black.result)) |>
        mutate(userResult = ifelse(userResult=="win","Win",
                                   ifelse(userResult=="agreed","Draw",
                                          ifelse(userResult=="stalemate","Draw",
                                                 ifelse(userResult=="insufficient","Draw",
                                                        ifelse(userResult=="50move","Draw",
                                                               "Loss")))))) |>
        group_by(type,userResult) |>
        select(userSide,userResult) |>
        summarize(count=n()) |>
        pivot_wider(names_from = userResult,values_from = count) |>
        select(type,Win,Loss,Draw) |>
        arrange(desc(type)) |>
        mutate(across(everything(),~replace_na(.,0)))
    }
  })
  
  output$table8 <- renderTable({
    if(input$statVar1=="User"){
      queryChess(category = "player", chessType ="All", username = input$user3, year =input$year3, month = input$month3, subcategory = "games") |>
      mutate(Side = ifelse(white.user==input$user3,"White","Black")) |>
      mutate(userELO = ifelse(Side=="White",white.rating,black.rating)) |>
      select(Side,userELO) |>
      group_by(Side) |>
      summarize(meanELO = mean(userELO),sdELO = sd(userELO),medianELO = median(userELO),iqrELO=IQR(userELO)) |>
      arrange(desc(Side)) |>
      mutate(Side = factor(Side))
    }
    else if (input$statVar1=="Opponent"){
      queryChess(category = "player", chessType ="All", username = input$user3, year =input$year3, month = input$month3, subcategory = "games") |>
        mutate(Side = ifelse(white.user!=input$user3,"White","Black")) |>
        mutate(userELO = ifelse(Side=="White",white.rating,black.rating)) |>
        select(Side,userELO) |>
        group_by(Side) |>
        summarize(meanELO = mean(userELO),sdELO = sd(userELO),medianELO = median(userELO),iqrELO=IQR(userELO)) |>
        arrange(desc(Side)) |>
        mutate(Side = factor(Side))
    }
  })
 
 k <- reactive({
    k <- ggplot(queryChess(category = "leaderboards", chessType ="All",chessTitle="All") |>
                  select(score,rank,type,country,title) |>
                  filter(!is.na(title)),aes(x=rank,y=score,color=get(input$facetVal1)))
  })
  output$plot5 <- renderPlot({
    k()+geom_jitter()+facet_wrap(~get(input$facetVal1))+ labs(x="Leaderboard Rankings",y="ELO Ranking",color=input$facetVal1)+theme(plot.title = element_text(hjust=0.5))
  })
} 
  

  
  
  