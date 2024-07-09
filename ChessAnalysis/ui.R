library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ST558 Project 2 - Thomas Bulick"),
  sidebarLayout(    
    sidebarPanel(
            radioButtons("gameType","Type of Chess",c("Rapid","Blitz","Bullet")),
            textInput("user","Username"),
            sliderInput("month","Month",min=1,max=12,value=1,step=1),
            sliderInput("year","Year",min=2012,max=2024,value=2012,step=1,sep=''),
            submitButton("Submit"),
      ),
    mainPanel(
      tabsetPanel(type="tabs",
                    #needs purpose, data and source with link, purpose of each tab, picture of the logo
                  tabPanel("About",
                           HTML('<br><center><img src="Logo.png" width="50%" height="50%"></center><br>'),
                           column(4,"The purpose of this app is to explore chess data, either looking at the top leaderboards or a specific individual's account."),
                           column(4,'This data specifically comes from the chess.com API, which for the purpose of this assignment provides access to data on the individual level, such as a selection of games or lifetime statistics, and then on the leaderboard level where the top 50 individuls in a particular category are recorded. Additional information can be found at the link below.'),
                           column(4,"The data download tab will allow you to adjust what data is being used, such as the type of chess or the month to draw data from. The data exploration has a variety of adjustable graphs and tables to visualize the data requested."),
                           HTML('<center><a href="https://www.chess.com/news/view/published-data-api">https://www.chess.com/news/view/published-data-api</a></center>')
                           ),
                  tabPanel("Data Download",
                           HTML('<br>'),
                           "Use the setting in the sidebar to adjust the data.",
                           HTML('<br>'),
                           "Leaderboard-level data: adjusted by Type of Chess only",
                           HTML('<br>'),
                           "User-level data: adjusted by Type of Chess, Month, and Year",
                           ),
                  tabPanel("Data Exploration"),
      ))
            
))
