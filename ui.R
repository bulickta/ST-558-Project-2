###Load Shiny Library###
library(shiny)

###Define UI Elements###
ui <- fluidPage(
  titlePanel("ST558 Project 2 - Thomas Bulick"),
  mainPanel(tabsetPanel(
    type = "tabs",
    tabPanel(
      "About",
      HTML(
        '<br><center><img src="Logo.png" width="50%" height="50%"></center><br>'
      ),
      fluidRow(
        column(
          4,
          "The purpose of this app is to explore chess data provided by chess.com, generally either looking at the top ranked leaderboards or a specific individual's account. This data is grouped into categories of chess, and for this assignment I selected three for simplicity. Rapid: chess with a 10-30 minute time limit. Blitz: chess with a 3-5 minute time limit. Bullet: chess with a 1-3 minute time limit."
        ),
        column(
          4,
          "This data comes via the chess.com API, and accesses data on three levels. 1 - Leaderboard level: data providing information on the top 50 individuals in a category. 2 - User Lifetime level: data providing a user's lifetime record on the site. 3 - User Game level: data on an individual's historical games played within a particular year and month. Additional information can be found at the link below."
        ),
        column(
          4,
          "This app contains two additional tabs: the data download tab will allow you to adjust and view the data being input from the API, such as the type of chess or the month to draw data from, and the data exploration tab has a variety of adjustable graphs and tables to visualize the requested data."
        ),
        HTML('<br>')
      ),
      fluidRow(
        HTML(
          '<center><a href="https://www.chess.com/news/view/published-data-api">https://www.chess.com/news/view/published-data-api</a></center>'
        )
      )
    ),
    tabPanel(
      "Data Download",
      HTML('<br>'),
      selectInput(
        "dataLevel",
        "Data Level",
        c("Leaderboard", "User Lifetime", "User Games"),
        selected = "Leaderboard"
      ),
      conditionalPanel(
        condition = "input.dataLevel == 'Leaderboard'",
        column(
          5,
          HTML('<br>'),
          HTML('<br>'),
          HTML('<br>'),
          radioButtons(
            "gameType1",
            "Type of Chess",
            c("All", "Rapid", "Blitz", "Bullet"),
            selected = "All"
          ),
          radioButtons(
            "chessTitle1",
            "Chess Title",
            c("All", "GM", "IM", "FM", "CM"),
            selected = "All"
          ),
          HTML('<br>'),
          HTML('<br>'),
          downloadButton("downloadLead", "Download Leaderboard Data")
        ),
        column(
          7,
          HTML('<br>'),
          HTML('<br>'),
          HTML('<br>'),
          dataTableOutput("table3")
        ),
        
      ),
      conditionalPanel(
        condition = "input.dataLevel == 'User Lifetime'",
        column(
          5,
          HTML('<br>'),
          HTML('<br>'),
          HTML('<br>'),
          textInput("user2", "Username", value = "tolkienatic"),
          radioButtons(
            "gameType2",
            "Type of Chess",
            c("All", "Rapid", "Blitz", "Bullet"),
            selected = "All"
          ),
          HTML('<br>'),
          HTML('<br>'),
          downloadButton("downloadLife", "Download User's Lifetime Record")
        ),
        column(
          7,
          HTML('<br>'),
          HTML('<br>'),
          HTML('<br>'),
          tableOutput("table1")
        ),
      ),
      conditionalPanel(
        condition = "input.dataLevel == 'User Games'",
        column(
          5,
          HTML('<br>'),
          HTML('<br>'),
          HTML('<br>'),
          textInput("user3", "Username", value = "tolkienatic"),
          radioButtons(
            "gameType3",
            "Type of Chess",
            c("All", "Rapid", "Blitz", "Bullet"),
            selected = "All"
          ),
          sliderInput(
            "month3",
            "Month",
            min = 1,
            max = 12,
            value = 1,
            step = 1
          ),
          sliderInput(
            "year3",
            "Year",
            min = 2012,
            max = 2024,
            value = 2024,
            step = 1,
            sep = ''
          ),
          HTML('<br>'),
          HTML('<br>'),
          downloadButton("downloadGames", "Download User's Games Data"),
        ),
        column(
          7,
          HTML('<br>'),
          HTML('<br>'),
          HTML('<br>'),
          dataTableOutput("table2"),
          HTML('<br>'),
          plotOutput("plot4")
        )
      )
    ),
    tabPanel(
      "Data Exploration",
      HTML('<br>'),
      selectInput(
        "dataLevel",
        "Data to Visualize",
        c("Leaderboard", "User Lifetime", "User Games"),
        selected = "Leaderboard"
      ),
      HTML('<br>'),
      HTML('<br>'),
      conditionalPanel(
        condition = "input.dataLevel == 'Leaderboard'",
        fluidRow(column(
          5,
          selectInput(
            "freqVar1",
            "Frequency Variable Compared to Chess Type",
            c("Chess Title" = "title", "Country" = "country"),
            selected = "Chess"
          )
        ), column(7, tableOutput("table5"))),
        HTML('<br>'),
        HTML('<br>'),
        fluidRow(
          column(
            5,
            radioButtons(
              "gameType1",
              "Type of Chess",
              c("All", "Rapid", "Blitz", "Bullet"),
              selected = "All"
            ),
            radioButtons(
              "chessTitle1",
              "Chess Title",
              c("All", "GM", "IM", "FM", "CM"),
              selected = "All"
            ),
            sliderInput(
              "bins",
              "Number of Bins",
              min = 1,
              max = 50,
              value = 5,
              step = 1,
              sep = ''
            )
          ),
          column(7, plotOutput("plot3"))
        ),
        HTML('<br>'),
        HTML('<br>'),
        fluidRow(
          "Faceting ELO and Chess Rank by the selected value",
          selectInput("facetVal1", "Faceting Value", c("type", "country", "title")),
          HTML('<br>'),
          HTML('<br>'),
          plotOutput("plot5")
        )
      ),
      conditionalPanel(
        condition = "input.dataLevel == 'User Lifetime'",
        fluidRow(
          column(
            5,
            textInput("user2", "Username", value = "tolkienatic"),
            HTML('<br>'),
            "Frequency table comparing Win/Loss/Draw record to Chess Type"
          ),
          column(7, tableOutput("table6")),
        ),
        HTML('<br>'),
        HTML('<br>'),
        fluidRow(column(
          5,
          radioButtons(
            "gameType2",
            "Type of Chess",
            c("All", "Rapid", "Blitz", "Bullet"),
            selected = "All"
          )
        ), column(7, plotOutput("plot1"))),
      ),
      conditionalPanel(
        condition = "input.dataLevel == 'User Games'",
        fluidRow(column(
          5,
          textInput("user3", "Username", value = "tolkienatic"),
          selectInput(
            "freqVar2",
            "Frequency Variable Compared to Chess Type for the given Month and Year",
            c("Side Played" = "side", "Outcome" = "outcome"),
            selected = "side"
          )
        ), column(7, tableOutput("table7"))),
        HTML('<br>'),
        HTML('<br>'),
        HTML('<br>'),
        fluidRow(
          column(
            5,
            "Numeric Summary of ELO",
            HTML('<br>'),
            selectInput(
              "statVar1",
              "Side to summarize",
              c("User", "Opponent"),
              selected = "User"
            )
          ),
          column(7, tableOutput("table8"))
        ),
        HTML('<br>'),
        HTML('<br>'),
        HTML('<br>'),
        fluidRow(
          column(
            5,
            radioButtons(
              "gameType3",
              "Type of Chess",
              c("All", "Rapid", "Blitz", "Bullet"),
              selected = "All"
            ),
            sliderInput(
              "month3",
              "Month",
              min = 1,
              max = 12,
              value = 1,
              step = 1
            ),
            sliderInput(
              "year3",
              "Year",
              min = 2012,
              max = 2024,
              value = 2023,
              step = 1,
              sep = ''
            ),
          ),
          column(7, plotOutput("plot2"), HTML('<br>'), )
        ),
        fluidRow(plotOutput("plot4"))
      )
    ),
  )))
