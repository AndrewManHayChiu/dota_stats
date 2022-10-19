homeBody <- tabItem(
  tabName = "home",
  h2("Overview"),
  
  # fluidRow(
  #   verbatimTextOutput("averageOverSlider")
  # ),
  
  h3("Win Rate"),
  fluidRow(
    box(
      title = "MH",
      color = "orange",
      valueBoxOutput("mhWinRateBox"),
      valueBoxOutput("mhKLABox")
    ),
    box(
      title = "Bottle",
      color = "blue",
      valueBoxOutput("bottleWinRateBox"),
      valueBoxOutput("bottleKLABox")
    ),
    box(
      title = "Shiri",
      color = "black",
      valueBoxOutput("shiriWinRateBox"),
      valueBoxOutput("shiriKLABox")
    ),
    box(
      title = "Bacon",
      color = "orange",
      valueBoxOutput("baconWinRateBox"),
      valueBoxOutput("baconKLABox")
    ),
    box(
      title = "Cat",
      color = "blue",
      valueBoxOutput("catWinRateBox"),
      valueBoxOutput("catKLABox")
    ),
    box(
      title = "More",
      color = "black",
      valueBoxOutput("moreWinRateBox"),
      valueBoxOutput("moreKLABox")
    ),
    box(
      title = "Boss",
      color = "black",
      valueBoxOutput("bossWinRateBox"),
      valueBoxOutput("bossKLABox")
    )
    # box(
    #   title = "Test",
    #   color = "black",
    #   box(
    #     title = "Win Rate",
    #     color = "orange",
    #     valueBoxOutput("mhWinRateBox")
    #   ),
    #   box(
    #     title = "KLA",
    #     color = "orange",
    #     valueBoxOutput("mhKLABox")
    #   )
    # )

  ),
  
  fluidRow(
    box(
      title = "KLA vs Winrate",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      plotOutput("klaWinRateScatterPlot")
    ),
    box(
      title = "Winrate Over Time",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      plotOutput("winRateTimeSeriesPlot")
    )
  )
  
  # h3("Two Stacks"),
  # fluidRow(),
  # 
  # h3("Three Stacks"),
  # fluidRow()
)
