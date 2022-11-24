custom_sidebar <- sidebarMenu(
  
  menuItem("Home", tabName = "home"
           # icon = icon("dashboard")
           ),
  menuItem("Player", tabName = "player", 
           # icon = icon("fa-solid fa-person-simple"),
           badgeLabel = "dev", badgeColor = "orange"
           ),
  menuItem("Data", tabName = "data"
           # icon = icon("table")
           ),
  br(),
  
  sliderInput(
    inputId = "averageOverSlider",
    label = "Matches",
    min = 1,
    max = 20,
    value = 20,
    step = 1,
    ticks = FALSE
  ),
  br(),
  
  p("Not yet implemented"),
  checkboxGroupInput(
    inputId = "gameModeCheckGroup",
    label = "Game Mode",
    choices = list(
      "All Pick" = 1,
      "Turbo" = 23),
    selected = c(1)
  ),
  br(),
  
  p("Not yet implemented"),
  checkboxGroupInput(
    inputId = "lobbyTypeCheckGroup",
    label = "Lobby Type",
    choices = list(
      "Normal" = 0,
      "Ranked" = 7),
    selected = c(0, 7)
  ),
  br(),
  
  p("Not yet implemented")
  # sliderInput(
  #   inputId = "patchSlider",
  #   label = "Patch",
  #   min = min(patches$name),
  #   max = max(patches$name),
  #   value = c(min(patches$name), max(patches$name))
  #   # step = 0.01
  # )
)