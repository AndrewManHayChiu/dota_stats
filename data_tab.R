dataBody <- tabItem(
  
  tabName = "data",
  
  h1("Data"),
  
  fluidRow(
    radioGroupButtons(
      inputId = "data_radioGroup",
      label = "",
      choices = players,
      justified = TRUE,
      status = "primary",
      selected = 208812212
      # multiple = FALSE
    ),
  ),
  
  # fluidRow(
  #   textOutput("player_selected")
  # ),
  
  
  fluidRow(
    box(
      width = 12,
      h2("Recent matches"),
      downloadButton("downloadData", "Download"),
      dataTableOutput("recent_match_data")
    )
  )
)

