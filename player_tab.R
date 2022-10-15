playerBody <- tabItem(
  tabName = "player",
  
  h1("Battle Report"),
  
  p("Based on Dota's Battle Report"),
  
  fluidRow(
    radioGroupButtons(
      inputId = "player_radioGroup",
      label = "Player",
      choices = players,
      justified = TRUE,
      status = "primary",
      selected = 208812212
    ),
    radioGroupButtons(
      inputId = "patch_radioGroup",
      label = "Patch",
      choices = patches,
      justified = TRUE,
      status = "primary",
      selected = 7.32
    )
  ),
  
  h2("Summary"),
  fluidRow(
    box(
      width = 4,
      title = "Featured Hero"
    ),
    box(
      width = 4,
      title = "Featured Lane"
    ),
    box(
      width = 4,
      title = "Overall Performance",
      valueBox(
        value = "5 - 10",
        subtitle = "Win - Loss"
      ),
      valueBox(
        value = "33%",
        subtitle = "Win Rate"
      )
    ),
  ),
  
  h2("Highlights"),
  fluidRow(
    box(
      width = 4,
      title = "Game Duration",
      valueBox(
        value = "20 mins",
        subtitle = "Shortest Game"
      ),
      valueBox(
        value = "65 mins",
        subtitle = "Longest Game"
      ),
      valueBox(
        value = "30 mins",
        subtitle = "Average Duration"
      )
      
    ),
    box(
      width = 4,
      title = "Dire vs Radiant Win Rate"
    ),
    box(
      width = 4,
      title = "Overall Roles Played and Win Rate"
    )
  ),
  
  fluidRow(
    box(
      title = "Game",
      width = 4,
      valueBox(
        value = 1,
        subtitle = "Games Played"
      ),
      valueBox(
        value = 1,
        subtitle = "Heroes Played"
      ),
      valueBox(
        value = 1,
        subtitle = "Max Win Streak"
      ),
      valueBox(
        value = 1,
        subtitle = "Max Loss Streak"
      ),
      valueBox(
        value = 1,
        subtitle = "Lane Winrate"
      )
    ),
    box(
      title = "Heroes",
      width = 4,
      
    ),
    box(
      title = "Map Objectives",
      width = 4,
      valueBox(
        value = 1,
        subtitle = "Roshan Kills"
      ),
      valueBox(
        value = 1,
        subtitle = "Average Tower Damage"
      ),
      valueBox(
        value = 1,
        subtitle = "Average Dewards"
      ),
      valueBox(
        value = 1,
        subtitle = "Average Bounty Runes"
      )
    )
  ),
  
  h2("Analysis"),
  fluidRow(
    
  )
)