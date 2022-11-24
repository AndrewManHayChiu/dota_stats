playerBody <- tabItem(
  tabName = "player",
  
  h1("Battle Report"),
  
  p("Based on Dota's Battle Report"),
  # p(textOutput("selected_player")),
  # p(textOutput("selected_patch")),
  
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
      choices = tail(patches$name),
      justified = TRUE,
      status = "primary",
      selected = max(patches$name)
    )
  ),
  
  h2("Summary"),
  fluidRow(
    box(
      width = 4,
      title = "Featured Hero",
      h3(textOutput("battle_report_featured_hero"))
    ),
    box(
      width = 4,
      title = "Featured Lane",
      h3(textOutput("battle_report_featured_role"))
    ),
    box(
      width = 4,
      title = "Overall Performance",
      h3(textOutput("battle_report_win_rate"))
    ),
  ),
  
  h2("Highlights"),
  fluidRow(
    box(
      width = 4,
      title = "Game Duration",
      valueBox(
        value = textOutput('battle_report_shortest_game_duration'),
        subtitle = "Shortest Game"
      ),
      valueBox(
        value = textOutput('battle_report_average_game_duration'),
        subtitle = "Average Game"
      ),
      valueBox(
        value = textOutput('battle_report_longest_game_duration'),
        subtitle = "Longest Duration"
      )
      
    ),
    box(
      width = 4,
      title = "Dire vs Radiant Win Rate",
      box(
        title = "Radiant",
        textOutput("battle_report_radiant_winrate")
      ),
      box(
        title = "Dire",
        textOutput("battle_report_dire_winrate")
      )
    ),
    box(
      width = 4,
      title = "Overall Roles Played and Win Rate",
      plotOutput("battle_report_roles_winrate_plot")
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
