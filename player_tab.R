playerBody <- tabItem(
  tabName = "player",
  
  h1("Battle Report"),
  
  p("Inspired by Dota's Battle Report"),
  # p(textOutput("selected_player")),
  # p(textOutput("selected_patch")),
  
  fluidRow(
    column(
      width = 12,
      radioGroupButtons(
        inputId = "player_radioGroup",
        label = "Player",
        choices = players,
        justified = TRUE,
        status = "primary",
        selected = 208812212
      )
    ),
    column(
      width = 12, 
      radioGroupButtons(
        inputId = "patch_radioGroup",
        label = "Patch",
        choices = tail(patches$name),
        justified = TRUE,
        status = "primary",
        selected = max(patches$name)
      )
    )
  ),
  

  tabsetPanel(
    
# Summary -----------------------------------------------------------------
    tabPanel(
      title = "Summary",
      fluidRow(
        box(
          width = 4,
          title = "Featured Hero",
          h3(textOutput("battle_report_featured_hero"))
        ),
        box(
          width = 4,
          title = "Featured Lane",
          h3(textOutput("battle_report_featured_role"), align = 'centre')
        ),
        box(
          width = 4,
          title = "Overall Performance",
          h3(textOutput("battle_report_win_rate"))
        ),
      ),
      
      fluidRow(
        box(
          width = 4,
          title = "Matches (Parsed)",
          h3(textOutput("battle_report_games_parsed"))
        )
      )
    ),


# Highlights --------------------------------------------------------------
    tabPanel(
      title = "Highlights",
      fluidRow(
        box(
          width = 4,
          title = "Game Duration",
          # valueBox(
          #   value = textOutput('battle_report_shortest_game_duration'),
          #   subtitle = "Shortest Game"
          # ),
          # valueBox(
          #   value = textOutput('battle_report_average_game_duration'),
          #   subtitle = "Average Game"
          # ),
          # valueBox(
          #   value = textOutput('battle_report_longest_game_duration'),
          #   subtitle = "Longest Duration"
          # )
          plotOutput("battle_report_game_duration_histogram")
        ),
        
        box(
          width = 4,
          title = "Dire vs Radiant Win Rate",
          plotOutput("battle_report_team_winrate_plot")
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
          title = "Objectives",
          width = 4,
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
      )
    ),
    

# Role --------------------------------------------------------------------
    tabPanel(
      title = "Role",
      fluidRow(
        box(
          width = 12,
          title = "Camps Stacked",
          collapsible = T,
          plotOutput("battle_report_role_camps_stacked")
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          title = "Denies",
          collapsible = T,
          plotOutput("battle_report_role_denies")
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          title = "Last Hits",
          collapsible = T,
          plotOutput("battle_report_role_last_hits")
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          title = "Healing",
          collapsible = T,
          plotOutput("battle_report_role_healing")
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          title = "Net Wealth",
          collapsible = T,
          plotOutput("battle_report_role_netwealth")
        )
      ), # End Net Wealth fluidRow
      
      fluidRow(
        box(
          width = 12,
          title = "Observers",
          collapsible = T,
          plotOutput("battle_report_role_observers")
        )
      ), # End Observers fluidRow
      
      fluidRow(
        box(
          width = 12,
          title = "Sentries",
          collapsible = T,
          plotOutput("battle_report_role_sentries")
        )
      ) # End Sentries fluidRow
    ),


# Analysis ----------------------------------------------------------------
    tabPanel(
      title = "Analysis",
      
      # Another tab panel for Analysis
      tabsetPanel(
        # tabPanel(
        #   title = "Heroes",
        #   box(
        #     # width =12,
        #     # gt_output("battle_report_analysis_data_gt")
        #   )
        # ), # End Heroes tabPanel
        
        tabPanel(
          title = "Roles",
          fluidRow(
            box(
              width = 12,
              gt_output("battle_report_analysis_data_gt")
            )
          ) # End fluidRow
        ) # End Roles tabPanel
      )
    ) # End Analysis tabPanel
  )
)
