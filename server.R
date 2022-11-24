function(input, output, session) {
  
  # RECENT MATCH DATA REACTIVE
  mhRecentMatchDataReactive <- reactive({
    mhRecentMatchData %>% top_n(input$averageOverSlider, wt = match_id)
  })
  
  bottleRecentMatchDataReactive <- reactive({
    bottleRecentMatchData %>% top_n(input$averageOverSlider, wt = match_id)
  })

  shiriRecentMatchDataReactive <- reactive({
    shiriRecentMatchData %>% top_n(input$averageOverSlider, wt = match_id)
  })

  baconRecentMatchDataReactive <- reactive({
    baconRecentMatchData %>% top_n(input$averageOverSlider, wt = match_id)
  })

  catRecentMatchDataReactive <- reactive({
    catRecentMatchData %>% top_n(input$averageOverSlider, wt = match_id)
  })

  moreRecentMatchDataReactive <- reactive({
    moreRecentMatchData %>% top_n(input$averageOverSlider, wt = match_id)
  })

  bossRecentMatchDataReactive <- reactive({
    bossRecentMatchData %>% top_n(input$averageOverSlider, wt = match_id)
  })
  
  # WIN RATES
  mhWinRate <- reactive({
    sum(mhRecentMatchDataReactive()$win) / length(mhRecentMatchDataReactive()$win)
  })
  
  bottleWinRate <- reactive({
    sum(bottleRecentMatchDataReactive()$win) / length(bottleRecentMatchDataReactive()$win)
  })

  shiriWinRate <- reactive({
    sum(shiriRecentMatchDataReactive()$win) / length(shiriRecentMatchDataReactive()$win)
  })

  baconWinRate <- reactive({
    sum(baconRecentMatchDataReactive()$win) / length(baconRecentMatchDataReactive()$win)
  })

  catWinRate <- reactive({
    sum(catRecentMatchDataReactive()$win) / length(catRecentMatchDataReactive()$win)
  })

  moreWinRate <- reactive({
    sum(moreRecentMatchDataReactive()$win) / length(moreRecentMatchDataReactive()$win)
  })

  bossWinRate <- reactive({
    sum(bossRecentMatchDataReactive()$win) / length(bossRecentMatchDataReactive()$win)
  })
  
  # KLA RATIOS
  mhKLA <- reactive({
    calc_kla_ratio(mhRecentMatchDataReactive() %>% top_n(input$averageOverSlider))
  })
  
  bottleKLA <- reactive({
    calc_kla_ratio(bottleRecentMatchDataReactive())
  })

  shiriKLA <- reactive({
    calc_kla_ratio(shiriRecentMatchDataReactive())
  })

  baconKLA <- reactive({
    calc_kla_ratio(baconRecentMatchDataReactive())
  })

  catKLA <- reactive({
    calc_kla_ratio(catRecentMatchDataReactive())
  })

  moreKLA <- reactive({
    calc_kla_ratio(moreRecentMatchDataReactive())
  })

  bossKLA <- reactive({
    calc_kla_ratio(bossRecentMatchDataReactive())
  })


# Battle Report data ------------------------------------------------------

  output$selected_player <- renderText({
    input$player_radioGroup
  })
  
  output$selected_patch <- renderText({
    input$patch_radioGroup
  })
  
  battle_report_data <- reactive({
    match_stats_df %>%
      filter(player_id == input$player_radioGroup,
             patch == input$patch_radioGroup)
      # filter(player_id == 208812212)
  })
  

# Featured Hero stats -----------------------------------------------------

  
  output$battle_report_featured_hero <- renderText({
    battle_report_data() %>%
      count(hero) %>%
      top_n(1, wt = n) %>%
      .$hero
  })
  
  output$battle_report_featured_role <- renderText({
    battle_report_data() %>%
      count(position) %>%
      top_n(1, wt = n) %>%
      .$position
  })
  

# Win rate stats ----------------------------------------------------------

  output$battle_report_win_rate <- renderText({
    matches_won <- battle_report_data() %>%
      count(win) %>%
      filter(win == T) %>%
      .$n
    
    matches_lost <- battle_report_data() %>%
      count(win) %>%
      filter(win == F) %>%
      .$n
    
    matches_won <- ifelse(length(matches_won) == 0, 0, matches_won)
    matches_lost <- ifelse(length(matches_lost) == 0, 0, matches_lost)
    
    winrate <- matches_won / (matches_won + matches_lost)
    
    paste(matches_won, "-", matches_lost, "|", scales::percent(winrate), "Winrate", sep = " ")
  })
  

# Game duration stats -----------------------------------------------------

  battle_report_game_durations <- reactive({
    battle_report_data() %>%
      summarise(
        min = min(duration),
        avg = mean(duration),
        max = max(duration)) %>%
      gather(stat, duration) %>%
      mutate(
        min = floor(duration / 60),
        sec = round(duration %% 60, 0),
        minsec = paste(min, sec),
        minsec = lubridate::ms(minsec)
      )
  })
  
  output$battle_report_shortest_game_duration <- renderText({
    battle_report_game_durations() %>%
      filter(stat == 'min') %>%
      .$minsec %>%
      as.character()
  })
  
  output$battle_report_average_game_duration <- renderText({
    battle_report_game_durations() %>%
      filter(stat == 'avg') %>%
      .$minsec %>%
      as.character()
  })
  
  output$battle_report_longest_game_duration <- renderText({
    battle_report_game_durations() %>%
      filter(stat == 'max') %>%
      .$minsec %>%
      as.character()
  })
  

# Roles and winrates ------------------------------------------------------

  output$battle_report_roles_winrate_plot <- renderPlot({
    roles_winrate <- battle_report_data() %>%
      group_by(position, win) %>%
      count() %>%
      ungroup() %>%
      spread(win, n) %>%
      full_join(
        data.frame(position = c("Pos1", "Pos2", "Pos3", "Pos4", "Pos5"))
      ) %>%
      rename(lost = `FALSE`, won = `TRUE`) %>%
      replace_na(list(lost = 0, won = 0)) %>%
      mutate(
        games_played = lost + won,
        winrate = won / (won + lost),
        winrate = ifelse(is.na(winrate), 0, winrate),
        winrate = scales::percent(winrate)) %>%
      arrange(position)
    roles_winrate
    
    roles_winrate %>%
      ggplot() +
      geom_col(aes(x = games_played, y = position)) +
      geom_text(aes(x = games_played, y = position, label = winrate), hjust = -0.5,) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
      labs(y = "",
           x = "")
  })
  

# Radiant Dire Winrates ---------------------------------------------------

  battle_report_dire_radiant_winrate_data <- reactive({
    battle_report_data() %>%
      group_by(team, win) %>%
      count() %>%
      ungroup() %>% 
      spread(win, n) %>%
      rename(lost = `FALSE`, won = `TRUE`) %>%
      replace_na(list(lost = 0, won = 0)) %>%
      mutate(
        games_played = lost + won,
        winrate = won / games_played,
        winrate = ifelse(is.na(winrate), 0, winrate),
        winrate = scales::percent(winrate))
  })
  
  battle_report_radiant_winrate <- renderText({
    battle_report_dire_radiant_winrate_data() %>%
      filter(team == 'Radiant') %>%
      .$winrate %>%
      as.character()
  })
  
  battle_report_dire_winrate <- renderText({
    battle_report_dire_radiant_winrate_data() %>%
      filter(team == 'Dire') %>%
      .$winrate %>%
      as.character()
  })



# Win rate boxes ----------------------------------------------------------

  output$mhWinRateBox <- renderValueBox({
    valueBox(
      scales::percent(round(mhWinRate(), 2)), 
      "Win Rate", 
      color = value_box_colour(mhWinRate())
    )
  })
  
  output$bottleWinRateBox <- renderValueBox({
    valueBox(
      scales::percent(round(bottleWinRate(), 2)),
      "Win Rate", color = value_box_colour(bottleWinRate())
    )
  })

  output$shiriWinRateBox <- renderValueBox({
    valueBox(
      scales::percent(round(shiriWinRate(), 2)),
      "Win Rate",
      color = value_box_colour(shiriWinRate())
    )
  })

  output$catWinRateBox <- renderValueBox({
    valueBox(
      scales::percent(round(catWinRate(), 2)),
      "Win Rate",
      color = value_box_colour(catWinRate())
    )
  })

  output$baconWinRateBox <- renderValueBox({
    valueBox(
      scales::percent(round(baconWinRate(), 2)),
      "Win Rate",
      color = value_box_colour(baconWinRate())
    )
  })

  output$moreWinRateBox <- renderValueBox({
    valueBox(
      scales::percent(round(moreWinRate(), 2)),
      "Win Rate",
      color = value_box_colour(moreWinRate())
    )
  })

  output$bossWinRateBox <- renderValueBox({
    valueBox(
      scales::percent(round(bossWinRate(), 2)),
      "Win Rate",
      color = value_box_colour(bossWinRate())
    )
  })
  
  
  # KLA BOXES
  output$mhKLABox <- renderValueBox({
    valueBox(
      round(mhKLA(), 2), 
      "KLA"
    )
  })
  
  output$bottleKLABox <- renderValueBox({
    valueBox(
      round(bottleKLA(), 2),
      "KLA"
    )
  })

  output$shiriKLABox <- renderValueBox({
    valueBox(
      round(shiriKLA(), 2),
      "KLA"
    )
  })

  output$baconKLABox <- renderValueBox({
    valueBox(
      round(baconKLA(), 2),
      "KLA"
    )
  })

  output$catKLABox <- renderValueBox({
    valueBox(
      round(catKLA(), 2),
      "KLA"
    )
  })

  output$moreKLABox <- renderValueBox({
    valueBox(
      round(moreKLA(), 2),
      "KLA"
    )
  })

  output$bossKLABox <- renderValueBox({
    valueBox(
      round(bossKLA(), 2),
      "KLA"
    )
  })
  
  # KLA vs WIN RATE SCATTER PLOT
  output$klaWinRateScatterPlot <- renderPlot({

    mhRecentMatchDataReactive() %>%
      bind_rows(bottleRecentMatchDataReactive()) %>%
      bind_rows(shiriRecentMatchDataReactive()) %>%
      bind_rows(catRecentMatchDataReactive()) %>%
      bind_rows(baconRecentMatchDataReactive()) %>%
      bind_rows(bossRecentMatchDataReactive()) %>%
      bind_rows(moreRecentMatchDataReactive()) %>%
      group_by(player_name) %>%
      summarise(kla = (sum(kills) + sum(assists)) / (sum(deaths) + 1),
                wr = sum(win) / length(win)) %>%
      ggplot(aes(x = kla, y = wr)) +
      geom_point(aes(colour = player_name), size = 4) +
      geom_smooth(method = "lm", formula = "y ~ x") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent"),
            legend.position = "bottom",
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            axis.title.x = element_text(margin = margin(t = 20)),
            axis.title.y = element_text(margin = margin(r = 20)),
            title = element_text(face = "bold")) +
      labs(x = "KLA Ratio", y = "Win Rate", title = "Win Rate vs KLA") +
      guides(colour = guide_legend(title = "Player"))
  })
  
  # WINRATE TIME SERIES PLOT
  
  output$winRateTimeSeriesPlot <- renderPlot({

    ggplot() +
      geom_rect(
        data = patch_dates,
        aes(alpha = 0.9),
        xmin = patch_dates$start_date,
        xmax = patch_dates$end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = patch_dates$col,
        show.legend = FALSE
      ) +
    geom_line(
      data = combinedRecentMatchData,
      aes(x = date, y = roll, group = player_name, colour = player_name),
      size = 1
    ) +
    geom_text(
      data = patch_dates,
      aes(x = x, y = y, label = patch),
      size = 3,
      hjust = 0,
      vjust = 2
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75),
      labels = scales::percent
    ) +
    scale_x_continuous(
      limits = as.POSIXct(c(as.character(Sys.Date() - 100), as.character(Sys.Date()))),
      breaks = as.POSIXct(c(as.character(Sys.Date() - 100), as.character(Sys.Date() - 50), as.character(Sys.Date())))
    ) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent"),
      legend.position = "bottom",
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      # axis.title.x = element_text(margin = margin(t = 20)),
      axis.title.y = element_text(margin = margin(r = 20)),
      title = element_text(face = "bold")
    ) +
    labs(
      title = "Winrate Over Time",
      x = "",
      y = "Winrate"
    ) +
    guides(colour = guide_legend(title = "Player"))

  })
  
  # DATA OUTPUT
  data_output <- reactive({
    if (input$data_radioGroup == 208812212) {
      mhRecentMatchData
    } else if (input$data_radioGroup == 1075592541) {
      bottleRecentMatchData
    } else if (input$data_radioGroup == 156306162) {
      shiriRecentMatchData
    } else if (input$data_radioGroup == 1075655293) {
      baconRecentMatchData
    } else if (input$data_radioGroup == 103619307) {
      catRecentMatchData
    } else if (input$data_radioGroup == 1079351025) {
      moreRecentMatchData
    } else if (input$data_radioGroup == 100501459) {
      bossRecentMatchData
    }
  })

  output$recent_match_data <- DT::renderDataTable(
    data_output(),
    options = list(scrollX = TRUE)
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$data_radioGroup, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_output(), file, row.names = FALSE)
    }
  )
  
}