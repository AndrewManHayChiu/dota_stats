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


# Number of matches parsed ------------------------------------------------

  output$battle_report_games_parsed <- renderText({
    battle_report_data() %>%
      count() %>%
      .$n
  })  

# Game duration stats -----------------------------------------------------

  battle_report_game_duration_stats <- reactive({
    match_stats_df %>%
      filter(
        player_id %in% unlist(players),
        patch == input$patch_radioGroup
      ) %>%
      group_by(player_id == input$player_radioGroup) %>%
      summarise(
        avg_seconds = mean(duration)) %>%
      mutate(
        avg_minutes = floor(avg_seconds / 60)
      )  %>%
      rename(player = 1)
  })
  
  output$battle_report_game_duration_histogram <- renderPlot({
    battle_report_data() %>%
      mutate(
        min = floor(duration / 60),
        sec = round(duration %% 60, 0),
        minsec = paste(min, sec),
        minsec = lubridate::ms(minsec)
      ) %>%
      ggplot(aes(x = min)) +
      geom_histogram(binwidth = 10, colour = 'white') +
      geom_segment(
        data = battle_report_game_duration_stats(), 
        aes(x = avg_minutes, xend = avg_minutes ,
            y = 0, yend = 30),
        colour = 'orange',
        size = 1.5) +
      geom_text(
        data = battle_report_game_duration_stats(),
        aes(x = avg_minutes, y = c(-2, -1), label = c('Clan avg', 'Your avg'))
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
      labs(x = "Minutes", y = "Matches")
  })
  
  # battle_report_game_durations <- reactive({
  #   battle_report_data() %>%
  #     summarise(
  #       min = min(duration),
  #       avg = mean(duration),
  #       max = max(duration)) %>%
  #     gather(stat, duration) %>%
  #     mutate(
  #       min = floor(duration / 60),
  #       sec = round(duration %% 60, 0),
  #       minsec = paste(min, sec),
  #       minsec = lubridate::ms(minsec)
  #     )
  # })
  
  # output$battle_report_shortest_game_duration <- renderText({
  #   battle_report_game_durations() %>%
  #     filter(stat == 'min') %>%
  #     .$minsec %>%
  #     as.character()
  # })
  # 
  # output$battle_report_average_game_duration <- renderText({
  #   battle_report_game_durations() %>%
  #     filter(stat == 'avg') %>%
  #     .$minsec %>%
  #     as.character()
  # })
  # 
  # output$battle_report_longest_game_duration <- renderText({
  #   battle_report_game_durations() %>%
  #     filter(stat == 'max') %>%
  #     .$minsec %>%
  #     as.character()
  # })
  

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
        winrate = scales::percent(winrate),
        label_position = ifelse(games_played < 5, games_played + 2, games_played)) %>%
      arrange(position)
    
    roles_winrate %>%
      ggplot() +
      geom_col(aes(x = games_played, y = position, fill = position), show.legend = F) +
      geom_text(aes(x = label_position, y = position, label = winrate), hjust = -0.15) +
      geom_text(aes(x = label_position, y = position, label = position), hjust = 1.25) +
      scale_fill_manual(values = c("#8FC1B5", "#589A8D", "#007566", "#146551", "#265C4B")) + 
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank()) +
      labs(y = "",
           x = "") +
      scale_x_continuous(limits = c(0, max(roles_winrate$games_played + 1)))
  
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
  
  output$battle_report_team_winrate_plot <- renderPlot({
    
    battle_report_dire_radiant_winrate_data() %>%
      ggplot(aes(x = team)) +
      geom_col(aes(y = winrate)) +
      geom_text(aes(y = winrate, label = winrate), vjust = -1.5) +
      geom_text(aes(y = 0, label = team), vjust = -1.5, colour = "white") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
      labs(x = "", y = "")
  })


# Role statistics ---------------------------------------------------------
  
  # box_plot_aesthetics <- geom_boxplot(
  #   aes(fill = player),
  #   show.legend = F) +
  #   facet_grid(
  #     statistic ~ position,
  #     scales = "free",
  #     switch = "y") +
  #   theme_minimal() +
  #   theme(
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor = element_blank(),
  #     axis.text.y = element_blank(),
  #     # strip.text.y.left = element_text(angle = 0),
  #     strip.text.y.left = element_blank(),
  #     strip.background.x = element_rect(
  #       fill = "grey25",
  #       colour = "white"),
  #     strip.text.x = element_text(colour = "white", face = "bold")
  #   ) +
  #   labs(x = '', y = '')
  
  battle_report_role_data <- reactive({
    match_stats_df %>%
      filter(
        player_id %in% unlist(players),
        patch == input$patch_radioGroup
      ) %>%
      mutate(player = ifelse(player_id == input$player_radioGroup, 'You', 'Others')) %>%
      select(camps_stacked, last_hits, denies, 
             observers, sentries, 
             healing, 
             net_wealth,
             player, position) %>%
      gather(statistic, value, -position, -player)
  })
  
  output$battle_report_role_camps_stacked <- renderPlot({
    battle_report_role_data() %>%
      filter(statistic == 'camps_stacked') %>%
      ggplot(aes(x = player, y = value)) +
      geom_boxplot(
        aes(fill = player),
        show.legend = F) +
      facet_grid(
        statistic ~ position,
        scales = "free",
        switch = "y") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y.left = element_blank(),
        strip.background.x = element_rect(
          fill = "grey25",
          colour = "white"),
        strip.text.x = element_text(colour = "white", face = "bold")
      ) +
      labs(x = '', y = '') +
      scale_fill_manual(values = c("grey75", "#F5B142"))
  })
  
  output$battle_report_role_netwealth <- renderPlot({
    battle_report_role_data() %>%
      filter(statistic == 'net_wealth') %>%
      ggplot(aes(x = player, y = value)) +
      geom_boxplot(
        aes(fill = player),
        show.legend = F) +
      facet_grid(
        statistic ~ position,
        scales = "free",
        switch = "y") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y.left = element_blank(),
        strip.background.x = element_rect(
          fill = "grey25",
          colour = "white"),
        strip.text.x = element_text(colour = "white", face = "bold")
      ) +
      labs(x = '', y = '') +
      scale_fill_manual(values = c("grey75", "#A86D0C"))
  })
  
  output$battle_report_role_denies <- renderPlot({
    battle_report_role_data() %>%
      filter(statistic == 'denies') %>%
      ggplot(aes(x = player, y = value)) +
      geom_boxplot(
        aes(fill = player),
        show.legend = F) +
      facet_grid(
        statistic ~ position,
        scales = "free",
        switch = "y") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y.left = element_blank(),
        strip.background.x = element_rect(
          fill = "grey25",
          colour = "white"),
        strip.text.x = element_text(colour = "white", face = "bold")
      ) +
      labs(x = '', y = '') +
      scale_fill_manual(values = c("grey75", "#429EF5"))
  })

  output$battle_report_role_last_hits <- renderPlot({
    battle_report_role_data() %>%
      filter(statistic == 'last_hits') %>%
      ggplot(aes(x = player, y = value)) +
      geom_boxplot(
        aes(fill = player),
        show.legend = F) +
      facet_grid(
        statistic ~ position,
        scales = "free",
        switch = "y") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y.left = element_blank(),
        strip.background.x = element_rect(
          fill = "grey25",
          colour = "white"),
        strip.text.x = element_text(colour = "white", face = "bold")
      ) +
      labs(x = '', y = '') +
      scale_fill_manual(values = c("grey75", "#5EB1FF"))
  })

  output$battle_report_role_healing <- renderPlot({
    battle_report_role_data() %>%
      filter(statistic == 'healing') %>%
      ggplot(aes(x = player, y = value)) +
      geom_boxplot(
        aes(fill = player),
        show.legend = F) +
      facet_grid(
        statistic ~ position,
        scales = "free",
        switch = "y") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y.left = element_blank(),
        strip.background.x = element_rect(
          fill = "grey25",
          colour = "white"),
        strip.text.x = element_text(colour = "white", face = "bold")
      ) +
      labs(x = '', y = '') +
      scale_fill_manual(values = c("grey75", "#1D65A8"))
  })

  output$battle_report_role_observers <- renderPlot({
    battle_report_role_data() %>%
      filter(statistic == 'observers') %>%
      ggplot(aes(x = player, y = value)) +
      geom_boxplot(
        aes(fill = player),
        show.legend = F) +
      facet_grid(
        statistic ~ position,
        scales = "free",
        switch = "y") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y.left = element_blank(),
        strip.background.x = element_rect(
          fill = "grey25",
          colour = "white"),
        strip.text.x = element_text(colour = "white", face = "bold")
      ) +
      labs(x = '', y = '') +
          scale_fill_manual(values = c("grey75", "#F5B142"))
  })

  output$battle_report_role_sentries <- renderPlot({
    battle_report_role_data() %>%
      filter(statistic == 'sentries') %>%
      ggplot(aes(x = player, y = value)) +
      geom_boxplot(
        aes(fill = player),
        show.legend = F) +
      facet_grid(
        statistic ~ position,
        scales = "free",
        switch = "y") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y.left = element_blank(),
        strip.background.x = element_rect(
          fill = "grey25",
          colour = "white"),
        strip.text.x = element_text(colour = "white", face = "bold")
      ) +
      labs(x = '', y = '') +
      scale_fill_manual(values = c("grey75", "#A86D0C"))
  })



# Analysis page table -----------------------------------------------------

  output$battle_report_analysis_data <- DT::renderDataTable({
    battle_report_data() %>%
      group_by(position, hero) %>%
      summarise(
        Games           = length(match_id),
        Wins            = sum(win == T),
        Losses          = sum(win == F),
        Denies          = round(mean(denies), 2),
        Kills           = round(mean(kills), 2),
        Assists         = round(mean(assists), 2),
        KLA             = round(mean(kla),2),
        `Net Wealth`    = round(mean(net_wealth), 2),
        `Last Hits`     = round(mean(last_hits, na.rm = T), 2),
        `Camps Stacked` = round(mean(camps_stacked, na.rm = T), 2),
        Observers       = round(mean(observers, na.rm = T), 2),
        Sentries        = round(mean(sentries, na.rm = T), 2),
        Healing         = round(mean(healing, na.rm = T), 2)
      ) %>%
      mutate(
        Winrate = round(Wins / (Wins + Losses), 2)
      )
  },
  filter = 'top',
  options = list(
    autoWidth = T,
    scrollX = T
  ))
  
  output$battle_report_analysis_data_gt <- gt::render_gt({

    battle_report_data() %>%
      group_by(position, hero) %>%
      summarise(
        Games           = length(match_id),
        Wins            = sum(win == T),
        Losses          = sum(win == F),
        Denies          = round(mean(denies), 0),
        Kills           = round(mean(kills), 0),
        Assists         = round(mean(assists), 0),
        Deaths          = round(mean(deaths), 0),
        KLA             = round(mean(kla), 1),
        `Net Wealth`    = round(mean(net_wealth), 0),
        `Last Hits`     = round(mean(last_hits, na.rm = T), 0),
        `Camps Stacked` = round(mean(camps_stacked, na.rm = T), 1),
        Observers       = round(mean(observers, na.rm = T), 1),
        Sentries        = round(mean(sentries, na.rm = T), 1),
        Healing         = round(mean(healing, na.rm = T), 1)
      ) %>%
      mutate(
        Winrate   = round(Wins / (Wins + Losses), 1),
        Healing   = replace_na(0),
        Sentries  = replace_na(0),
        Observers = replace_na(0)
      ) %>%
      gt(
        rowname_col = "hero"
      ) %>%
      tab_header(
        title = "Role analysis",
        subtitle = "By Hero"
      ) %>%
      tab_spanner(
        label = "Support",
        columns = c("Observers", "Sentries", "Healing", "Camps Stacked")
      ) %>%
      tab_spanner(
        label = "Main",
        columns = c("Games", "Wins", "Losses", "Winrate")
      ) %>%
      tab_spanner(
        label = "Kills, Assists, Deaths",
        columns = c("Kills", "Assists", "Deaths", "KLA")
      ) %>%
      tab_spanner(
        label = "Wealth",
        columns = c("Last Hits", "Denies", "Net Wealth")
      )
  })
  

# Home page stats ---------------------------------------------------------

# Win rate boxes -----

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