# The purpose of this script is to download detailed match data,
# which will then be saved locally rather than making calls each time.
# If match data has not been parsed, it will not be saved to the raw data,
# so that the loop may check next time if the data has been manually parsed.

setwd("C:/Users/amhch/OneDrive/Projects/dota_stats")

source('./functions.R')
library(ggplot2)

# Get any existing match data from rds file
matches <- readRDS("data/matches.rds")

for (player_id in players) {
  
  print(paste("Player ID:", player_id))
  
  player_recent_matches <- get_recent_matches_data(
    player_id = player_id,
    api_key = api_key,
    limit = 400
  )
  
  # Some accounts threw server error (status_code: 500)
  # get_recent_matches_data function modified to return NULL in this case
  if (!is.null(player_recent_matches)) {
    
    player_match_ids <- player_recent_matches$match_id
    
    for (match_id in player_match_ids[!player_match_ids %in% names(matches)][1:30]) {
      
      print(paste("Match ID:", match_id))
      
      match_data <- get_match_data(match_id = match_id, api_key = api_key)
      
      # Don't keep the data if match has not been parsed
      # This ensures that the raw data will not be saved
      # and can be filled in the future after it has been parsed
      if (length(match_data$players$ability_uses) > 10) {
        print("Match data parsed")
        matches[[as.character(match_id)]] <- match_data
      } else if (length(match_data$players$ability_uses) == 10) {
        print("Match data not parsed")
      } else {
        print("Match data not available")
      }
    }
  } else {
    print("No data available")
  }
}

# Save raw data to rdata file
saveRDS(matches, "data/matches.rds")


# Function to extract match stats from raw data ---------------------------

# get_match_stats <- function(data) {
#   data.frame(
#     match_id      = data$match_id,
#     player_id     = data$players$account_id,
#     rank_tier     = data$players$rank_tier,
#     hero_id       = data$players$hero_id,
#     is_radiant    = data$players$isRadiant,
#     radiant_win   = data$players$radiant_win,
#     lane_role     = data$players$lane_role,     # use lane to determine core/support using net_wealth
#     net_wealth    = data$players$net_worth,     # use net_wealth to determine core/support for lanes,
#     last_hits     = data$players$last_hits,
#     camps_stacked = data$players$camps_stacked,
#     denies        = data$players$denies,
#     party_id      = data$players$party_id,
#     party_size    = data$players$party_size,
#     observers     = data$players$purchase_ward_observer,
#     sentries      = data$players$purchase_ward_sentry,
#     healing       = data$players$hero_healing,
#     smoke         = data$players$item_uses['smoke_of_deceit'],
#     patch         = data$patch,
#     duration      = data$duration,
#     region        = data$region
#   ) %>%
#     left_join(lanes, by = c('lane_role' = 'lane_role')) %>%
#     left_join(heroes[c('id','hero_name', 'localized_name')], by = c('hero_id' = 'id')) %>%
#     left_join(rename(patches, patch = name), by = c('patch' = 'id')) %>%
#     mutate(
#       team = ifelse(is_radiant, 'Radiant', 'Dire'),
#       region = ifelse(region == 5, 'Singapore', ifelse(region == 7, 'Australia', NA))
#     ) %>%
#     rowwise() %>%
#     mutate(
#       win = win(team, radiant_win),
#       rank_tier_str = get_rank_tier(rank_tier)
#     ) %>%
#     ungroup() %>%
#     group_by(team, lane) %>%
#     mutate(core = net_wealth == max(net_wealth)) %>%
#     ungroup() %>%
#     left_join(positions, by = c('lane' = 'lane', 'core' = 'core')) %>%
#     select(
#       match_id, player_id, 
#       rank_tier, rank_tier_str, 
#       party_id, party_size, 
#       hero = localized_name, 
#       team, lane, position = position1, 
#       win,
#       net_wealth, last_hits, camps_stacked, denies, 
#       patch = patch.y, duration, region, 
#       observers, sentries, healing)
# } 

# match_id <- names(matches)[3]
# match_data_list <- matches[[match_id]]
# match_data_list
# 
# get_match_stats(match_data_list)


# Scrape match stats from matches list ------------------------------------

match_stats_df <- data.frame()

for (match_id in names(matches)) {
  print(match_id)
  
  match_stats_df <- match_stats_df %>%
    bind_rows(get_match_stats(matches[[match_id]]))
  
}

match_stats_df


# save match stats to csv -------------------------------------------------

write.csv(match_stats_df, "data/match_stats.csv", row.names = F)

# Match data exploration --------------------------------------------------

matches[["6862808469"]]$players$kda
matches[["6862808469"]]$players$kills
matches[["6862808469"]]$players$deaths
matches[["6862808469"]]$players$assists

# Match stats for battle report -------------------------------------------

match_stats <- read.csv('data/match_stats.csv')

# Matches parsed for the player
match_stats %>%
  filter(
    player_id == 208812212,
    patch == 7.32
  ) %>%
  count() %>%
  .$n

# win rate
matches_won <- match_stats %>%
  filter(
    player_id == 208812212,
    patch == 7.32
  ) %>%
  count(win) %>%
  filter(win == T) %>%
  .$n


matches_lost <- match_stats %>%
  filter(
    player_id == 156306162,
    patch == 7.32
  ) %>%
  count(win) %>%
  filter(win == F) %>%
  .$n

matches_won <- ifelse(length(matches_won) == 0, 0, matches_won)
matches_lost <- ifelse(length(matches_lost) == 0, 0, matches_lost)

winrate <- matches_won / (matches_won + matches_lost)

scales::percent(winrate)

paste0(matches_won, "-", matches_lost, " ", scales::percent(winrate), " Winrate")

# Game durations

match_stats %>%
  filter(
    player_id == 208812212,
    patch == 7.32
  ) %>%
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

game_duration_stats <- match_stats %>%
  filter(
    player_id %in% unlist(players),
    patch == 7.32
  ) %>%
  group_by(player_id == 208812212) %>%
  summarise(
    avg_seconds = mean(duration)) %>%
  mutate(
    avg_minutes = floor(avg_seconds / 60)
  )  %>%
  rename(player = 1)

game_duration_stats

match_stats %>%
  filter(
    player_id == 208812212,
    patch == 7.32
  ) %>%
  mutate(
    min = floor(duration / 60),
    sec = round(duration %% 60, 0),
    minsec = paste(min, sec),
    minsec = lubridate::ms(minsec))
  ggplot(aes(x = min)) +
  geom_histogram(binwidth = 10, colour = 'white') +
  geom_segment(
    data = game_duration_stats, 
    aes(x = avg_minutes, xend = avg_minutes ,
        y = 0, yend = 30),
    colour = 'orange',
    size = 1.5) +
  geom_text(
    data = game_duration_stats,
    aes(x = avg_minutes, y = c(-2, -1), label = c('Clan avg', 'Your avg'))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(x = "Minutes", y = "Matches")



# Roles played and win rate

roles_winrate <- match_stats %>%
  filter(
    player_id == 208812212,
    patch == 7.32
  ) %>%
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

roles_winrate

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

# Dire vs Radiant

battle_report_dire_radiant_winrate_data <- match_stats %>%
  filter(
    player_id == 208812212,
    patch == 7.32
  ) %>%
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

battle_report_dire_radiant_winrate_data

battle_report_dire_radiant_winrate_data %>%
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



# Heroes played -----------------------------------------------------------

match_stats %>%
  filter(
    player_id == 208812212,
    patch == 7.32
  ) %>%
  count(hero) %>%
  arrange(desc(n))


# Role stats -----------------------------------------------------------

match_stats %>%
  filter(
    player_id %in% unlist(players),
    patch == 7.32
  ) %>%
  mutate(player = ifelse(player_id == 208812212, 'You', 'Others')) %>%
  select(camps_stacked, last_hits, denies, 
         observers, sentries, 
         healing, 
         net_wealth,
         player, position) %>%
  gather(statistic, value, -position, -player) %>%
  # filter(statistic == 'camps_stacked') %>%
  ggplot(aes(x = player, y = value)) +
  geom_boxplot(
    aes(fill = player),
    notch = F,
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
    strip.text.y.left = element_text(angle = 0),
    # strip.text.y.left = element_blank(),
    strip.background.x = element_rect(
      fill = "grey25", 
      colour = "white"),
    strip.text.x = element_text(colour = "white", face = "bold")
  ) +
  scale_fill_manual(values = c("grey75", "#F5B142")) +
  labs(x = '', y = '')


# KLA scatter -------------------------------------------------------------

match_stats %>%
  filter(
    player_id %in% unlist(players),
    patch == 7.32
  ) %>%
  select(kills, deaths, assists, kla, win, position) %>%
  ggplot() +
  geom_jitter(aes(x = assists, y = deaths, colour = win)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



# Battle Pass Analysis Page -----------------------------------------------



# Roles analysis ----------------------------------------------------------

rounding = 0

match_stats %>%
  filter(
    player_id %in% unlist(players),
    patch == 7.32
  ) %>%
  group_by(position, hero) %>%
  summarise(
    Games           = length(match_id),
    Wins            = sum(win == T),
    Losses          = sum(win == F),
    Denies          = round(mean(denies), rounding),
    Kills           = round(mean(kills), rounding),
    Assists         = round(mean(assists), rounding),
    Deaths          = round(mean(deaths), rounding),
    KLA             = round(mean(kla), 1),
    `Net Wealth`    = round(mean(net_wealth), rounding),
    `Last Hits`     = round(mean(last_hits, na.rm = T), rounding),
    `Camps Stacked` = round(mean(camps_stacked, na.rm = T), rounding),
    Observers       = round(mean(observers, na.rm = T), rounding),
    Sentries        = round(mean(sentries, na.rm = T), rounding),
    Healing         = round(mean(healing, na.rm = T), rounding)
  ) %>%
  mutate(
    Winrate   = round(Wins / (Wins + Losses), rounding),
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
