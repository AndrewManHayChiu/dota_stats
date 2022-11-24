# The purpose of this script is to download detailed match data,
# which will then be saved locally rather than making calls each time.

source('./functions.R')

# Get any existing match data from rds file
matches <- readRDS("data/matches.rds")

# First step is to get players match IDs
mhRecentMatchData <- get_recent_matches_data(
  player_id = 208812212, 
  api_key = api_key, 
  limit = 400)

mhRecentMatchData

mh_match_ids <- mhRecentMatchData$match_id

# Compare to existing match data
# mh_match_ids[mh_match_ids %in% names(matches)]
# mh_match_ids[!mh_match_ids %in% names(matches)]

# TODO: Check if the existing math data has already been parsed

# Then download the match data for the match IDs

matches <- list() # for a fresh start

for (id in mh_match_ids[!mh_match_ids %in% names(matches)][1:10]) {
  match_data <- get_match_data(match_id = id, api_key = api_key)

  matches[[as.character(id)]] <- match_data
}

# Save data to rdata file
saveRDS(matches, "data/matches.rds")



# Match data exploration --------------------------------------------------

# match_data <- get_match_data(match_id = 6852756675, api_key = api_key)

match_id <- names(matches)[1]
match_id
match_data_list <- matches[[match_id]]
match_data_list

# names(match_data_list)
# 
# # Damage dealt
# names(match_data_list$players$damage)
# names(match_data_list$players$damage)[grepl('^npc_dota_hero', names(match_data_list$players$damage))]
# names(match_data_list$players$damage)[grepl('^illusion_npc_dota_hero', names(match_data_list$players$damage))]
# 
# player_damage_columns <- names(match_data_list$players$damage)[grepl('^npc_dota_hero', names(match_data_list$players$damage))]
# match_data_list$players$damage[player_damage_columns]
# lapply(match_data_list$players$damage[player_damage_columns], function(x) sum(x, na.rm = T))
# 
# # Damage received
# match_data_list$players$damage_taken
# player_damage_taken_cols <- names(match_data_list$players$damage_taken)[grepl('^npc_dota_hero', names(match_data_list$players$damage_taken))]
# data.frame(hero_name = player_damage_taken_cols, damage_received = unlist(lapply(match_data_list$players$damage_taken[player_damage_taken_cols], function(x) sum(x, na.rm = T))))
# 
# # Gold reasons
# gold_reasons <- match_data_list$players$gold_reasons
# names(gold_reasons) <- c('Other', 'Death', 'Sell', 'Building', 'Hero', 'Creep', 'Neutrals', 'Roshan', 'Courier', 'Rune', 19, 'Ward')
# gold_reasons
# 
# match_data_list$players$xp_reasons



get_match_stats <- function(data) {
  data.frame(
    match_id = data$match_id,
    player_id = data$players$account_id,
    rank_tier = data$players$rank_tier,
    hero_id = data$players$hero_id,
    is_radiant = data$players$isRadiant,
    radiant_win = data$players$radiant_win,
    lane_role = data$players$lane_role,     # use lane to determine core/support using net_wealth
    net_wealth = data$players$net_worth,     # use net_wealth to determine core/support for lanes,
    last_hits = data$players$last_hits,
    camps_stacked = data$players$camps_stacked,
    denies = data$players$denies,
    party_id = data$players$party_id,
    party_size = data$players$party_size,
    patch = data$patch,
    duration = data$duration,
    region = data$region,
    observers = data$players$purchase_ward_observer,
    sentries = data$players$purchase_ward_sentry,
    healing = data$players$hero_healing,
    smoke = data$players$item_uses['smoke_of_deceit']
  ) %>%
    left_join(lanes, by = c('lane_role' = 'lane_role')) %>%
    left_join(heroes[c('id','hero_name', 'localized_name')], by = c('hero_id' = 'id')) %>%
    left_join(rename(patches, patch = name), by = c('patch' = 'id')) %>%
    mutate(
      team = ifelse(is_radiant, 'Radiant', 'Dire'),
      region = ifelse(region == 5, 'Singapore', ifelse(region == 7, 'Australia', NA))
      ) %>%
    rowwise() %>%
    mutate(
      win = win(team, radiant_win),
      rank_tier_str = get_rank_tier(rank_tier)
    ) %>%
    ungroup() %>%
    group_by(team, lane) %>%
    mutate(core = net_wealth == max(net_wealth)) %>%
    ungroup() %>%
    left_join(positions, by = c('lane' = 'lane', 'core' = 'core')) %>%
    select(
      match_id, player_id, 
      rank_tier, rank_tier_str, 
      party_id, party_size, 
      hero = localized_name, 
      team, lane, position = position1, 
      win,
      net_wealth, last_hits, camps_stacked, denies, 
      patch = patch.y, duration, region, 
      observers, sentries, healing)
} 

get_match_stats(match_data_list)


# Scrape match stats from matches list ------------------------------------

match_stats_df <- data.frame()

match_ids <- names(matches)

for (match_id in match_ids) {
  print(match_id)

    match_stats_df <- match_stats_df %>%
    bind_rows(get_match_stats(matches[[match_id]]))
  
}

match_stats_df


# save match stats to csv -------------------------------------------------

write.csv(match_stats_df, "data/match_stats.csv", row.names = F)


# Match stats for battle report -------------------------------------------

match_stats <- read.csv('data/match_stats.csv')

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
    winrate = scales::percent(winrate)) %>%
  arrange(position)
roles_winrate

library(ggplot2)

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

# Dire vs Radiant

match_stats %>%
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
