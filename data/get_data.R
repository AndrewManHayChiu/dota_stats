# The purpose of this script is to download detailed match data,
# which will then be saved locally rather than making calls each time.

source('./functions.R')

patches <- c(7.25, 7.26, 7.27, 7.28, 7.29, 7.30, 7.31, 7.32)

patch_dates <- data.frame(
  patches,
  start_date = as.POSIXct(c(
    '2020-03-17', '2020-04-17', '2020-07-28', '2020-12-17', '2021-04-09', 
    '2021-08-18', '2022-02-23', '2022-08-23')),
  end_date = as.POSIXct(c(
    '2020-04-17', '2020-07-28', '2020-12-17', '2021-04-09', '2021-08-18', 
    '2022-02-23', '2022-08-23', as.character(Sys.Date()))),
  col = c("#4F4D8C", "#5F5DA6", "#8F8EBF", "#2E4159", "#262626", 
          "#4F4D8C", "#5F5DA6", "#8F8EBF"),
  x = c(as.POSIXct(c(
    '2020-03-20', '2020-04-20', '2020-08-01', '2020-12-20', '2021-04-12', 
    '2021-08-21', '2022-02-26', '2022-08-26'))),
  y = rep(0.025, 8)
)

players <- list(
  "MH"     = 208812212,
  "Bottle" = 1075592541,
  "Shiri"  = 156306162,
  "Cat"    = 103619307,
  "Bacon"  = 1075655293,
  "Mo"     = 152471066,
  "More"   = 1079351025,
  "Boss"   = 100501459
)

players_df <- data.frame(
  player_name = names(unlist(players)), 
  player_id = unlist(players)
)

heroes <- read.csv('data/heroes.csv')

lanes <- data.frame(
  lane_role = c(0, 1, 2, 3, 4),
  lane = c("Unknown", "Safe", "Mid", "Off", "Jungle")
)

positions <- data.frame(
  lane = c('Safe', 'Safe', 'Mid', 'Off', 'Off'),
  core = c(T, F, T, T, F),
  position1 = c('Pos1', 'Pos5', 'Pos2', 'Pos3', 'Pos4'),
  position2 = c('Safe Lane', 'Hard Support', 'Mid Lane', 'Soft Support', 'Off Lane')
)

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

mh_match_ids[mh_match_ids %in% names(matches)]
mh_match_ids[!mh_match_ids %in% names(matches)]

# Then download the match data for the match IDs

for (id in mh_match_ids[!mh_match_ids %in% names(matches)][1:2]) {
  match_data <- get_match_data(match_id = mh_match_ids[1], api_key = api_key)
  
  matches[[as.character(id)]] <- match_data
}

# match_data <- get_match_data(match_id = mh_match_ids[1], api_key = api_key)

# Saving data to rdata file

# Create a list to save match data as sub-lists
matches <- list()
matches[[as.character(match_data$match_id)]] <- match_data

matches

# match IDs in the list called matches
names(matches)

matches

saveRDS(matches, "data/matches.rds")



# Match data exploration --------------------------------------------------

match_data <- get_match_data(match_id = 6852756675, api_key = api_key)

names(match_data)

match_data$match_id

match_data$patch

match_data$region # 5 - Singapore, 7 - Australia

match_data$duration

# The real meat of the data is in the sub-list called 'players' 
match_data$players

# Use this to see if any friends were playing together
match_data$players$party_id
match_data$players$party_size
match_data$players$account_id

match_data$players$hero_id
match_data$players$hero_damage
match_data$players$hero_healing

match_data$players$creeps_stacked
match_data$players$camps_stacked
match_data$players$denies
match_data$players$last_hits

match_data$players$net_worth
match_data$players$total_xp

match_data$players$purchase_ward_observer
match_data$players$purchase_ward_sentry

# Damage dealt
names(match_data$players$damage)
names(match_data$players$damage)[grepl('^npc_dota_hero', names(match_data$players$damage))]
names(match_data$players$damage)[grepl('^illusion_npc_dota_hero', names(match_data$players$damage))]

player_damage_columns <- names(match_data$players$damage)[grepl('^npc_dota_hero', names(match_data$players$damage))]
match_data$players$damage[player_damage_columns]
lapply(match_data$players$damage[player_damage_columns], function(x) sum(x, na.rm = T))

# Damage received
match_data$players$damage_taken
player_damage_taken_cols <- names(match_data$players$damage_taken)[grepl('^npc_dota_hero', names(match_data$players$damage_taken))]
lapply(match_data$players$damage_taken[player_damage_taken_cols], function(x) sum(x, na.rm = T))

# Gold reasons
gold_reasons <- match_data$players$gold_reasons
names(gold_reasons) <- c('Other', 'Death', 'Sell', 'Building', 'Hero', 'Creep', 'Neutrals', 'Roshan', 'Courier', 'Rune', 19, 'Ward')
gold_reasons

# Smokes
match_data$players$item_uses[c('dust', 'smoke_of_deceit')]

match_data$players$xp_reasons

match_data$players$rank_tier

# useful match data
data.frame(
  match_id = match_data$match_id,
  player_id = match_data$players$account_id,
  hero_id = match_data$players$hero_id,
  is_radiant = match_data$players$isRadiant,
  radiant_win = match_data$players$radiant_win,
  lane_role = match_data$players$lane_role,     # use lane to determine core/support using net_wealth
  net_wealth = match_data$players$net_worth,     # use net_wealth to determine core/support for lanes,
  last_hits = match_data$players$last_hits,
  camps_stacked = match_data$players$camps_stacked,
  denies = match_data$players$denies,
  party_id = match_data$players$party_id,
  party_size = match_data$players$party_size,
  patch = match_data$patch,
  duration = match_data$duration
) %>%
  left_join(lanes) %>%
  left_join(heroes[c('id', 'localized_name')], by = c('hero_id' = 'id')) %>%
  mutate(team = ifelse(is_radiant, 'Radiant', 'Dire')) %>%
  group_by(team, lane) %>%
  mutate(core = net_wealth == max(net_wealth)) %>%
  ungroup() %>%
  left_join(positions) %>%
  select(
    match_id, player_id, party_id, party_size, hero = localized_name, 
    team, lane, position1, 
    net_wealth, last_hits, camps_stacked, denies, 
    patch, duration)


match_data
