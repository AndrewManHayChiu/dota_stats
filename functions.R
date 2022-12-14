library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(gt)

# API key stored in non-GIT-tracked folder, 'secret'
fileName <- 'secret/api_key.txt'
api_key <- readChar(fileName, file.info(fileName)$size)
rm(fileName)


# data --------------------------------------------------------------------

patches <- fromJSON("https://raw.githubusercontent.com/odota/dotaconstants/master/json/patch.json")
patch_colours <- c("#4F4D8C", "#5F5DA6", "#8F8EBF", "#2E4159", "#262626")

patch_dates <- patches %>%
  select(
    id,
    patch = name,
    start_date = date) %>%
  mutate(
    start_date = as.POSIXct(start_date),
    end_date = lead(start_date, n = 1, default = Sys.Date()),
    x = start_date + days(3),
    y = 0.025,
    col = rep(patch_colours, len = dim(patches)[1]))

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

heroes_json <- fromJSON("https://raw.githubusercontent.com/odota/dotaconstants/master/build/heroes.json")

heroes <- data.frame(
  id = unlist(lapply(heroes_json, function(x) x[[1]])),
  hero_name = unlist(lapply(heroes_json, function(x) x[[2]])),
  localized_name = unlist(lapply(heroes_json, function(x) x[[3]])),
  primary_attr = unlist(lapply(heroes_json, function(x) x[[4]])),
  attack_type = unlist(lapply(heroes_json, function(x) x[[5]]))
)

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


# Functions ---------------------------------------------------------------

value_box_colour <- function(value) {
  if (value < .4) {
    "red"
  } else if (value < 0.5) {
    "orange"
  } else if (value < 0.6) {
    "yellow"
  } else {
    "green"
  }
}

get_player_data <- function(player_id, api_key) {
  url <- paste0("https://api.opendota.com/api/players/", player_id, 
                "/?api_key=", api_key)
  res <- GET(url)
  player_data <- fromJSON(rawToChar(res$content))
  player_data
}

# get_player_data(player_id = 208812212, api_key = api_key)

get_match_data <- function(match_id, api_key = api_key) {
  url <- paste0(
    "https://api.opendota.com/api",
    "/matches/", match_id, 
    "?api_key=", api_key)
  res <- GET(url)
  match_data <- fromJSON(rawToChar(res$content))
  match_data
}

match_data <- get_match_data(match_id = 6856617943, api_key = api_key)

get_rank_tier <- function(rank_tier) {
  
  ranks <- list(
    "1" = "Herald",
    "2" = "Crusader",
    "3" = "Guardian",
    "4" = "Archon",
    "5" = "Legend",
    "6" = "Ancient",
    "7" = "Divine",
    "8" = "Immortal"
  )
  
  if (!is.na(rank_tier)) {
    rank_tier_str <- as.character(rank_tier)
    rank_int <- substring(rank_tier_str, 1, 1)
    tier_int <- substring(rank_tier_str, 2, 2)
    
    rank = ranks[[rank_int]]
    
    return(paste(rank, tier_int, sep = " "))
  } else {
    return(NA)
  }
}

# Test
# get_rank_tier(25)

win <- function(team, radiant_win) {
  if (team == "Radiant" & radiant_win == TRUE) {
    TRUE
  } else if (team == "Dire" & radiant_win == FALSE) {
    TRUE
  } else {
    FALSE
  }
}

get_recent_matches_data <- function(player_id, api_key = api_key, limit = 20, lobby_type = 7, game_mode = 1) {
  url <- paste0("https://api.opendota.com/api/players/", player_id, 
                "/matches",
                "/?api_key=", api_key,
                "?limit=", limit,
                "?lobby_type=", lobby_type,
                "?lobby_type=0",
                "?game_mode=", game_mode)
  res <- GET(url)
  
  if (res$status_code == 200) {
    
    recent_matches_data <- fromJSON(rawToChar(res$content))
    
    recent_matches_data$player_id <- player_id
    recent_matches_data$team <- ifelse(recent_matches_data$player_slot <= 127, 'Radiant', 'Dire')
    
    recent_matches_data %>%
      rowwise() %>%
      mutate(win = win(team, radiant_win)) %>%
      ungroup() %>%
      left_join(players_df, by = "player_id") %>%
      left_join(heroes, by = c("hero_id" = "id")) %>%
      arrange(desc(match_id)) %>%
      mutate(roll = zoo::rollmean(win, k = 20, fill = NA),
             date = as.POSIXct(start_time, tz = "UTC", origin = "1970-01-01"),
             duration_minutes = seconds_to_period(duration))
  }
}

# recent_match_data <- get_recent_matches_data(player_id = 208812212, api_key = api_key, limit = 100)
# recent_match_data <- get_recent_matches_data(player_id = 156306162, api_key = api_key, limit = 100)
# recent_match_data


get_match_stats <- function(data) {
  data.frame(
    match_id      = data$match_id,
    player_id     = data$players$account_id,
    rank_tier     = data$players$rank_tier,
    hero_id       = data$players$hero_id,
    is_radiant    = data$players$isRadiant,
    radiant_win   = data$players$radiant_win,
    lane_role     = data$players$lane_role,     # use lane to determine core/support using net_wealth
    net_wealth    = data$players$net_worth,     # use net_wealth to determine core/support for lanes,
    last_hits     = data$players$last_hits,
    camps_stacked = data$players$camps_stacked,
    denies        = data$players$denies,
    party_id      = data$players$party_id,
    party_size    = data$players$party_size,
    observers     = data$players$purchase_ward_observer,
    sentries      = data$players$purchase_ward_sentry,
    healing       = data$players$hero_healing,
    kills         = data$players$kills,
    deaths        = data$players$deaths,
    assists       = data$players$assists,
    patch         = data$patch,
    duration      = data$duration,
    region        = data$region
    # NA if 
    # smoke         = data$players$item_uses['smoke_of_deceit'],
  ) %>%
    left_join(lanes, by = c('lane_role' = 'lane_role')) %>%
    left_join(heroes[c('id','hero_name', 'localized_name')], by = c('hero_id' = 'id')) %>%
    left_join(rename(patches, patch = name), by = c('patch' = 'id')) %>%
    mutate(
      team = ifelse(is_radiant, 'Radiant', 'Dire'),
      region = ifelse(region == 5, 'Singapore', ifelse(region == 7, 'Australia', NA)),
      kla = (kills + assists) / (deaths + 1)
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
      kills, assists, deaths, kla,
      net_wealth, last_hits, camps_stacked, denies, 
      patch = patch.y, duration, region, 
      observers, sentries, healing)
} 

# match_id <- names(matches)[3]
# match_data_list <- matches[[match_id]]
# get_match_stats(match_data_list)

# Metrics -----------------------------------------------------------------

calc_kla_ratio <- function(recent_match_data) {
  kills_assists <- (sum(recent_match_data$kills) + sum(recent_match_data$assists))
  lives <- (sum(recent_match_data$deaths) + 1)
  
  kills_assists / lives
}

calc_longest_streak <- function(recent_match_data) {
  streak <- recent_match_data %>%
    select(win) %>%
    mutate(previous_win = lag(win),
           start = win != previous_win)
  
  streak[1, 'start'] <- 1
  
  streak <- streak %>%
    mutate(streak_id = cumsum(start)) %>%
    group_by(streak_id) %>%
    mutate(streak = row_number()) %>%
    ungroup()
  
  streak %>%
    group_by(win) %>%
    summarise(streak = max(streak))
}

# calc_longest_streak(mhRecentMatchData)