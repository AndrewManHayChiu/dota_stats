library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(lubridate)

# API key stored in non-GIT-tracked folder, 'secret'
fileName <- 'secret/api_key.txt'
api_key <- readChar(fileName, file.info(fileName)$size)

rm(fileName)

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

valBoxWidth <- 3

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
get_rank_tier(25)
get_rank_tier(NA)

# mhRecentMatchData %>% select(match_id, date)

# match_df <- get_match_data(match_id = "6814105296", api_key = api_key)

# match_df$lobby_type
# match_df$players$lane       # 1 Bot, 2 - mid, 3 - Top, 4 - Radiant Jungle, 5 - Dire Jungle
# match_df$players$lane_role  # 0 - Unknown, 1 - Safelane, 2 - Mid, 3 - Off, 4 - Jungle
# match_df$players$lane_pos
# match_df$players$camps_stacked
# match_df$players$
# match_df$patch
# match_df$region
# match_df$throw
# match_df$comeback
# match_df$loss
# match_df$win


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

# recent_match_data <- get_recent_matches_data(player_id = 208812212, limit = 100)
# recent_match_data

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
# calc_longest_streak(bottleRecentMatchData)




# Generate static data ----------------------------------------------------

mhRecentMatchData     <- get_recent_matches_data(player_id = 208812212, api_key = api_key, limit = 400)
bottleRecentMatchData <- get_recent_matches_data(player_id = 1075592541, api_key = api_key, limit = 400)
shiriRecentMatchData  <- get_recent_matches_data(player_id = 156306162, api_key = api_key, limit = 400)
baconRecentMatchData  <- get_recent_matches_data(player_id = 1075655293, api_key = api_key, limit = 400)
catRecentMatchData    <- get_recent_matches_data(player_id = 103619307, api_key = api_key, limit = 400)
moreRecentMatchData   <- get_recent_matches_data(player_id = 1079351025, api_key = api_key, limit = 400)
bossRecentMatchData   <- get_recent_matches_data(player_id = 100501459, api_key = api_key, limit = 400)

# COMBINED RECENT MATCH DATA
combinedRecentMatchData <- mhRecentMatchData %>%
  bind_rows(bottleRecentMatchData) %>%
  bind_rows(shiriRecentMatchData) %>%
  bind_rows(baconRecentMatchData) %>%
  bind_rows(catRecentMatchData) %>%
  bind_rows(moreRecentMatchData) %>%
  bind_rows(bossRecentMatchData) %>%
  left_join(
    heroes %>%
      select(hero_id = id, localized_name),
    by = "hero_id")

mhWinRateAllTime     <- sum(mhRecentMatchData$win) / length(mhRecentMatchData$win)
bottleWinRateAllTime <- sum(bottleRecentMatchData$win) / length(bottleRecentMatchData$win)
shiriWinRateAllTime  <- sum(shiriRecentMatchData$win) / length(shiriRecentMatchData$win)
baconWinRateAllTime  <- sum(baconRecentMatchData$win) / length(baconRecentMatchData$win)
catWinRateAllTime    <- sum(catRecentMatchData$win) / length(catRecentMatchData$win)
moreWinRateAllTime   <- sum(moreRecentMatchData$win) / length(moreRecentMatchData$win)
bossWinRateAllTime   <- sum(bossRecentMatchData$win) / length(bossRecentMatchData$win)