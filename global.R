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

# API key stored in non-GIT-tracked folder, 'secret'
fileName <- 'secret/api_key.txt'
api_key <- readChar(fileName, file.info(fileName)$size)

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
  "MH" = 208812212,
  "Bottle" = 1075592541,
  "Shiri" = 156306162,
  "Cat" = 103619307,
  "Bacon" = 1075655293,
  "Mo" = 152471066,
  "More" = 1079351025,
  "Boss" = 100501459
)

players_df <- data.frame(
  player_name = names(unlist(players)), 
  player_id = unlist(players)
)

heroes <- read.csv('data/heroes.csv')

valBoxWidth <- 3

get_win_rate <- function(id, api_key = api_key, matches) {
  url <- paste0("https://api.opendota.com/api/players/", id, 
                "/wl",
                "?api_key=", api_key,
                "&limit=", matches)
  res <- GET(url)
  wl_data <- fromJSON(rawToChar(res$content))
  wl_data$win / (wl_data$win + wl_data$lose)
}

# get_win_rate(id = "208812212", api_key = api_key, matches = "20")

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

# value_box_colour(0.3)

get_player_data <- function(player_id, api_key) {
  url <- paste0("https://api.opendota.com/api/players/", player_id, 
                "/?api_key=", api_key)
  res <- GET(url)
  player_data <- fromJSON(rawToChar(res$content))
  player_data
}

# player_data <- get_player_data(player_id = 208812212, api_key = api_key)
# player_data

win <- function(team, radiant_win) {
  if (team == "Radiant" & radiant_win == TRUE) {
    TRUE
  } else if (team == "Dire" & radiant_win == FALSE) {
    TRUE
  } else {
    FALSE
  }
}

# win(team = "Radiant", radiant_win = TRUE)
# win(team = "Radiant", radiant_win = FALSE)
# win(team = "Dire", radiant_win = TRUE)
# win(team = "Dire", radiant_win = FALSE)


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
  
  recent_matches_data <- recent_matches_data %>%
    rowwise() %>%
    mutate(win = win(team, radiant_win)) %>%
    ungroup()
  
  recent_matches_data %>%
    left_join(players_df, by = "player_id") %>%
    arrange(desc(match_id)) %>%
    mutate(roll = zoo::rollmean(win, k = 20, fill = NA),
           date = as.POSIXct(start_time, tz = "UTC", origin = "1970-01-01"))
}

# recent_match_data <- get_recent_matches_data(player_id = 208812212, limit = 100)
# recent_match_data

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


calc_kla_ratio <- function(recent_match_data) {
  kills_assists <- (sum(recent_match_data$kills) + sum(recent_match_data$assists))
  lives <- (sum(recent_match_data$deaths) + 1)
  
  kills_assists / lives
}

mhWinRateAllTime <- sum(mhRecentMatchData$win) / length(mhRecentMatchData$win)
