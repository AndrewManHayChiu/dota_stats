library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(ggplot2)
library(DT)

source('functions.R')

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