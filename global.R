library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(ggplot2)
library(DT)

source('functions.R')

match_stats_df <- read.csv('data/match_stats.csv')

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
  bind_rows(bossRecentMatchData)

mhWinRateAllTime     <- sum(mhRecentMatchData$win) / length(mhRecentMatchData$win)
bottleWinRateAllTime <- sum(bottleRecentMatchData$win) / length(bottleRecentMatchData$win)
shiriWinRateAllTime  <- sum(shiriRecentMatchData$win) / length(shiriRecentMatchData$win)
baconWinRateAllTime  <- sum(baconRecentMatchData$win) / length(baconRecentMatchData$win)
catWinRateAllTime    <- sum(catRecentMatchData$win) / length(catRecentMatchData$win)
moreWinRateAllTime   <- sum(moreRecentMatchData$win) / length(moreRecentMatchData$win)
bossWinRateAllTime   <- sum(bossRecentMatchData$win) / length(bossRecentMatchData$win)