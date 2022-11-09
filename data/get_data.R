# The purpose of this script is to download detailed match data,
# which will then be saved locally rather than making calls each time.

# library(httr)
# library(jsonlite)
# library(dplyr)
# library(tidyr)
# library(lubridate)

# API key stored in non-GIT-tracked folder, 'secret'
# fileName <- '../secret/api_key.txt'
# api_key <- readChar(fileName, file.info(fileName)$size)
# rm(fileName)

# Same function as the one in global.R
# TODO: bring functions out of global.R so that the functions can be called
#       without running the calls
# get_match_data <- function(match_id, api_key = api_key) {
#   url <- paste0(
#     "https://api.opendota.com/api",
#     "/matches/", match_id, 
#     "?api_key=", api_key)
#   res <- GET(url)
#   match_data <- fromJSON(rawToChar(res$content))
#   match_data
# }

# match_df <- get_match_data(match_id = "6814105296", api_key = api_key)

# First step is to get players match IDs

source('./global.R')

# Get any existing match data from rds file
# matches <- readRDS("data/matches.rds")

mhRecentMatchData

mh_match_ids <- mhRecentMatchData$match_id

# Then download the match data for the match ID

match_data <- get_match_data(match_id = mh_match_ids[1], api_key = api_key)

names(match_data)

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
match_data$players$denies

match_data$players$net_worth
match_data$players$total_xp

match_data$players$lane_pos

match_data

# Saving data to rdata file

# Create a list to save match data as sub-lists
matches <- list()
matches[[as.character(match_data$match_id)]] <- match_data

matches

# match IDs in the list called matches
names(matches)

saveRDS(matches, "data/matches.rds")


