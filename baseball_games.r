### Run the scripts from here

library(baseballr)
library(retrosheet)
library(dplyr)
library(tidyverse)
library(glue)
library(tibble)
library(readr)
library(knitr)
library(lubridate)
library(ggplot2)

## Set variables for script use
season = 2025
today <- Sys.Date()
yesterday <- today - 1
tomorrow <- today + 1

## Get info on games
source ("scripts/all_games_for_season.R")
source ("scripts/get_all_games_yesterday.R")
source ("scripts/get_all_games_today.R")
source ("scripts/get_all_games_tomorrow.R")

## Get info on the teams
source ("scripts/get_team_ids.R")

## Get the current standings
source ("scripts/get_standings.R")

## Get info on specific teams
source ("scripts/giants.R")
source ("scripts/angels.R")
source ("scripts/padres.R")
source ("scripts/dodgers.R")
source ("scripts/mets.R")
source ("scripts/athletics.R")