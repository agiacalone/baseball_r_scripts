### Get all the games for a specific season

# Load the required libraries
library(baseballr)
library(retrosheet)
library(dplyr)
library(tidyverse)
library(glue)
library(tibble)
library(readr)
library(knitr)
library(lubridate)

# Source our working R scripts
#source ("R/team_ids.R")
source ("R/season_games.R")

# Get the whole game schedule for the given season
output_markdown(season_games(season), "output/all_season_games.md")