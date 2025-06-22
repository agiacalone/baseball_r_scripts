### Variables for the Los Angeles Angels

season = 2025
teamID <- 108 # Los Angeles Angels
today <- Sys.Date()
yesterday <- today - 1

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
source ("R/team_games.R")
source ("R/date_game.R")
source ("R/output_markdown.R")
source ("R/make_box_score.R")
source ("R/pbp_summary.R")
source ("R/pbp_half.R")
source ("R/grouped.R")
source ("R/game_ident.R")
source ("R/write_half_inning_tables.R")

# Get the game IDs for today's and yesterday's games
today_game_id <- game_ident(teamID, today)
yesterday_game_id <- game_ident(teamID, yesterday)

# Create output for all the team's games for the whole season
output_markdown(team_games(teamID, season), "output/Angels/angels_all_season_games.md")

# Get just today's and yesterday's games
output_markdown(date_game(today_game_id, season), "output/Angels/angels_today_game.md")
output_markdown(date_game(yesterday_game_id, season), "output/Angels/angels_yesterday_game.md")

# Create output file for the games' box scores
output_markdown(make_box_score(mlb_game_linescore(game_ident(teamID, today))), "output/Angels/angels_today_box_score.md")
output_markdown(make_box_score(mlb_game_linescore(game_ident(teamID, yesterday))), "output/Angels/angels_yesterday_box_score.md")

# Create output file for the games' play-by-play summaries
write_half_inning_tables(grouped(pbp_half(today_game_id)), "output/Angels/angels_today_play_by_play.md")
write_half_inning_tables(grouped(pbp_half(yesterday_game_id)), "output/Angels/angels_yesterday_play_by_play.md")