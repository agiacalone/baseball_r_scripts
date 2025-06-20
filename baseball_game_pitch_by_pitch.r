library(baseballr)
library(retrosheet)
library(dplyr)
library(tidyverse)
library(glue)
library(tibble)
library(readr)
library(knitr)

#### BEGIN VARIABLE SECTION ####

season <- 2025
gameDate <- "2025-06-19"
giantsID <- 137  # San Francisco Giants
angelsID <- 108  # Los Angeles Angels
dodgersID <- 119  # Los Angeles Dodgers
metsID <- 121  # New York Mets
padresID <- 135  # San Diego Padres

#### END VARIABLE SECTION ####

#### BEGIN FUNCTIONS SECTION ####

# Filtered games for a whole season
season_games <- function(season) {
  mlb_schedule(season=season) %>%
    select(
      date,
      game_pk,
      awayScore = teams_away_score,
      homeScore = teams_home_score,
      awayTeamName = teams_away_team_name,
      away_id = teams_away_team_id,
      homeTeamName = teams_home_team_name,
      home_id = teams_home_team_id,
      series_description
    ) %>%
    arrange(date)
}

# Get all games for a season by a team
team_games <- function(teamID, season) {
  mlb_schedule(season=season) %>%
    filter(teams_home_team_id == teamID | teams_away_team_id == teamID) %>%
    select(
      date,
      game_pk,
      awayScore = teams_away_score,
      homeScore = teams_home_score,
      awayTeamName = teams_away_team_name,
      away_id = teams_away_team_id,
      homeTeamName = teams_home_team_name,
      home_id = teams_home_team_id,
      series_description
    ) %>%
    arrange(date)
}

# Get all the games for a specific date
date_games <- function(gameDate, season) {
  mlb_schedule(season=season) %>%
    filter(date == gameDate) %>%
    select(
      date,
      game_pk,
      awayScore = teams_away_score,
      homeScore = teams_home_score,
      awayTeamName = teams_away_team_name,
      away_id = teams_away_team_id,
      homeTeamName = teams_home_team_name,
      home_id = teams_home_team_id,
      series_description
    ) %>%
    arrange(date)
}

# Function to retrieve standings for all MLB
standings <- function(season, league_id) {
  mlb_standings(season = season, league_id = league_id) %>%
    select(
      Rank = team_records_division_rank,
      Division = division_id,
      Team = team_records_team_name,
      Wins = team_records_wins,
      Losses = team_records_losses,
      GB = team_records_games_back,
      WLPct = team_records_winning_percentage
    ) %>%
    arrange(Division, as.numeric(Rank))
}

# Get the game ID for a specific game date and team
game_ident <- function(teamID, gameDate) {
  game_ident <- mlb_schedule(season=season) %>%
  filter((teams_home_team_id == teamID | teams_away_team_id == teamID) & date == gameDate) %>%
  pull(game_pk)
  }

# Basic info for the game
gameinfo <- function(game_ident) {
  mlb_game_info(game_ident)
}

# Function to create box score
make_box_score <- function(linescore) {
  innings <- linescore$num
  away_runs_by_inning <- as.character(linescore$away_runs)
  home_runs_by_inning <- as.character(linescore$home_runs)
  away_team <- unique(linescore$away_team_name)
  home_team <- unique(linescore$home_team_name)
  totals <- linescore %>% filter(num == max(num))
  away_runs_total <- as.character(sum(as.numeric(linescore$away_runs)))
  home_runs_total <- as.character(sum(as.numeric(linescore$home_runs)))
  
  box_score <- tibble(
    Inning = as.character(innings),
    !!away_team := away_runs_by_inning,
    !!home_team := home_runs_by_inning
  )
  box_score <- bind_rows(
    box_score,
    tibble(
      Inning = "R",
      !!away_team := away_runs_total,
      !!home_team := home_runs_total
    ),
    tibble(
      Inning = "H",
      !!away_team := as.character(totals$away_hits),
      !!home_team := as.character(totals$home_hits)
    ),
    tibble(
      Inning = "E",
      !!away_team := as.character(totals$away_errors),
      !!home_team := as.character(totals$home_errors)
    )
  )
  return(box_score)
}

# Create the summary of the play-by-play game
pbp_summary <- function(game_ident) {
  mlb_pbp(game_ident) %>%
    select(
      at_bat = about.atBatIndex,
      pitch_in_ab = index,
      inning = about.inning,
      half = about.halfInning,
      pitchnum = pitchNumber,
      awayscore = details.awayScore,
      homescore = details.homeScore,
      outs = count.outs.start,
      detail = details.code,
      batter = matchup.batter.fullName,
      pitcher = matchup.pitcher.fullName,
      description = details.description
    ) %>%
    arrange(at_bat, pitch_in_ab)
}

# Select and sort
pbp_half <- function(game_ident) {
  mlb_pbp(game_ident) %>%
    select(
      inning = about.inning,
      half = about.halfInning,
      at_bat = about.atBatIndex,
      pitch_in_ab = index,
      awayscore = details.awayScore,
      homescore = details.homeScore,
      outs = count.outs.start,
      batter = matchup.batter.fullName,
      pitcher = matchup.pitcher.fullName,
      description = details.description
    ) %>%
    arrange(at_bat, pitch_in_ab, inning, half)
}

# Group by half-inning
grouped <- function(pbp_data) {
  pbp_data %>%
    group_by(inning, half) %>%
    group_split()
}

# Compose a nice text recap
text_recap <- function(pbp_data) {
  lines <- c()
  for (half_frame in grouped(pbp_data)) {
    this_inning <- half_frame$inning[1]
    this_half <- half_frame$half[1]
    header <- glue::glue("\n--- {stringr::str_to_title(this_half)} of Inning {this_inning} ---\n")
    lines <- c(lines, header)
    
    for (i in seq_len(nrow(half_frame))) {
      play <- half_frame[i, ]
      play_line <- glue::glue(
        "Batter: {play$batter} | Pitcher: {play$pitcher}\n  {play$description}"
      )
      lines <- c(lines, play_line)
    }
  }
  return(lines)
}

# Markdown output for most non-complex data tables
output_markdown <- function(box_score, file = "box_score.md") {
  # Generate Markdown table as a character vector
  md_table <- knitr::kable(box_score, format = "markdown")
  # Write to file
  writeLines(md_table, file)
  cat("Markdown table written to", file, "\n")
}

# Create Markdown output for half-inning tables (pitch-by-pitch)
write_half_inning_tables <- function(grouped_list, file = "half_innings.md") {
  output <- c()
  for (i in seq_along(grouped_list)) {
    this_inning <- unique(grouped_list[[i]]$inning)
    this_half <- unique(grouped_list[[i]]$half)
    output <- c(
      output,
      paste0("\n### Inning ", this_inning, " (", this_half, ")"),
      knitr::kable(grouped_list[[i]], format = "markdown"),
      "\n"
    )
  }
  writeLines(output, file)
  cat("Wrote Markdown tables to", file, "\n")
}

### END FUNCTIONS SECTION ###

### BEGIN MAIN EXECUTION SECTION ###

# Get the whole game schedule for the given season
all_season_games <- season_games(season)

# Get the games for a specific team for the whole season
giants_games <- team_games(giantsID, season)
angels_games <- team_games(angelsID, season)

# Get the games for today's date
date_games <- date_games(gameDate, season)

# Get the standings for the American and National Leagues
al_standings <- standings(season, league_id = 103)
nl_standings <- standings(season, league_id = 104)

# Set the game ID for the SF Giants and LA Angels
giants_game_id <- game_ident(giantsID, gameDate)
angels_game_id <- game_ident(angelsID, gameDate)

# Get info for Giants
giants_game_info <- gameinfo(giants_game_id)
angels_game_info <- gameinfo(angels_game_id)

# Get the linescores to create the box scores
giants_linescore <- mlb_game_linescore(giants_game_id)
angels_linescore <- mlb_game_linescore(angels_game_id)

# Make box score
giants_boxscore <- make_box_score(giants_linescore)
angels_boxscore <- make_box_score(angels_linescore)

# Get the play-by-play data
pbp_summary_giants <- pbp_summary(giants_game_id)
pbp_summary_angels <- pbp_summary(angels_game_id)

# Half-inning play-by-play
pbp_half_giants <- pbp_half(giants_game_id)
pbp_half_angels <- pbp_half(angels_game_id)

# Group by team and half-inning
print(giants_grouped <- grouped(pbp_half_giants))
print(angels_grouped <- grouped(pbp_half_angels))

# Create some pretty Markdown outputs
output_markdown(date_games, "date_games.md")
output_markdown(giants_games, "giants_games.md")
output_markdown(angels_games, "angels_games.md")
output_markdown(al_standings, "al_standings.md")
output_markdown(nl_standings, "nl_standings.md")
output_markdown(giants_boxscore, "giants_box_score.md")
output_markdown(angels_boxscore, "angels_box_score.md")
write_half_inning_tables(giants_grouped, "giants_play_by_play.md")
write_half_inning_tables(angels_grouped, "angels_play_by_play.md")

# Create the text recaps for games
#print(text_recap_giants <- text_recap(pbp_half_giants))
#print(text_recap_angels <- text_recap(pbp_half_angels))

### END MAIN EXECUTION SECTION ###
