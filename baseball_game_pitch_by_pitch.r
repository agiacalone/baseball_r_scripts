library(baseballr)
library(dplyr)
library(glue)
library(readr)

# Get the game schedule for the given season
games <- mlb_schedule(season="2016")

# Find the game and create data frames
game_id <- 777522  # replace with your game's gamePk

# Basic info for the game
gameinfo <- mlb_game_info(game_id)

# Get the linescore for the game
linescore <- mlb_game_linescore(game_id)

# Create the full play-by-play data
pbp <- mlb_pbp(game_id)

# Create the summary of the play-by-play game
pbp_summary <- pbp %>%
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

# Select and sort
pbp_half <- pbp %>%
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

# Group by half-inning
grouped <- pbp_half %>%
  group_by(inning, half) %>%
  group_split()

# Compose a nice text recap
lines <- c()
for (half_frame in grouped) {
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

# Write to file
write_lines(lines, "pbp_half_inning_recaps.txt")

# Optional: print first 50 lines in console for preview
cat(paste(lines[1:50], collapse="\n"))