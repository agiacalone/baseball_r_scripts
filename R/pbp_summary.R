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