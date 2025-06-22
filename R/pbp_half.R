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
    mutate(half = factor(half, levels = c("top", "bottom"))) %>%
    arrange(inning, half, at_bat, pitch_in_ab)
}