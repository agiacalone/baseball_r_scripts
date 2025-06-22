season_batting_totals <- game_logs_df %>%
  group_by(person_id) %>%
  summarise(
    games = n(),
    at_bats = sum(stat_batting_atBats, na.rm=TRUE),
    hits = sum(stat_batting_hits, na.rm=TRUE),
    home_runs = sum(stat_batting_homeRuns, na.rm=TRUE),
    rbi = sum(stat_batting_rbi, na.rm=TRUE),
    avg = ifelse(sum(stat_batting_atBats, na.rm=TRUE) > 0, sum(stat_batting_hits, na.rm=TRUE)/sum(stat_batting_atBats, na.rm=TRUE), NA_real_)
    # ...add more stats as desired
  ) %>%
  left_join(roster, by = "person_id")