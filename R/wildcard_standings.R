wildcard_standings <- function(season, league_id) {
  mlb_standings(
    season = season,
    league_id = league_id,
    standings_type = "wildCard"
  ) %>%
    select(
      Rank = team_records_league_rank,
      Division = division_id,
      Team = team_records_team_name,
      Wins = team_records_wins,
      Losses = team_records_losses,
      GB = team_records_games_back,
      WLPct = team_records_winning_percentage
    ) %>%
    arrange(as.numeric(Rank), desc(as.numeric(WLPct)))
}