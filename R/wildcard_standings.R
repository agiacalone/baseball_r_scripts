wildcard_standings <- function(season, league_id) {
  mlb_standings(
    season = season,
    league_id = league_id,
    standings_type = "wildCard"
  ) %>%
    select(
      Div_Rank = team_records_division_rank,
      Division = division_id,
      Team = team_records_team_name,
      Wins = team_records_wins,
      Losses = team_records_losses,
      GB = team_records_games_back,
      WLPct = team_records_winning_percentage
    ) %>%
    arrange(as.numeric(Div_Rank), desc(as.numeric(WLPct))) %>%
  mutate(
    Division = case_when(
      Division == 201 ~ "AL Wild",
      Division == 204 ~ "NL Wild",
      TRUE ~ as.character(Division)
    )
  ) %>%
    mutate(OverallRank = row_number()) %>%
    select(OverallRank, everything())
}