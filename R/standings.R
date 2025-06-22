# Function to retrieve league standings for all MLB

standings <- function(season, type, league_id) {
  mlb_standings(season = season, league_id = league_id) %>%
    select(
      standings_type,
      Rank = team_records_division_rank,
      Division = division_id,
      Team = team_records_team_name,
      Wins = team_records_wins,
      Losses = team_records_losses,
      GB = team_records_games_back,
      WLPct = team_records_winning_percentage
    ) %>%
    mutate(
      Division = case_when(
        Division == 200 ~ "AL West",
        Division == 201 ~ "AL East",
        Division == 202 ~ "AL Central",
        Division == 203 ~ "NL West",
        Division == 204 ~ "NL East",
        Division == 205 ~ "NL Central",
        TRUE ~ as.character(Division)
      )
    ) %>%
    arrange(Division, as.numeric(Rank), desc(as.numeric(WLPct)))
}