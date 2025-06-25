# Function to retrieve combined standings for all MLB

combined_intraleague_standings <- function(season, league_id) {
  stgs <- mlb_standings(season = season, league_id = league_id)
  bind_rows(stgs) %>%
    select(
      Div_Rank = team_records_division_rank,
      Division = division_id,
      Team = team_records_team_name,
      Wins = team_records_wins,
      Losses = team_records_losses,
      GB = team_records_games_back,
      WLPct = team_records_winning_percentage
    ) %>%
    arrange(desc(as.numeric(WLPct)), as.numeric(Div_Rank)) %>%
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
    mutate(OverallRank = row_number()) %>%
    select(OverallRank, everything())
}