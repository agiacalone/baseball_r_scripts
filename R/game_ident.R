# Get the game ID for a specific game date and team

game_ident <- function(teamID, gameDate) {
  game_ident <- mlb_schedule(season=season) %>%
    filter((teams_home_team_id == teamID | teams_away_team_id == teamID) & date == gameDate) %>%
    pull(game_pk)
}