# Get team names and IDs

team_ids <- function(season) {
  mlb_teams(season=season) %>%
    filter(league_id == 103 | league_id == 104) %>%
    select(
      team_id, 
      team_abbreviation, 
      teamname = team_full_name, 
      league_id
    ) %>%
    mutate(teamname = str_to_title(teamname))
}