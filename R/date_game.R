# Get one day's game for a team

date_game <- function(game_id, season) {
  # Handle NULL or missing game_id
  if (is.null(game_id) || length(game_id) == 0 || (length(game_id) == 1 && is.na(game_id))) {
    message("No valid game_id provided. Returning empty data frame.")
    return(tibble::tibble())
  }
  mlb_schedule(season) %>%
    filter(game_pk == game_id) %>%
    select(
      game_date,
      game_pk,
      awayScore = teams_away_score,
      homeScore = teams_home_score,
      awayTeamName = teams_away_team_name,
      homeTeamName = teams_home_team_name,
      series_description
    ) %>%
    mutate(game_date = with_tz(ymd_hms(game_date, tz = "UTC"), tzone = "America/Los_Angeles")) %>%
    mutate(
      game_datetime = ymd_hms(game_date),
      date = as.Date(game_datetime),
      time = format(game_datetime, "%H:%M:%S")
    ) %>%
    select(-game_date, -game_datetime) %>%
    select(date, time, everything()) %>%
    arrange(date, time)
}