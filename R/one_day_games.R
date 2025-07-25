# Get all the games for a specific date

one_day_games <- function(date_ofgame, season) {
  mlb_schedule(season) %>%
    filter(date == as.Date(date_ofgame)) %>%
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