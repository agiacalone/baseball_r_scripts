# Run the scripts from here

season = 2025
today <- Sys.Date()
yesterday <- today - 1

source ("scripts/all_games_for_season.R")
source ("scripts/get_team_ids.R")
source ("scripts/get_standings.R")
source ("scripts/get_all_games_today.R")
source ("scripts/get_all_games_yesterday.R")
source ("scripts/giants.R")
source ("scripts/angels.R")
source ("scripts/padres.R")
source ("scripts/dodgers.R")