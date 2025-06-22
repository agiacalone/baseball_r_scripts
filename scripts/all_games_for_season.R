### Get all the games for a specific season

source ("R/season_games.R")

# Get the whole game schedule for the given season
output_markdown(season_games(season), "output/all_season_games.md")