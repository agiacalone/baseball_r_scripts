### Get tomorrow's games

# Source our working R scripts
source ("R/date_game.R")
source ("R/one_day_games.R")
source ("R/output_markdown.R")

output_markdown(one_day_games(tomorrow, season), "output/tomorrow_games.md")