### Variables for the Los Angeles Dodgers

teamID <- 119 # Los Angeles Dodgers

# Source our working R scripts
source ("R/team_games.R")
source ("R/date_game.R")
source ("R/output_markdown.R")
source ("R/make_box_score.R")
source ("R/pbp_summary.R")
source ("R/pbp_half.R")
source ("R/grouped.R")
source ("R/game_ident.R")
source ("R/write_half_inning_tables.R")

# Get the game IDs for today's and yesterday's games
today_game_id <- game_ident(teamID, today)
yesterday_game_id <- game_ident(teamID, yesterday)
tomorrow_game_id <- game_ident(teamID, tomorrow)

# Create output for all the team's games for the whole season
output_markdown(team_games(teamID, season), "output/Dodgers/dodgers_all_season_games.md")

# Get just today's and yesterday's games
output_markdown(date_game(today_game_id, season), "output/Dodgers/dodgers_today_game.md")
output_markdown(date_game(yesterday_game_id, season), "output/Dodgers/dodgers_yesterday_game.md")
output_markdown(date_game(tomorrow_game_id, season), "output/Dodgers/dodgers_tomorrow_game.md")

# Create output file for the games' box scores
output_markdown(make_box_score(teamID, today), "output/Dodgers/dodgers_today_box_score.md")
output_markdown(make_box_score(teamID, yesterday), "output/Dodgers/dodgers_yesterday_box_score.md")

# Create output file for the games' play-by-play summaries
write_half_inning_tables(grouped(pbp_half(today_game_id)), "output/Dodgers/dodgers_today_play_by_play.md")
write_half_inning_tables(grouped(pbp_half(yesterday_game_id)), "output/Dodgers/dodgers_yesterday_play_by_play.md")