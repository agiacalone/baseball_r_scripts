### Get the current standings for MLB

# Source our working R scripts
source ("R/combined_standings.R")
source ("R/standings.R")
source ("R/wildcard_standings.R")
source ("R/output_markdown.R")

# Get the combined and individual standings for the American and National Leagues
output_markdown(combined_standings(season), "output/Standings/combined_standings.md")
output_markdown(standings(season, league_id = 103), "output/Standings/al_standings.md")
output_markdown(standings(season, league_id = 104), "output/Standings/nl_standings.md")
output_markdown(wildcard_standings(season, league_id = 103), "output/Standings/al_wc_standings.md")
output_markdown(wildcard_standings(season, league_id = 104), "output/Standings/nl_wc_standings.md")