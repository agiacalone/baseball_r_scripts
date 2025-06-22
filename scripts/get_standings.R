### Get the current standings for MLB

# Load the required libraries
library(baseballr)
library(retrosheet)
library(dplyr)
library(tidyverse)
library(glue)
library(tibble)
library(readr)
library(knitr)
library(lubridate)

# Source our working R scripts
source ("R/combined_standings.R")
source ("R/standings.R")
source ("R/output_markdown.R")

# Get the combined and individual standings for the American and National Leagues
output_markdown(combined_standings(season), "output/combined_standings.md")
output_markdown(standings(season, league_id = 103), "output/al_standings.md")
output_markdown(standings(season, league_id = 104), "output/nl_standings.md")