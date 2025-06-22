### Get today's games

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
source ("R/date_game.R")
source ("R/one_day_games.R")
source ("R/output_markdown.R")

output_markdown(one_day_games(today, season), "output/today_games.md")