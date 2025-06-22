### Get list of all team IDs

season = 2025

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
source ("R/team_ids.R")
source ("R/output_markdown.R")

# Get all MLB teams and their IDs
teams <- team_ids(season)

output_markdown(teams, "output/team_ids.md")