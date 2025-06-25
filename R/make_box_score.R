# Function to create box score from linescore output

make_box_score <- function(linescore) {
  if (is.null(linescore) || !is.data.frame(linescore) || nrow(linescore) == 0) {
    return(tibble(Inning = character(), Away = character(), Home = character()))
  }
  innings <- linescore$num
  away_runs_by_inning <- as.character(linescore$away_runs)
  home_runs_by_inning <- as.character(linescore$home_runs)
  away_team <- unique(linescore$away_team_name)
  home_team <- unique(linescore$home_team_name)
  # Helper for last non-NA value
  last_non_na <- function(x) tail(x[!is.na(x)], 1)
  # Totals
  away_runs_total <- as.character(sum(as.numeric(linescore$away_runs), na.rm = TRUE))
  home_runs_total <- as.character(sum(as.numeric(linescore$home_runs), na.rm = TRUE))
  away_hits_total <- as.character(sum(as.numeric(linescore$away_hits), na.rm = TRUE))
  home_hits_total <- as.character(sum(as.numeric(linescore$home_hits), na.rm = TRUE))
  away_errors_total <- as.character(sum(as.numeric(linescore$away_errors), na.rm = TRUE))
  home_errors_total <- as.character(sum(as.numeric(linescore$home_errors), na.rm = TRUE))
  
  box_score <- tibble(
    Inning = as.character(innings),
    !!away_team := away_runs_by_inning,
    !!home_team := home_runs_by_inning
  )
  box_score <- bind_rows(
    box_score,
    tibble(
      Inning = "R",
      !!away_team := away_runs_total,
      !!home_team := home_runs_total
    ),
    tibble(
      Inning = "H",
      !!away_team := away_hits_total,
      !!home_team := home_hits_total
    ),
    tibble(
      Inning = "E",
      !!away_team := away_errors_total,
      !!home_team := home_errors_total
    )
  )
  return(box_score)
}