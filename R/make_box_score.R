library(tibble)

make_box_score <- function(teamID, date) {
  gid <- game_ident(teamID, date)
  if (is.null(gid) || length(gid) == 0) {
    message(sprintf("No game found for team %s on %s. Skipping output.", teamID, date))
    # Return an empty tibble (safe default structure)
    return(tibble(Inning = character(), Away = character(), Home = character()))
  }
  linescore <- mlb_game_linescore(gid)
  if (is.null(linescore) || !is.data.frame(linescore) || nrow(linescore) == 0) {
    return(tibble(Inning = character(), Away = character(), Home = character()))
  }
  
  innings <- linescore$num
  away_runs_by_inning <- as.character(linescore$away_runs)
  home_runs_by_inning <- as.character(linescore$home_runs)
  away_team <- unique(linescore$away_team_name)
  home_team <- unique(linescore$home_team_name)
  
  last_non_na <- function(x) tail(x[!is.na(x)], 1)
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