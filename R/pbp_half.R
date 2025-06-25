pbp_half <- function(game_ident) {
  # Safely attempt to get the play-by-play data
  pbp <- tryCatch(
    mlb_pbp(game_ident),
    error = function(e) {
      message("Error fetching play-by-play data: ", e$message)
      return(NULL)
    }
  )
  
  # If pbp is NULL or has zero rows, return a blank tibble with correct columns
  if (is.null(pbp) || nrow(pbp) == 0) {
    return(tibble(
      inning = integer(),
      half = factor(levels = c("top", "bottom")),
      at_bat = integer(),
      pitch_in_ab = integer(),
      awayscore = integer(),
      homescore = integer(),
      outs = integer(),
      batter = character(),
      pitcher = character(),
      description = character()
    ))
  }
  
  # Check for all required columns before proceeding
  required_cols <- c(
    "about.inning", "about.halfInning", "about.atBatIndex", "index",
    "details.awayScore", "details.homeScore", "count.outs.start",
    "matchup.batter.fullName", "matchup.pitcher.fullName", "details.description"
  )
  missing_cols <- setdiff(required_cols, colnames(pbp))
  if (length(missing_cols) > 0) {
    warning("Missing required columns in play-by-play data: ", paste(missing_cols, collapse=", "))
    return(tibble(
      inning = integer(),
      half = factor(levels = c("top", "bottom")),
      at_bat = integer(),
      pitch_in_ab = integer(),
      awayscore = integer(),
      homescore = integer(),
      outs = integer(),
      batter = character(),
      pitcher = character(),
      description = character()
    ))
  }
  
  # All good: proceed as usual
  pbp %>%
    select(
      inning = about.inning,
      half = about.halfInning,
      at_bat = about.atBatIndex,
      pitch_in_ab = index,
      awayscore = details.awayScore,
      homescore = details.homeScore,
      outs = count.outs.start,
      batter = matchup.batter.fullName,
      pitcher = matchup.pitcher.fullName,
      description = details.description
    ) %>%
    mutate(half = factor(half, levels = c("top", "bottom"))) %>%
    arrange(inning, half, at_bat, pitch_in_ab)
}