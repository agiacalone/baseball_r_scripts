pbp_summary <- function(game_ident) {
  # Safely fetch play-by-play data
  pbp <- tryCatch(
    mlb_pbp(game_ident),
    error = function(e) {
      message("Error fetching play-by-play data: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(pbp) || length(pbp) == 0) {
    return(tibble(
      at_bat = integer(),
      pitch_in_ab = integer(),
      inning = integer(),
      half = factor(levels = c("top", "bottom")),
      pitchnum = integer(),
      awayscore = integer(),
      homescore = integer(),
      outs = integer(),
      detail = character(),
      batter = character(),
      pitcher = character(),
      description = character()
    ))
  }
  
  # Return an empty tibble with correct columns if no data
  if (is.null(pbp) || nrow(pbp) == 0) {
    return(tibble(
      at_bat = integer(),
      pitch_in_ab = integer(),
      inning = integer(),
      half = factor(levels = c("top", "bottom")),
      pitchnum = integer(),
      awayscore = integer(),
      homescore = integer(),
      outs = integer(),
      detail = character(),
      batter = character(),
      pitcher = character(),
      description = character()
    ))
  }
  
  # Check for required columns
  required_cols <- c(
    "about.atBatIndex", "index", "about.inning", "about.halfInning",
    "pitchNumber", "details.awayScore", "details.homeScore", "count.outs.start",
    "details.code", "matchup.batter.fullName", "matchup.pitcher.fullName",
    "details.description"
  )
  missing_cols <- setdiff(required_cols, colnames(pbp))
  if (length(missing_cols) > 0) {
    warning("Missing required columns in play-by-play data: ", paste(missing_cols, collapse=", "))
    return(tibble(
      at_bat = integer(),
      pitch_in_ab = integer(),
      inning = integer(),
      half = factor(levels = c("top", "bottom")),
      pitchnum = integer(),
      awayscore = integer(),
      homescore = integer(),
      outs = integer(),
      detail = character(),
      batter = character(),
      pitcher = character(),
      description = character()
    ))
  }
  
  # All good? Proceed!
  pbp %>%
    select(
      at_bat = about.atBatIndex,
      pitch_in_ab = index,
      inning = about.inning,
      half = about.halfInning,
      pitchnum = pitchNumber,
      awayscore = details.awayScore,
      homescore = details.homeScore,
      outs = count.outs.start,
      detail = details.code,
      batter = matchup.batter.fullName,
      pitcher = matchup.pitcher.fullName,
      description = details.description
    ) %>%
    mutate(half = factor(half, levels = c("top", "bottom"))) %>%
    arrange(at_bat, pitch_in_ab)
}