text_recap <- function(pbp_data) {
  lines <- c()
  
  # Defensive: bail out early if pbp_data is bad or empty
  if (is.null(pbp_data) || !is.data.frame(pbp_data) || nrow(pbp_data) == 0) {
    lines <- c(lines, "No play-by-play data available for this game.")
    return(lines)
  }
  
  half_groups <- tryCatch(
    grouped(pbp_data),
    error = function(e) {
      message("Error grouping play-by-play data: ", e$message)
      return(list())
    }
  )
  
  # If no groups, return a message
  if (length(half_groups) == 0) {
    lines <- c(lines, "No play-by-play data available for this game.")
    return(lines)
  }
  
  for (half_frame in half_groups) {
    # Defensive: skip if the frame is empty
    if (nrow(half_frame) == 0) next
    
    this_inning <- if ("inning" %in% colnames(half_frame)) half_frame$inning[1] else NA
    this_half   <- if ("half"   %in% colnames(half_frame)) half_frame$half[1]   else NA
    header <- glue::glue("\n--- {stringr::str_to_title(this_half)} of Inning {this_inning} ---\n")
    lines <- c(lines, header)
    
    for (i in seq_len(nrow(half_frame))) {
      play <- half_frame[i, ]
      batter     <- if ("batter"     %in% colnames(play)) play$batter     else "Unknown"
      pitcher    <- if ("pitcher"    %in% colnames(play)) play$pitcher    else "Unknown"
      description<- if ("description"%in% colnames(play)) play$description else "No description"
      play_line <- glue::glue(
        "Batter: {batter} | Pitcher: {pitcher}\n  {description}"
      )
      lines <- c(lines, play_line)
    }
  }
  return(lines)
}