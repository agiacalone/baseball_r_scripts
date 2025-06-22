# Compose a nice text recap

text_recap <- function(pbp_data) {
  lines <- c()
  for (half_frame in grouped(pbp_data)) {
    this_inning <- half_frame$inning[1]
    this_half <- half_frame$half[1]
    header <- glue::glue("\n--- {stringr::str_to_title(this_half)} of Inning {this_inning} ---\n")
    lines <- c(lines, header)
    
    for (i in seq_len(nrow(half_frame))) {
      play <- half_frame[i, ]
      play_line <- glue::glue(
        "Batter: {play$batter} | Pitcher: {play$pitcher}\n  {play$description}"
      )
      lines <- c(lines, play_line)
    }
  }
  return(lines)
}