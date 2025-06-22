# Create Markdown output for half-inning tables (pitch-by-pitch)

write_half_inning_tables <- function(grouped_list, file = "half_innings.md") {
  output <- c()
  for (i in seq_along(grouped_list)) {
    this_inning <- unique(grouped_list[[i]]$inning)
    this_half <- unique(grouped_list[[i]]$half)
    output <- c(
      output,
      paste0("\n### Inning ", this_inning, " (", this_half, ")"),
      knitr::kable(grouped_list[[i]], format = "markdown"),
      "\n"
    )
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(output, file)
  cat("Markdown table written to", file, "\n")
}