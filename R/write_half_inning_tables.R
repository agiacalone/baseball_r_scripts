write_half_inning_tables <- function(grouped_list, file = "half_innings.md") {
  output <- c()
  
  # Defensive: If grouped_list is empty or NULL, write a placeholder
  if (is.null(grouped_list) || length(grouped_list) == 0) {
    output <- c(output, "No half-inning data available.")
  } else {
    for (i in seq_along(grouped_list)) {
      this_group <- grouped_list[[i]]
      
      # Skip if this_group is not a data frame or has no rows
      if (!is.data.frame(this_group) || nrow(this_group) == 0) next
      
      this_inning <- if ("inning" %in% colnames(this_group)) unique(this_group$inning) else "?"
      this_half   <- if ("half" %in% colnames(this_group)) unique(this_group$half) else "?"
      
      output <- c(
        output,
        paste0("\n### Inning ", this_inning, " (", this_half, ")"),
        knitr::kable(this_group, format = "markdown"),
        "\n"
      )
    }
    # If after looping, output is still empty, add placeholder
    if (length(output) == 0) {
      output <- c(output, "No half-inning data available.")
    }
  }
  
  # Make sure the output directory exists
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  
  # Actually write the output to file
  writeLines(output, file)
  cat("Markdown table written to", file, "\n")
}