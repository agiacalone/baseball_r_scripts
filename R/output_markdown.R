# Markdown output for most non-complex data tables

output_markdown <- function(box_score, file = "box_score.md") {
  # Generate Markdown table as a character vector
  md_table <- knitr::kable(box_score, format = "markdown")
  # Create output directory if it doesn't exist
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  # Write to file
  writeLines(md_table, file)
  cat("Markdown table written to", file, "\n")
}