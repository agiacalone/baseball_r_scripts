grouped <- function(pbp_data) {
  # Defensive: Handle NULL or non-data.frame input
  if (is.null(pbp_data) || !is.data.frame(pbp_data) || nrow(pbp_data) == 0) {
    return(list())
  }
  
  # Make sure required columns exist
  required_cols <- c("inning", "half")
  missing_cols <- setdiff(required_cols, colnames(pbp_data))
  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(list())
  }
  
  # All good! Proceed to group
  pbp_data %>%
    group_by(inning, half) %>%
    group_split()
}