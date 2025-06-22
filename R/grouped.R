# Group by half-inning

grouped <- function(pbp_data) {
  pbp_data %>%
    group_by(inning, half) %>%
    group_split()
}