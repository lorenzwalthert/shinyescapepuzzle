score_state <- function(state) {
  center <- state %>%
    dplyr::filter(.data$position %in% c(3, 4, 5, 7, 8, 11)) %>%
    dplyr::pull(.data$color)
  ifelse(all(center == 1), 1, 0)
}
