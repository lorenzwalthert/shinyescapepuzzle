#' Goes clockwise.
#' @importFrom dplyr left_join select mutate .data
press_botton <- function(state, mapping) {
  joined <- state %>%
    left_join(mapping, by = c("position" = "old_position")) %>%
    mutate(new_position = ifelse(is.na(.data$new_position), .data$position, .data$new_position)) %>%
    select(.data$color, .data$new_position)
  state %>%
    select(-.data$color) %>%
    left_join(joined, by = c("position" = "new_position"))
}

circular_lag <- function(vec) {
  # c(vec[length(vec)], vec[rlang::seq2(1, length(vec) - 1)])
  c(vec[-1], vec[1])
}

mapping_from_vec <- function(vec) {
  tibble(
    old_position = vec,
    new_position = circular_lag(vec)
  )
}


press_left <- purrr::partial(press_botton,
                             mapping = mapping_from_vec(c(1, 3, 7, 8, 5, 2))
)

press_top <- purrr::partial(press_botton,
                            mapping = mapping_from_vec(c(3, 6, 10, 11, 8, 4))
)

press_right <- purrr::partial(press_botton,
                              mapping = mapping_from_vec(c(4, 7, 11, 12, 9, 5))
)

#' @export
press <- function(state, direction) {
  do.call(paste0("press_", direction), lst(state))
}
