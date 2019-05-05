#' @import ggplot2
#' @export
plot_state <- function(state = hectagon_lattice()) {
  state %>%
    gg_base() +
    geom_point(size = 6)
}

plot_skeleton <- function(state = hectagon_lattice()) {
  state %>%
    gg_base() +
    geom_text()
}

gg_base <- function(state) {
  state %>%
    ggplot(aes(
    x = .data$x, y = .data$y, label = .data$position,
    color = .data$color
  )) + scale_color_manual(values = c("0" = "red", "1" = "green")) +
    theme_void() + theme(legend.position = "")
}
