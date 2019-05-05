#' @import tibble
hectagon_lattice_raw <- function(nx = 5, ny = 5, dist = 1, origin = c(0, 0)) {
  locations <- cbind(
    location = 1:(nx * ny),
    x = sort(c(
      rep(seq(from = 0, by = dist, length.out = nx), each = ceiling(ny / 2)),
      rep(seq(from = dist / 2, by = dist, length.out = nx),
        each = floor(ny / 2)
      )
    )) + origin[1],
    y = rep(c(
      seq(from = 0, by = dist * sqrt(3), length.out = ceiling(ny / 2)),
      seq(
        from = dist * sqrt(3) / 2, by = dist * sqrt(3),
        length.out = floor(ny / 2)
      )
    ) + origin[2], times = nx)
  )
  as_tibble(locations)
}

#' @importFrom dplyr inner_join
#' @export
hectagon_lattice <-function () {
  mapping <- tribble(~location,
                     4,
                     6,
                     7,
                     9,
                     11,
                     10,
                     12,
                     14,
                     16,
                     15,
                     17,
                     19
  ) %>%
    add_column(
      position = 1:(nrow(.)),
      color = factor(sample(rep(0:1, each = nrow(.) /2)))
  )

  hectagon_lattice_raw() %>%
    inner_join(mapping) %>%
    select(-.data$location)
}

