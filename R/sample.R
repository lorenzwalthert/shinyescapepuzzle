run_until_done <- function(limit = 1e5) {
  state <- hectagon_lattice()
  for (run in rlang::seq2(1, limit)) {
    new_state <- state %>%
      press(sample(c("left", "right", "top"), 1))

    score <- new_state %>%
      score_state()
    if (score == 1) {
      break
    }
    state <- new_state
  }
  # print(plot_state(new_state))
  run
}
