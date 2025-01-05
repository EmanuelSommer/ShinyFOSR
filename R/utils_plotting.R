#' Beautify a plot label
#'
#' @description Basic utility to beautify a plot label by replacing underscores with spaces and capitalizing the first letter of each word.
#'
#' @return beautified label
#'
#' @noRd
beautify_plot_label <- function(label) {
  label <- stringr::str_replace_all(label, "_", " ")
  label <- stringr::str_to_title(label)
  label
}
