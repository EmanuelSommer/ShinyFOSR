#' Compress a single refund model object
#'
#' @description Memory heavy model objects can be compressed by removing
#'  unnecessary fields. This function removes the following fields from a
#'  refund model object: model, residuals, fitted.values, linear.predictors,
#'  weights, working.weights, prior.weights, y, hat, and offset.
#'  The object size before and after compression is printed to the console.
#'
#' @return The compressed refund model object.
#'
#' @noRd
 compress_single_model <- function(model) {
  cat("Object size before compression: ", object.size(model), "\n")
  model$model <- NULL
  model$residuals <- NULL
  model$fitted.values <- NULL
  model$linear.predictors <- NULL
  model$weights <- NULL
  model$working.weights <- NULL
  model$prior.weights <- NULL
  model$y <- NULL
  model$hat <- NULL
  model$offset <- NULL
  cat("Object size after compression: ", object.size(model), "\n")
  model
 }
