
#' Convert a Cyclical Variable to Sine Representation
#'
#' This function transforms a cyclic variable (e.g., hour of the day, day of the week, month of the year)
#' into its sine representation. This transformation ensures that machine learning models respect
#' the cyclical nature of such features.
#'
#' @param x A numeric vector representing a cyclic variable (e.g., hour, month, day).
#' @param period A positive numeric scalar representing the period of the cycle.
#'   For example, use 24 for hours of the day, 7 for days of the week, and 12 for months.
#'
#' @return A numeric vector representing the sine-transformed values.
#'
#' @details This function applies the transformation:
#'   \deqn{sin(2 * \pi * x / period)}
#'   This encoding ensures that the first and last values in the cycle are smoothly connected.
#'
#' @examples
#' # Convert hours of the day to sine encoding
#' hours <- 0:23
#' cyclical_sin(hours, 24)
#'
#' # Convert months of the year to sine encoding
#' months <- 1:12
#' cyclical_sin(months, 12)
#'
#' @export
cyclical_sin <- function(x, period) {
  if (!is.numeric(x)) {
    stop("'x' must be numeric.")
  }
  if (!is.numeric(period) || length(period) != 1L || period <= 0) {
    stop("'period' must be a positive numeric scalar.")
  }
  return(sin(2 * pi * x / period))
}

#' Convert a Cyclical Variable to Cosine Representation
#'
#' This function transforms a cyclic variable (e.g., hour of the day, day of the week, month of the year)
#' into its cosine representation. This transformation ensures that machine learning models respect
#' the cyclical nature of such features.
#'
#' @param x A numeric vector representing a cyclic variable (e.g., hour, month, day).
#' @param period A positive numeric scalar representing the period of the cycle.
#'   For example, use 24 for hours of the day, 7 for days of the week, and 12 for months.
#'
#' @return A numeric vector representing the cosine-transformed values.
#'
#' @details This function applies the transformation:
#'   \deqn{cos(2 * \pi * x / period)}
#'   This encoding ensures that the first and last values in the cycle are smoothly connected.
#'
#' @examples
#' # Convert hours of the day to cosine encoding
#' hours <- 0:23
#' cyclical_cos(hours, 24)
#'
#' # Convert months of the year to cosine encoding
#' months <- 1:12
#' cyclical_cos(months, 12)
#'
#' @export
cyclical_cos <- function(x, period) {
  if (!is.numeric(x)) {
    stop("'x' must be numeric.")
  }
  if (!is.numeric(period) || length(period) != 1L || period <= 0) {
    stop("'period' must be a positive numeric scalar.")
  }
  return(cos(2 * pi * x / period))
}
