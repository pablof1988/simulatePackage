#' @title Period of a sequence
#' @description Counts the number of distinct values in a sequence
#'   before it starts repeating. Useful for estimating the period of a
#'   pseudorandom number generator.
#' @param x a vector with the generated sequence.
#' @return An integer: the number of unique values in \code{x}.
#' @examples
#' x <- c(3, 7, 1, 3, 7, 1, 3, 7, 1)
#' period(x)
#' @export
period <- function(x){
  sum(!duplicated(x))
}
