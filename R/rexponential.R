#' Generate exponential random numbers
#'
#' @description
#' Generates random numbers from an exponential distribution with rate
#' parameter \code{lambda}.
#'
#' @param n A positive integer giving the number of random values to generate.
#' @param lambda A positive numeric value specifying the rate parameter of the
#'   exponential distribution.
#'
#' @return A numeric vector of length \code{n} containing random values from
#'   the exponential distribution.
#'
#' @details
#' The exponential distribution with rate \eqn{\lambda} has density
#' \deqn{f(x) = \lambda e^{-\lambda x}, \quad x \ge 0.}
#'
#' Random values are obtained by applying the inverse-transform method to
#' uniform random numbers.
#'
#' @examples
#' x <- rexponential(100, lambda = 2)
#' hist(x)
#'
#' @seealso \code{\link[stats]{rexp}}, \code{\link[stats]{Exponential}}
#'
#' @export
rexponential <- function(n, lambda){
  u <- runif(n)
  -2 * log(1 - u)
}
