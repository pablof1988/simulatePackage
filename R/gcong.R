#' Congruential generator of pseudo-random numbers U(0, 1)
#'
#' @description
#' This functions implements the congruential method to generate pseudo
#' random numbers coming from an uniform distribution U(0, 1)
#'
#' @param n the length of the sample
#' @param seed the initial seed to begin the generation of pseudo random numbers
#' @param a the multiplicative constant
#' @param c the additive constant, by default c = 0, getting a multiplicative generator, otherwise one can get an additive generator
#' @param m the module
#'
#' @return A sequence of length \eqn{n} of pseudo random numbers coming from an uniform distribution U(0, 1)
#'
#' @examples
#' # (a = 9, c = 5, m = 2^20) is an additive generator, with the following properties:
#' # Let be k = 2, a = 1 + 4k = 9
#' # c and m are prime relatives
#' # These properties lets a maximum period 2^20 = 1048576
#' sample <- gcong(n = 10000, seed = 1234,  a = 9, c = 5, m = 2^20)
#' uniftest(sample)
#' independence(sample, h = 100)
#' period(sample)
#'
#' @export
gcong <- function(n, seed, a, c = 0, m){
  replicate(n, {
    seed <<- ((a * seed) + c) %% m
    seed / m
  })
}
