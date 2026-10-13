#' Generate Pseudorandom Numbers with the Middle-Square Method
#'
#' @description
#' Generates a sequence of pseudorandom numbers using the middle-square
#' method. Starting from a four-digit seed, the method squares the current
#' seed, selects the four middle digits of the result, and uses them as the
#' seed for the next iteration.
#'
#' @param n A positive integer indicating the number of pseudorandom numbers
#'   to generate.
#' @param seed A four-digit numeric seed used to initialize the generator.
#'
#' @details
#' Let \eqn{x_i} denote the four-digit seed at iteration \eqn{i}. The next
#' seed, \eqn{x_{i+1}}, is obtained by squaring \eqn{x_i} and extracting the
#' four middle digits of the square. The value returned at that iteration is
#' \deqn{u_{i+1} = \frac{x_{i+1}}{10^4}.}
#'
#' When the square has seven digits, it is treated as if it had a leading
#' zero before its middle digits are extracted. Consequently, a generated
#' seed can contain leading zeros even though it is stored as a numeric
#' value.
#'
#' The middle-square method is deterministic: the same seed and the same
#' value of \code{n} always produce the same sequence. It can also enter a
#' short cycle or reach zero, so it is mainly useful for teaching and for
#' illustrating the limitations of early pseudorandom number generators.
#' The generated values lie on a finite grid in the interval \eqn{[0,1)} and
#' should not be interpreted as independent observations from an exact
#' continuous uniform distribution.
#'
#' @return A numeric vector of length \code{n} containing the generated
#'   pseudorandom numbers, scaled by \eqn{10^4} to lie in \eqn{[0,1)}.
#'
#' @references
#' von Neumann, J. (1951). Various techniques used in connection with random
#' digits. In \emph{Monte Carlo Method}, National Bureau of Standards Applied
#' Mathematics Series 12, 36--38.
#'
#' @examples
#' middlesquare(n = 6, seed = 5735)
#'
#' # The generator is deterministic.
#' identical(
#'   middlesquare(n = 4, seed = 5735),
#'   middlesquare(n = 4, seed = 5735)
#' )
#'
#' # Functions in this package can be used to examine the sequence.
#' u <- middlesquare(n = 20, seed = 5735)
#' period(u)
#'
#' @seealso
#' [period()] for counting distinct values before repetition,
#' [uniftest()] for assessing uniformity, and [independence()] for assessing
#' serial independence.
#'
#' @export
middlesquare <- function(n, seed){
  stopifnot("seed must contain exactly 4 digits" = nchar(seed) == 4)
  replicate(n,{
    square <- format(seed^2, scientific = F)
    d2 <- nchar(square)
    x0 <- if(d2 == 7){
      substr(square, 2, 5)
    } else{
      substr(square, 3, 6)
    }
    seed <<- as.numeric(x0)

    seed / 10^4
  })
}

