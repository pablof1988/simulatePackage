#' Goodness-of-Fit Test for Uniformity
#'
#' @description
#' Performs a Pearson chi-squared goodness-of-fit test to assess whether a
#' numeric sample follows a continuous uniform distribution on the interval
#' \eqn{[0, 1]}.
#'
#' @param x A numeric vector containing the observations to be tested. Values
#'   are expected to lie in the interval \eqn{[0, 1]}.
#'
#' @details
#' The interval \eqn{[0, 1]} is divided into 10 classes of width 0.1. Under
#' the null hypothesis of uniformity, each class has probability 0.1. The
#' function compares the observed and expected class frequencies using the
#' Pearson chi-squared statistic
#' \deqn{X^2 = \sum_{i=1}^{10} \frac{(n \hat{\pi}_i - n \pi_i)^2}{n \pi_i}.}
#'
#' The null hypothesis is that the observations follow a
#' \eqn{U(0,1)} distribution (\eqn{H_0: \Pi = \hat{\Pi}}). The alternative hypothesis is that they do not (\eqn{H_1: \Pi \neq \hat{\Pi}}).
#' The reference distribution has 9 degrees of freedom. For the chi-squared
#' approximation to be appropriate, the expected frequency in every class
#' should generally be at least 5; because the expected frequency is
#' \eqn{n/10}, a sample size of at least 50 is recommended.
#'
#' @return A named numeric vector with three elements:
#' \describe{
#'   \item{est}{The Pearson chi-squared test statistic.}
#'   \item{df}{The degrees of freedom, equal to 9.}
#'   \item{p-val}{The upper-tail p-value of the test.}
#' }
#'
#' @examples
#' set.seed(123)
#' x <- runif(100)
#' uniftest(x)
#'
#' @seealso [stats::pchisq()] for the chi-squared distribution and
#'   [fdth::fdt()] for the frequency distribution table used by this function.
#'
#' @importFrom fdth fdt
#' @export
uniftest <- function(x){
  n <- length(x)
  pi <- rep(0.1, 10)
  k <- length(pi)
  tfr <- fdth::fdt(x, start = 0, end = 1, h = 0.1)
  pihat <- tfr$table$rf # Estimador de pi
  est <- sum((((n*pihat) - (n*pi))^2) / (n*pi))
  pval <- stats::pchisq(q = est, df = k - 1, lower.tail = F)
  c("est" = est, "df" = k - 1, "p-val" = pval)
}
