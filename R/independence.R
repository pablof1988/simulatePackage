#' This function implements the Ljung-Box test to prove if a sample of pseudorandom numbers U(0, 1) is independent
#'
#' @param u The sample to be tested (numeric vector of pseudorandom numbers U(0, 1))
#' @param h The number of lags to be tested. It must be a positive integer not greater than n - 1, where n is the sample size
#'
#' @return Returns a list with the number of lags, the degrees of freedom, the Ljung-Box statistic Q and the p-value for the independence test
#'
#' @details
#' This function contrast the hypotheses \eqn{H_0: \rho_1 = \rho_2 = \dots = \rho_h = 0} Vs. \eqn{H_1: \exists \rho_i \neq 0}, where \eqn{\rho_k} is the correlation between \eqn{u_i} and \eqn{u_{i-k}}.
#' For each lag \eqn{k = 1, \dots, h} the autocorrelation coefficient
#' \eqn{r_k = \sum_{t=k+1}^{n}(u_t - \bar{u})(u_{t-k} - \bar{u}) / \sum_{t=1}^{n}(u_t - \bar{u})^2}
#' is computed, so h coefficients are obtained. The test statistic is
#' \eqn{Q = n(n+2)\sum_{k=1}^{h} r_k^2 / (n-k)}, which under \eqn{H_0} follows a chi-square distribution with h degrees of freedom.
#'
#' @examples
#' sample <- runif(100)
#' independence(sample, 10)
#'
#' @importFrom stats pchisq
#'
#' @export

independence <- function(u, h){
  n <- length(u)
  d <- u-mean(u)
  r_k <- sapply(1:h,function(k){sum(d[(k+1):n] * d[1:(n-k)])/sum(d^2)})
  Q <- n*(n+2)*sum(r_k^2/(n-(1:h)))
  p_valor <- 1 - pchisq(Q, h)
  return(c("Est"=Q,  "p_val"=p_valor, "df"=h))
}
