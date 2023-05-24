#' This function plots the change in the Bayes Factor under different possible priors
#'
#' @title Bayes Factor Robustness Check
#' @author Roy Michael Moore, \email{roy.moore@@student.uva.nl}
#'
#' @import BayesFactor
#'
#' @param x a vector of observations for the first (or only) group
#' @param y a vector of observations for the second group (or condition, for paired)
#' @param paired if TRUE, observations are paired
#' @param max_r maximum value of the scale parameter of the Cauchy prior to be plotted
#' @param iterations will determine how precise the plot will be

#' @export
robustness_check <- function (
  x,
  y,
  paired = FALSE,
  max_r = 2,
  iterations = 200
) {

  r <- max_r/iterations
  counter <- 1
  res <- data.frame(matrix(, nrow = iterations, ncol = 2, dimnames = list(NULL, c("r", "BF"))))

  while(r < max_r + max_r/iterations) {
    res[counter, ] <-
      c(r, extractBF(ttestBF(x = x,
                             y = y,
                             paired = paired,
                             rscale = r))[[1]])
    counter <- counter + 1
    r <- r + max_r/iterations
  }

  plot(
    x = res[, 1],
    y = res[, 2],
    type = "l",
    log = "y",
    main = "Bayes Factor Robustness Check",
    xlab = "Prior Scale",
    ylab = "BF")

  return(res)
}
