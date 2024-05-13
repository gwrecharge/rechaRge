#' KGE computation
#'
#' Compute the Kling-Gupta Efficiency coefficient which summarizes the discrepancy
#' between observed values and the values expected under the model in question.
#'
#' @param sim Simulated values
#' @param obs Observed values
#' @return Kling-Gupta Efficiency between 'sim' and 'obs'
#' @export
#'
#' @examples
#' sim <- c(0.5, 0.5, 10, 15, 0.5, 20, 25, 0.1, 15, 10)
#' obs <- c(1, 0.1, 0.1, 20, 0.6, 30, 20, 0.5, 30, 8)
#' rechaRge::KGE(sim, obs)
KGE <- function(sim, obs) {
  if (is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
      is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))) {
    stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")
  }
  if (length(obs) != length(sim)) {
    stop("Invalid argument: length(sim) != length(obs)")
  }

  # Index of the elements that belongs to both vectors
  vi <- which(!is.na(sim) & !is.na(obs))

  if (length(vi) > 0) {
    # Mean and Standard deviation
    obs <- as.numeric(obs[vi])
    mean.obs <- base::mean(obs, na.rm = TRUE)
    sigma.obs <- stats::sd(obs, na.rm = TRUE)

    # KGE Computation
    KGE <- NA
    if (mean.obs == 0) {
      warning("Warning: mean(obs) == 0 ; Beta = Inf")
    } else if (sigma.obs == 0) {
      warning("Warning: sd(obs) == 0 ; Alpha = Inf")
    } else {
      # Mean and Standard deviation
      sim <- as.numeric(sim[vi])
      mean.sim <- base::mean(sim, na.rm = TRUE)
      sigma.sim <- stats::sd(sim, na.rm = TRUE)

      # Pearson product-moment correlation coefficient
      # * A value of 1 shows that a linear equation describes the relationship
      # perfectly and positively, with all data points lying on the same line
      # and with Y increasing with X.
      # * A score of -1 shows that all data points lie on a single line but
      # that Y increases as X decreases.
      # * A value of 0 shows that a linear model is not needed, i.e., that there
      # is no linear relationship between the variables.
      r <- stats::cor(sim, obs, method = "pearson", use = "pairwise.complete.obs")

      # Alpha is a measure of relative variability between simulated and observed values (See Ref1)
      Alpha <- sigma.sim / sigma.obs

      # Beta is the ratio between the mean of the simulated values to the mean of observations
      Beta <- mean.sim / mean.obs

      KGE <- 1 - sqrt((r - 1) ^ 2 + (Alpha - 1) ^ 2 + (Beta - 1) ^ 2)
    }
  } else {
    warning("There are no pairs of 'sim' and 'obs' without missing values !")
  }

  return(KGE)
}
