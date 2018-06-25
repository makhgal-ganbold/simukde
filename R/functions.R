## ==================================================================

##
## Internal Fuctions
##

## ==================================================================

#' @title Some functions for a given distribution
#'
#' @description Provides the auxiliary probability density function of Accept Reject Method and a random number generator.
#'
#' @param dname character, auxiliary distribution name.
#' @return list of the density function and a random number generator for the given distribution.
#'
#' @noRd

get_aux_distr <- function (dname) {

  switch (
    dname,
    "mvnorm" = {
      density <- function (x, params) mvtnorm::dmvnorm(x, mean = params$mean, sigma = params$sigma)
      random <- function (n, params) mvtnorm::rmvnorm(n, mean = params$mean, sigma = params$sigma)
    },
    "norm" = {
      density <- function (x, params) stats::dnorm(x, mean = params$mean, sd = params$sd)
      random <- function (n, params) stats::rnorm(n, mean = params$mean, sd = params$sd)
    },
    "exp" = {
      density <- function (x, params) stats::dexp(x, rate = params$rate)
      random <- function (n, params) stats::rexp(n, rate = params$rate)
    },
    "unif" = {
      density <- function (x, params) stats::dunif(x, min = params$min, max = params$max)
      random <- function (n, params) stats::runif(n, min = params$min, max = params$max)
    }
  )
  list("density" = density, "random" = random)

}

#' @title The constant of Accept Reject Method
#'
#' @description Returns the constant of Accept Reject Method for given kernel density estimation, auxiliary distribution and its parameters.
#'
#' @param kd kernel density estimation, a value of the function \code{\link[ks]{kde}}.
#' @param ddistr function, an auxiliary distribution density function from \code{\link{get_aux_distr}}.
#' @return numeric, the constant of Accept Reject Method.
#'
#' @noRd

est_const <- function (kd, ddistr, params) {

  ## kernel density estimation (KDE)

  if (class(kd) != "kde") {
    stop("kd is invlalid")
  }

  ## auxiliary distribution

  if (class(ddistr) != "function") {
    stop("distr is invlalid")
  }

  ## evaluated points by using KDE

  eval.points <- as.matrix(expand.grid(kd$eval.points))

  ## auxiliary distribution density

  density <- ddistr(x = eval.points, params)

  ## constant of accept reject method

  max(kd$estimate / density)

}

## ==================================================================

##
## Main Fuctions
##

## ==================================================================

#' @title Simulation with Kernel Density Estimation
#'
#' @description Generates random values from a univariate and multivariate continuous distribution by using kernel density estimation based on a sample. The function uses Accept Reject Method.
#'
#' @param x a numeric vector, matrix or data frame; data.
#' @param n integer; the number of random values will be generated.
#' @param distr character; auxiliary distribution name. See details.
#' @param const.only logical; if \code{TRUE}, the constant of Accept Reject Method will be returned.
#' @param seed a single value, interpreted as an integer, or \code{NULL} (default).
#' @param parallel logical; if \code{TRUE} (default) parallel generator will be worked.
#' @param ... other parameters for functions \code{\link[ks]{kde}}.
#' @return list of given data, simulated values, kernel density estimation and the constant of Accept Reject Method when \code{const.only} is \code{FALSE} (default).
#' @details Such function uses the function \code{\link[ks]{kde}} as kernel density estimator.
#'
#' Accept Reject Method is used to simulate random variables.
#' Following code named distributions can be used as a value of the argument \code{distr} and an auxiliary distribution of the simulation method.
#' For univariate distributions:
#' \describe{
#' \item{norm}{normal distribution (default), \eqn{(-\infty,+\infty)}}
#' \item{exp}{exponential distribution, \eqn{(0,+\infty)}}
#' \item{unif}{uniform distribution, \eqn{(a,b)}}
#' }
#' For multivariate distributions, "norm" (multivariate normal distribution) is used.
#' @seealso \code{\link[ks]{kde}}
#' @examples
#' \dontrun{
#'
#' ## 1-dimensional data
#' data(faithful)
#' hist(faithful$eruptions)
#' res <- simukde::simulate_kde(x = faithful$eruptions, n = 1000)
#' hist(res$random.values)
#'
#' ## 2-dimensional data
#' data(faithful)
#' res <- simukde::simulate_kde(x = faithful, n = 100)
#' plot(res$kde, display = "filled.contour2")
#' points(x = res$random.values, cex = 0.25, pch = 16, col = "green")
#' points(x = faithful, cex = 0.25, pch = 16, col = "black")
#' }
#' @export

simulate_kde <- function (x, n = 100, distr = "norm", const.only = FALSE, seed = NULL, parallel = TRUE, ...) {

  ## kernel density estimation (KDE)

  kd <- ks::kde(x = x, ...)

  ## evaluated points by using KDE

  eval.points <- as.matrix(expand.grid(kd$eval.points))

  ## auxiliary distribution

  if (is.matrix(x) || is.data.frame(x)) {
    if (distr == "norm") { # multivariate normal distribution
      params <- list("mean" = colMeans(kd$x), "sigma" = stats::cov(kd$x))
      aux_distr <- get_aux_distr(dname = "mvnorm")
    }
  } else if (is.vector(x)) {
    if (distr == "norm") { # normal distribution
      params <- list("mean" = mean(kd$x), "sd" = stats::sd(kd$x))
      aux_distr <- get_aux_distr(dname = "norm")
    } else if (distr == "exp") { # exponential distribution
      params <- list("rate" = 1 / mean(kd$x))
      aux_distr <- get_aux_distr(dname = "exp")
    } else if (distr == "unif") { # uniform distribution
      params <- list("min" = min(kd$x), "max" = max(kd$x))
      aux_distr <- get_aux_distr(dname = "unif")
    }
  } else {
    stop("x must be a vector, matrix or data frame.")
  }
  if (!exists("aux_distr")) {
    stop("distr is invalid.")
  }

  ## constant of accept reject method

  const <- est_const(kd = kd, ddistr = aux_distr$density, params)

  if (const.only) {
    return(const)
  }

  ## simulate multivariate random variable with KDE

  if (parallel) {
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(cl = cl, iseed = seed)
    }
    y <- parallel::parSapplyLB(cl = cl, X = seq_len(n), FUN = function (i) {
      while (TRUE) {
        u <- stats::runif(n = 1)
        y <- aux_distr$random(n = 1, params = params)
        f <- ks::kde(x = kd$x, eval.points = y)$estimate
        g <- aux_distr$density(x = y, params = params)
        if (u * const < f / g) {
          break
        }
      }
      y
    })
    parallel::stopCluster(cl = cl)
  } else {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    y <- replicate(n = abs(as.integer(n)), expr = {
      while (TRUE) {
        u <- stats::runif(n = 1)
        y <- aux_distr$random(n = 1, params = params)
        f <- ks::kde(x = kd$x, eval.points = y)$estimate
        g <- aux_distr$density(x = y, params = params)
        if (u * const < f / g) {
          break
        }
      }
      y
    }, simplify = TRUE)
  }

  ## result

  if (is.matrix(y)) {
    y <- t(y)
  }

  list("x" = x, "random.values" = y, "kde" = kd, "const" = const)

}
