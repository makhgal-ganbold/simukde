## ==================================================================

##
## Internal Fuctions
##

## ==================================================================

#' @title Some functions for a given distribution
#'
#' @description Provides the instrumental or candidate probability density function of the Accept-Reject method and a random number generator.
#'
#' @param dname character, instrumental or candidate distribution name.
#' @return list of the density function and a random number generator for the given distribution.
#'
#' @noRd

get_inst_distr <- function (dname) {

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
    },
    "gamma" = {
      density <- function (x, params) stats::dgamma(x, shape = params$shape, rate = params$rate)
      random <- function (n, params) stats::rgamma(n, shape = params$shape, rate = params$rate)
    },
    "cauchy" = {
      density <- function (x, params) stats::dcauchy(x, location = params$location, scale = params$scale)
      random <- function (n, params) stats::rcauchy(n, location = params$location, scale = params$scale)
    },
    "lnorm" = {
      density <- function (x, params) stats::dlnorm(x, meanlog = params$meanlog, sdlog = params$sdlog)
      random <- function (n, params) stats::rlnorm(n, meanlog = params$meanlog, sdlog = params$sdlog)
    },
    "weibull" = {
      density <- function (x, params) stats::dweibull(x, shape = params$shape, scale = params$scale)
      random <- function (n, params) stats::rweibull(n, shape = params$shape, scale = params$scale)
    }
  )
  list("density" = density, "random" = random)

}

#' @title The constant of the Accept-Reject method
#'
#' @description Returns the constant of the Accept-Reject method for a given kernel density estimation, an instrumental or candidate density and its parameters.
#'
#' @param estimated.values kernel density estimation.
#' @param eval.points evaluated points by using KDE.
#' @param ddistr function, an instrumental or candidate distribution density function from \code{\link{get_inst_distr}}.
#' @param params instrumental or candidate distribution parameters.
#' @return numeric, the constant of the Accept-Reject method.
#'
#' @noRd

est_const <- function (estimated.values, eval.points, ddistr, params) {

  ## instrumental or candidate distribution density

  density <- ddistr(x = eval.points, params)

  ## constant of accept reject method

  max(estimated.values / density)

}

#' @title Data Validation
#'
#' @description It validates data for a given instrumental distribution.
#'
#' @param x a numeric vector; data.
#' @param distr character; instrumental or candidate distribution name.
#'
#' @return logical value which indicates data is valid for a given instrumental distribution.
#'
#' @noRd

data_validation <- function (x, distr) {

  if (is.vector(x) && distr %in% c("exp", "gamma", "lnorm", "weibull") && any(x < 0)) {
    stop("Data x is invalid for the given distribution by the argument distr.")
  }

}

## ==================================================================

##
## Public Fuctions
##

## ==================================================================

#' @title Simulation with Kernel Density Estimation
#'
#' @description Generates random values from a univariate and multivariate continuous distribution by using kernel density estimation based on a sample. The function uses the Accept-Reject method.
#'
#' @param x a numeric vector, matrix or data frame; data.
#' @param n integer; the number of random values will be generated.
#' @param distr character; instrumental or candidate distribution name. See details.
#' @param const.only logical; if \code{TRUE}, the constant of the Accept-Reject method will be returned.
#' @param seed a single value, interpreted as an integer, or \code{NULL} (default).
#' @param parallel logical; if \code{TRUE} parallel generator will be worked. \code{FALSE} is default.
#' @param ... other parameters for functions \code{\link[ks]{kde}}.
#'
#' @return list of given data, simulated values, kernel density estimation and the constant of the Accept-Reject method when \code{const.only} is \code{FALSE} (default).
#'
#' @details Such function uses the function \code{\link[ks]{kde}} as kernel density estimator.
#'
#' The Accept-Reject method is used to simulate random variables.
#' Following code named distributions can be used as a value of the argument \code{distr} and an instrumental or candidate distribution of the simulation method.
#' For univariate distributions:
#' \describe{
#' \item{norm}{normal distribution (default), \eqn{(-\infty,+\infty)}}
#' \item{cauchy}{Cauchy distribution, \eqn{(-\infty,+\infty)}}
#' \item{lnorm}{log-normal distribution, \eqn{(0,+\infty)}}
#' \item{exp}{exponential distribution, \eqn{(0,+\infty)}}
#' \item{gamma}{gamma distribution, \eqn{(0,+\infty)}}
#' \item{weibull}{Weibull distribution, \eqn{(0,+\infty)}}
#' \item{unif}{uniform distribution, \eqn{(a,b)}}
#' }
#' And you can choose the best fitting instrumental distribution to simulate random variables more effectively by using \code{\link{find_best_fit}}. See examples.
#'
#' For multivariate distributions, "norm" (multivariate normal distribution) is used.
#'
#' @seealso \code{\link{find_best_fit}}, \code{\link[ks]{kde}}
#'
#' @references \itemize{
#'  \item Tarn Duong (2018). ks: Kernel Smoothing. R package version 1.11.2. \url{https://CRAN.R-project.org/package=ks}
#'  \item Christian P. Robert and George Casella (2010) Introducing Monte Carlo Methods with R. Springer. Pages 51-57.
#' }
#'
#' @export
#'
#' @examples
#' ## 1-dimensional data
#' data(faithful)
#' hist(faithful$eruptions)
#' res <- simukde::simulate_kde(x = faithful$eruptions, n = 100, parallel = FALSE)
#' hist(res$random.values)
#'
#' ## Simulation with the best fitting instrumental distribution
#' data(faithful)
#' par(mfrow = c(1, 3))
#' hist(faithful$eruptions)
#' fit <- simukde::find_best_fit(x = faithful$eruptions, positive = TRUE)
#' res <- simukde::simulate_kde(
#'   x = faithful$eruptions, n = 100,
#'   distr = fit$distribution, parallel = FALSE
#' )
#' hist(res$random.values)
#' par(mfrow = c(1, 1))
#' \donttest{
#' ## 2-dimensional data
#' data(faithful)
#' res <- simukde::simulate_kde(x = faithful, n = 100)
#' plot(res$kde, display = "filled.contour2")
#' points(x = res$random.values, cex = 0.25, pch = 16, col = "green")
#' points(x = faithful, cex = 0.25, pch = 16, col = "black")}
#' \dontshow{
#' ## 2-dimensional data
#' data(faithful)
#' res <- simukde::simulate_kde(x = faithful, n = 1, parallel = FALSE)
#' plot(res$kde, display = "filled.contour2")
#' points(x = res$random.values, cex = 0.5, pch = 16, col = "blue")
#' points(x = faithful, cex = 0.25, pch = 16, col = "gray")}

simulate_kde <- function (x, n = 100, distr = "norm", const.only = FALSE, seed = NULL, parallel = FALSE, ...) {

  ## check input for distr

  data_validation(x = x, distr = distr)

  ## check sample size

  if (n != as.integer(n) || n <= 0) {
    stop("n is invalid.")
  }

  ## kernel density estimation (KDE)

  kd <- ks::kde(x = x, ...)

  ## instrumental or candidate distribution

  if (is.matrix(x) || is.data.frame(x)) {
    if (distr == "norm") { # multivariate normal distribution
      params <- list("mean" = colMeans(kd$x), "sigma" = stats::cov(kd$x))
      inst_distr <- get_inst_distr(dname = "mvnorm")
      eval.points <- as.matrix(expand.grid(kd$eval.points))
      estimated.values <- kd$estimate
    } else {
      stop("distr is invalid for multivariate data.")
    }
  } else if (is.vector(x)) {
    if (distr == "norm") { # normal distribution
      params <- list("mean" = mean(kd$x), "sd" = stats::sd(kd$x))
      inst_distr <- get_inst_distr(dname = "norm")
      valid.points <- rep(x = TRUE, times = length(x = kd$eval.points))
    } else if (distr == "exp") { # exponential distribution
      params <- list("rate" = 1 / mean(kd$x))
      inst_distr <- get_inst_distr(dname = "exp")
      valid.points <- which(kd$eval.points > 0)
    } else if (distr == "unif") { # uniform distribution
      params <- list("min" = min(kd$x), "max" = max(kd$x))
      inst_distr <- get_inst_distr(dname = "unif")
      valid.points <- which(kd$eval.points > params$min & kd$eval.points < params$max)
    } else if (distr == "gamma") { # gamma distribution
      suppressWarnings(param.est <- MASS::fitdistr(x = kd$x, densfun = "gamma")$estimate)
      params <- list("shape" = param.est[["shape"]], "rate" = param.est[["rate"]])
      inst_distr <- get_inst_distr(dname = "gamma")
      valid.points <- which(kd$eval.points > 0)
    } else if (distr == "cauchy") { # Cauchy distribution
      suppressWarnings(param.est <- MASS::fitdistr(x = kd$x, densfun = "cauchy")$estimate)
      params <- list("location" = param.est[["location"]], "scale" = param.est[["scale"]])
      inst_distr <- get_inst_distr(dname = "cauchy")
      valid.points <- rep(x = TRUE, times = length(x = kd$eval.points))
    } else if (distr == "lnorm") { # log normal distribution
      param.est <- MASS::fitdistr(x = kd$x, densfun = "lognormal")$estimate
      params <- list("meanlog" = param.est[["meanlog"]], "sdlog" = param.est[["sdlog"]])
      inst_distr <- get_inst_distr(dname = "lnorm")
      valid.points <- which(kd$eval.points > 0)
    } else if (distr == "weibull") { # Weibull distribution
      suppressWarnings(param.est <- MASS::fitdistr(x = kd$x, densfun = "weibull")$estimate)
      params <- list("shape" = param.est[["shape"]], "scale" = param.est[["scale"]])
      inst_distr <- get_inst_distr(dname = "weibull")
      valid.points <- which(kd$eval.points > 0)
    } else {
      stop("The value of distr is invalid for univariate data.")
    }
    eval.points <- kd$eval.points[valid.points]
    estimated.values <- kd$estimate[valid.points]
  } else {
    stop("x must be a vector, matrix or data frame.")
  }
  if (!exists("inst_distr")) {
    stop("distr is invalid.")
  }

  ## constant of the Accept-Reject method

  const <- est_const(estimated.values = estimated.values, eval.points = eval.points, ddistr = inst_distr$density, params)

  if (const.only) {
    return(const)
  }

  ## simulate multivariate random variable with KDE

  if (parallel) {
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(cl = cl, iseed = seed)
    }
    y <- parallel::parSapply(cl = cl, X = seq_len(n), FUN = function (i) {
      while (TRUE) {
        u <- stats::runif(n = 1)
        y <- inst_distr$random(n = 1, params = params)
        f <- ks::kde(x = kd$x, eval.points = y)$estimate
        g <- inst_distr$density(x = y, params = params)
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
        y <- inst_distr$random(n = 1, params = params)
        f <- ks::kde(x = kd$x, eval.points = y)$estimate
        g <- inst_distr$density(x = y, params = params)
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

  list(
    "x" = x,
    "random.values" = y,
    "kde" = kd,
    "distr" = distr,
    "const" = const,
    "sample.size" = n,
    "seed" = seed
  )

}

#' @title Find The Best Fitting Distribution
#'
#' @description It finds the best fitting distribution from supported univariate continuous distributions for given data.
#'
#' @param x a numeric vector; data.
#' @param positive a logical constant; distribution type.
#' @param plot a logical constant. If \code{TRUE} (default), a histogram and density lines are drawn.
#' @param legend.pos a character string. Indicates the legend position and must be one of "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright" (default), "right" and "center".
#' @param dlc a vector; probability density line colors for supported (up to 7) distributions. If unspecified, the rainbow color palette will be used.
#' @param dlw a numerical constant; probability density line width.
#' @param ... Further arguments and parameters for the function \code{\link[graphics]{hist}}, particularly, main title and axis labels. However, the parameter \code{freq} is not able to override.
#'
#' @return A list containing the following items:
#' \describe{
#' \item{distribution}{the name of the best fitting distribution.}
#' \item{ks.statistic}{the Kolmogorov-Smirnov test statistic for the distribution.}
#' \item{p.value}{the p-value of the test.}
#' \item{summary}{results similar to above for other distributions.}
#' \item{x}{given data.}
#' \item{n}{the sample size.}
#' }
#'
#' @details This function is supported following univariate distributions:
#' \itemize{
#'  \item for positive random variables: Log normal, Exponential, Gamma and Weibull.
#'  \item for all random variables: Normal, Cauchy, Log normal, Exponential, Gamma, Weibull and Uniform.
#' }
#'
#' Legends of the plot are ordered by p-values of the test.
#'
#' @seealso \code{\link[stats]{ks.test}}, \code{\link[MASS]{fitdistr}}, \code{\link[graphics]{hist}}
#'
#' @references
#' \enumerate{
#'  \item William J. Conover (1971). Practical Nonparametric Statistics. New York: John Wiley & Sons. Pages 295–301.
#'  \item Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#' }
#'
#' @export
#'
#' @examples
#' petal.length <- datasets::iris$Petal.Length[datasets::iris$Species == "setosa"]
#' simukde::find_best_fit(x = petal.length, positive = TRUE)

find_best_fit <- function (x, positive = FALSE, plot = TRUE, legend.pos = "topright", dlc = NULL, dlw = 1, ...) {

  # Input validation
  if (!is.vector(x)) {
    stop("x must be a vector.")
  }
  # Sample range
  r <- range(x)
  # Prepare for plot
  if (isTRUE(plot)) {
    if (!legend.pos %in% c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")) {
      stop("legend.pos is invalid.")
    }
    p.d.f <- list()
    p.d.f.args <- seq(from = r[1], to = r[2], length.out = 100)
  }
  # Sample size
  n <- length(x)
  # Avoid from ties
  if (any(table(x) > 1)) {
    x.ks <- x + stats::rnorm(n = n, sd = min(1e-6, stats::sd(x) / 1000000))
  } else {
    x.ks <- x
  }
  # Result
  distribution <- list()
  statistic <- list()
  p.value <- list()
  # Common expressions
  expr <- expression({
    statistic[""] <- test$statistic
    p.value[""] <- test$p.value
  })
  # Normal Distribution
  distribution[""] <- "norm"
  param <- MASS::fitdistr(x = x, densfun = "normal")$estimate
  test <- stats::ks.test(x = x.ks, y = stats::pnorm, mean = param[1], sd = param[2])
  eval(expr)
  if (isTRUE(plot)) {
    p.d.f$norm <- stats::dnorm(x = p.d.f.args, mean = param[1], sd = param[2])
  }
  # Cauchy Distribution
  distribution[""] <- "cauchy"
  suppressWarnings(param <- MASS::fitdistr(x = x, densfun = "cauchy")$estimate)
  test <- stats::ks.test(x = x.ks, y = stats::pcauchy, location = param[1], scale = param[2])
  eval(expr)
  if (isTRUE(plot)) {
    p.d.f$cauchy <- stats::dcauchy(x = p.d.f.args, location = param[1], scale = param[2])
  }
  # Uniform Distribution
  distribution[""] <- "unif"
  param <- r
  test <- stats::ks.test(x = x.ks, y = stats::punif, min = param[1], max = param[2])
  eval(expr)
  if (isTRUE(plot)) {
    p.d.f$unif <- stats::dunif(x = p.d.f.args, min = param[1], max = param[2])
  }
  # Positive Distributions
  if (isTRUE(positive)) {
    if (any(x < 0)) {
      warning("Positive distributions were skipped due to data contains a negative value.")
    } else {
      # Log normal Distribution
      distribution[""] <- "lnorm"
      param <- MASS::fitdistr(x = x, densfun = "lognormal")$estimate
      test <- stats::ks.test(x = x.ks, y = stats::plnorm, meanlog = param[1], sdlog = param[2])
      eval(expr)
      if (isTRUE(plot)) {
        p.d.f$lnorm <- stats::dlnorm(x = p.d.f.args, meanlog = param[1], sdlog = param[2])
      }
      # Exponential Distribution
      distribution[""] <- "exp"
      param <- MASS::fitdistr(x = x, densfun = "exponential")$estimate
      test <- stats::ks.test(x = x.ks, y = stats::pexp, rate = param[1])
      eval(expr)
      if (isTRUE(plot)) {
        p.d.f$exp <- stats::dexp(x = p.d.f.args, rate = param[1])
      }
      # Gamma Distribution
      distribution[""] <- "gamma"
      suppressWarnings(param <- MASS::fitdistr(x = x, densfun = "gamma")$estimate)
      test <- stats::ks.test(x = x.ks, y = stats::pgamma, shape = param[1], rate = param[2])
      eval(expr)
      if (isTRUE(plot)) {
        p.d.f$gamma <- stats::dgamma(x = p.d.f.args, shape = param[1], rate = param[2])
      }
      # Weibull Distribution
      distribution[""] <- "weibull"
      suppressWarnings(param <- MASS::fitdistr(x = x, densfun = "weibull")$estimate)
      test <- stats::ks.test(x = x.ks, y = stats::pweibull, shape = param[1], scale = param[2])
      eval(expr)
      if (isTRUE(plot)) {
        p.d.f$weibull <- stats::dweibull(x = p.d.f.args, shape = param[1], scale = param[2])
      }
    }
  }
  S <- data.frame(
    distribution = simplify2array(distribution),
    ks.statistic = simplify2array(statistic),
    p.value = simplify2array(p.value),
    stringsAsFactors = FALSE
  )
  distr.order <- order(S$p.value, decreasing = TRUE)
  S <- S[distr.order,]
  if (isTRUE(plot)) {
    # Histogram
    ellipsis <- list(...)
    ellipsis$freq <- FALSE
    if (!methods::hasArg("ylim")) {
      ellipsis$ylim <- c(0, max(graphics::hist(x = x, plot = FALSE)$density, simplify2array(p.d.f)))
    }
    if (!methods::hasArg("xlab")) {
      ellipsis$xlab <- deparse(substitute(x))
    }
    xname <- ellipsis$xlab
    if (!methods::hasArg("main")) {
      ellipsis$main <- paste("Histogram of" , xname)
    }
    do.call(what = graphics::hist, args = c(list(x), ellipsis))
    # Density lines
    k <- length(p.d.f)
    if (is.null(dlc) || length(dlc) < k) {
      dlc <- grDevices::rainbow(n = 7)
    }
    p.d.f <- p.d.f[distr.order]
    for (i in 1:k) {
      graphics::lines(x = p.d.f.args, y = p.d.f[[i]], type = "l", col = dlc[i], lty = i, lwd = dlw)
    }
    graphics::legend(x = legend.pos, legend = names(p.d.f), col = dlc[1:k], lty = 1:k)
  }
  list(
    "distribution" = S$distribution[1],
    "ks.statistic" = S$ks.statistic[1],
    "p.value" = S$p.value[1],
    "summary" = S,
    "x" = x,
    "n" = n
  )

}
