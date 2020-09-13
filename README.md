# simukde

*The R package for Simulation with Kernel Density Estimation*

## Features

1. Generates random values from a univariate and multivariate continuous distribution by using kernel density estimation based on a sample.
2. Finds the best fitting distribution from supported univariate continuous distributions for given data.

## CRAN Page

https://cran.r-project.org/package=simukde

## Example

```R
## 1-dimensional data
data(faithful)
hist(faithful$eruptions)
res <- simukde::simulate_kde(x = faithful$eruptions, n = 1000)
hist(res$random.values)

## Simulation with the best fitting instrumental distribution
data(faithful)
par(mfrow = c(1, 3))
hist(faithful$eruptions)
fit <- simukde::find_best_fit(x = faithful$eruptions, positive = TRUE)
res <- simukde::simulate_kde(
  x = faithful$eruptions, n = 1000,
  distr = fit$distribution, parallel = FALSE
)
hist(res$random.values)
par(mfrow = c(1, 1))

## 2-dimensional data
data(faithful)
res <- simukde::simulate_kde(x = faithful, n = 100)
plot(res$kde, display = "filled.contour")
points(x = res$random.values, cex = 0.25, pch = 16, col = "green")
points(x = faithful, cex = 0.25, pch = 16, col = "black")
```

## Installation

From CRAN

```R
install.packages("simukde")
```

From the repository on GitHub

```R
install.packages("devtools")
devtools::install_github("galaamn/simukde")
```

## Author

[MAKHGAL Ganbold](https://www.galaa.mn/ "Galaa's Personal Page") and BAYARBAATAR Amgalan, National University of Mongolia
## Copyright

&copy; 2018 Makhgal Ganbold and BAYARBAATAR Amgalan

## Acknowledgment

Funding: This package has been done within the framework of the project Statistics and Optimization Based Methods for Identification of Cancer-Activated Biological Processes (P2017-2519) supported by the Asia Research Center, Mongolia and Korea Foundation for Advanced Studies, Korea.

The funders had no role in study design, analysis, decision to publish, or preparation of the package.
