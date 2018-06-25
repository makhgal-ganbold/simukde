# simukde

*An R package for Simulation with Kernel Density Estimation*

Generates random values from a univariate and multivariate continuous distribution by using kernel density estimation based on a sample.

## Example

```R
## 1-dimensional data
data(faithful)
hist(faithful$eruptions)
res <- simukde::simulate_kde(x = faithful$eruptions, n = 1000)
hist(res$random.values)

## 2-dimensional data
data(faithful)
res <- simukde::simulate_kde(x = faithful, n = 100)
plot(res$kde, display = "filled.contour2")
points(x = res$random.values, cex = 0.25, pch = 16, col = "green")
points(x = faithful, cex = 0.25, pch = 16, col = "black")
```

## Installation

```R
install.packages("devtools")
devtools::install_github("galaamn/simukde")
```

## Author

[MAKHGAL Ganbold](http://galaa.mn/ "Galaa's Personal Page"), National University of Mongolia, 2018
