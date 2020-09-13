# simukde 1.3.0

May 20 2021

* The example of the function `simulate_kde()` is modified due to a new version 1.13.0 of the `ks` package. The modification is that `display = "filled.contour2"` is replaced by `display = "filled.contour"` for the `plot()` function.

# simukde 1.2.0

September 13 2020

* New arguments were added to the function `find_best_fit`, particularly, `dlc` and `dlw` which specify probability density line color and width.
* Added ... (ellipsis) to the function `find_best_fit` for passing additional arguments to its internal function `hist`.

# simukde 1.1.0

October 9 2018

* Added a `NEWS.md` file to track changes to the package.
* The field `ByteCompile` was added into `DESCRIPTION` file with the value true.
* Added input validations for the argument `distr`.
* Supported more instrumental distributions hereto: Gamma, Cauchy, Log Normal, Weibull.
* Imported the package `MASS`.
* Added some tests for newly added instrumental distributions.
* Added data validation for the chosen distribution by the argument `distr`.
* Added the function `find_best_fit` for finding the best fitting instrumental univariate continuous distribution easily.
* Suggested the package `datasets`.
* Added an example for the function `simulate_kde`.
* Added some tests for the new function `find_best_fit`.

# simukde 1.0.0

October 7 2018

Initial Release
