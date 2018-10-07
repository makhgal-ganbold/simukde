# simukde 1.1.0

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
