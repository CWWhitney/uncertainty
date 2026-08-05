devtools::release()

## Test environments

- local OS Sequoia 15.6.1, R.Version R 4.5.1 Apple clang version 16.0.0
  (clang-1600.0.26.6) GNU Fortran (GCC) 14.2.0

## R CMD check results

I built functions with roxygen2::roxygenise() I ran devtools::document()
devtools::build() devtools::check() rcmdcheck::rcmdcheck()
urlchecker::url_check() There were no ERRORs, WARNINGs or NOTEs All is
well

I checked spelling with devtools::spell_check() All is fine

## Downstream dependencies

I accessed all dependencies with pak::local_install_deps() I used the
pak::pak("CWWhitney/uncertainty") to run R CMD check with
revdepcheck::revdep_check(num_workers = 4) on downstream dependencies.
All packages passed, After running this I removed the revdep files and
submitted with devtools::submit_cran()

## Initial Release

In this version I have:

- `varkernel()`: produces a matrix of the estimated bivariate density of
  an outcome variable and an observed variable expected to influence it.
- `varkernelslice()`: cuts a slice through the density kernel from
  `varkernel()` at a specified value of the input variable, and adds
  data gathering for assessing the sliced data.
- `varkernelslicerange()`: takes a broader slice through the density
  kernel for a specified range of the input variable, with a minimum
  value check.
- `varslice_resample()`: resamples values from a kernel density surface
  at a specified input value, using the `raster` package for extraction,
  and returns a single resampled vector (`n_samples`, default 100 runs).
- `varscatter()`: creates a scatter plot of the outcome variable against
  the input variable.
- `varviolin()`: creates a violin/boxplot style plot of variable
  intervals, determining the optimal interval (bin) width with the
  Freedman-Diaconis rule (`IQR()`).
- Updated all packages and dependencies
