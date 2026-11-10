# uncertainty (development)

- Documented how this package differs from and complements the CRAN package
  `uncertainty` (see `COMPARISON.md`).

# uncertainty 0.1.0

Initial release of the `uncertainty` package, a collection of functions for
assessing and visualizing uncertainty in variable relationships, e.g. for use
with Monte Carlo simulation outputs.

## New features

- `varkernel()`: produces a matrix of the estimated bivariate density of an
  outcome variable and an observed variable expected to influence it.
- `varkernelslice()`: cuts a slice through the density kernel from
  `varkernel()` at a specified value of the input variable, and adds data
  gathering for assessing the sliced data.
- `varkernelslicerange()`: takes a broader slice through the density kernel
  for a specified range of the input variable, with a minimum value check.
- `varslice_resample()`: resamples values from a kernel density surface at a
  specified input value, using the `raster` package for extraction, and
  returns a single resampled vector (`n_samples`, default 100 runs).
- `varscatter()`: creates a scatter plot of the outcome variable against the
  input variable.
- `varviolin()`: creates a violin/boxplot style plot of variable intervals,
  determining the optimal interval (bin) width with the Freedman-Diaconis
  rule (`IQR()`).

## Other changes

- Added an x-axis label parameter and restored axis values on the kernel
  slice plots.
- Added a boxplot width option and derived bin widths from the IQR.
- Cleaned up function descriptions and added citations.
- Removed unused variables and libraries, and updated package dependencies
  and roxygen documentation for compatibility with the latest R.
- Added a package vignette (`uncertainty-vignette.Rmd`).

## Citation

This package can be cited as:

Whitney, C., & Luedeling, E. (2022). *uncertainty: Calculates functions for
assessing uncertainty* (v0.1). Zenodo.
[doi:10.5281/zenodo.6640203](https://doi.org/10.5281/zenodo.6640203)

All versions of the package can be cited with the concept DOI
[10.5281/zenodo.6640202](https://doi.org/10.5281/zenodo.6640202), which
always resolves to the latest release.

`uncertainty` has been used in:

Luedeling E, Whitney C, Wilkes A, Aynekulu E, Rosenstock TS. Limitations of
using simple indicators for evaluating agricultural emission reductions at
farm level — evidence from Kenyan smallholder dairy production. Carbon
Footprints. 2022;1:21. doi:10.20517/cf.2022.11
