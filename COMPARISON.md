# Comparison with the CRAN package `uncertainty`

CRAN hosts a different package with the same name:
[`uncertainty`](https://CRAN.R-project.org/package=uncertainty) 0.3.0
(Hugo Gasca-Aragon, "Uncertainty Estimation and Contribution Analysis").
The two packages share a name only and are complementary.

| | CRAN `uncertainty` | CWWhitney/`uncertainty` |
|---|---|---|
| Purpose | Computing uncertainty (GUM-style) | Visualising/exploring uncertainty in simulation outputs |
| Methods | Gaussian first/second order, Monte Carlo, Kragten | Bivariate kernel density, slicing, resampling, violin/scatter plots |
| Main API | `uncertaintyBudget()`, `uncertainty()`, `summary()`, `plot()` (S3) | `varkernel()`, `varkernelslice()`, `varkernelslicerange()`, `varslice_resample()`, `varscatter()`, `varviolin()` |
| Exports | `exportPattern("^[[:alpha:]]+")` (everything) | Explicit `export()` of six `var*` functions |
| Dependencies | mvtnorm, triangle | ggplot2, psych, MASS, assertthat, raster |

## Conflicts

- **Package name**: only one can be on CRAN, and both cannot be loaded or
  installed side by side in one library.
- **S3 class/generic `uncertainty`**: the CRAN package defines `uncertainty()`
  and class `"uncertainty"`. This package defines neither, so there is no
  function or class clash beyond the name.

## Complementary workflow

CRAN `uncertainty` produces Monte Carlo draws (`method = "MC"`); this package
visualises the relationship between those draws and an input. Both need only
numeric vectors, so no code changes are required to combine them.

## Recommended next step

Agree with the CRAN author on a distinct name for this package before any
CRAN submission (e.g. `uncertaintyViz`), keeping the `var*` functions as is.
Renaming touches `DESCRIPTION`, `README`, the vignette, `*.Rproj` and the
repository name, so it is deliberately not done on this branch.
