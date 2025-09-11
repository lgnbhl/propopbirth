---
editor_options: 
  markdown: 
    wrap: 72
---

# propopbirth

Birth rate forecasts based on FSO (Federal Statistical Office)
methodology

Important note: Work in progress!

## Overview

The `{propopbirth}` package consists of several modules: First, model
input data are calculated. Second, TFR (total fertility rate) and MAB
(mean age of the mother at birth) are predicted. Third, TFR and MAB
forecasts are used to predict the age-specific birth rates.

## Population projections

The output from `{propopbirth}` is fully compatible with `{propop}` (An
R package for population projections in R). `{propop}` needs future
birth rates as input. This is only available at the FSO at cantonal
level. With `{propopbirth}` the birth rates can also be calculated for
other spatial units or with own hypothesis.

## Installation

To install the current github version of the package, make sure you have
devtools installed and type:

``` r
devtools::install_github("statistik-aargau/propopbirth")
```

## Vignettes

The package includes three vignettes.

-   Vignette 1: Prepare **model** **input data** from birth and
    population, with **examples**
-   Vignette 2: **Forecast of** **TFR** (total fertility rate) and
    **MAB** (mean age of the mother at birth), with **examples**
-   Vignette 3: Forecast of the **age-specific fertility rate**;
    including an **example of the complete forecast** of the birth rate

## Limitations, future plans

### Limitations

With population scenario models, it is always necessary to check whether
the spatial units are large enough to ensure that **meaningful forecast
estimates**. There is no population limit specification by the FSO, when
not to run the forecast models. However, it **visual checks** are
advisable (graphs see vignettes for examples). These are helpful to
verify whether the estimates for the birth rate forecasts are
meaningful.

### Future plans

Currently, FSO birth data is not accessible with the required variables.
For this reason, the birth data is included in the `{propopbirth}`
package. As soon as the birth data is publicly accessible (similar to
population data), it will be directly accessible in `{propopbirth}` .
This will allow a more flexible selection of spatial units (not only
municipalities, but also districts).

## Examples

See vignettes; in particular vignette 3 (example of the entire forecast
of the birth rate).
