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

The output from `{propopbirth}` is fully compatible with 
[`{propop}`](https://statistik-aargau.github.io/propop/index.html) (An
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

Detailed examples with explanations are provided in the vignettes (step
by step). Here an example of entire birth rate forecast for three
municipalities.

### Birth and population data

```{r setup, message=FALSE, warning=FALSE}
library(ggplot2)
library(propopbirth)
library(dplyr)
```

### Create model input data

```{r, echo = TRUE}
input <- create_input_data(
  population = fso_pop,
  births = fso_birth |> 
    dplyr::filter(spatial_unit %in% c("Aarau", "Frauenfeld", "Stadt Zürich")),
  year_first = 2011,
  year_last = 2023,
  age_fert_min = 15,
  age_fert_max = 49,
  fert_hist_years = 3,
  binational = TRUE
) 
```

### TFR (total fertility rate) forecast

```{r, echo = TRUE}
forecast_tfr <- forecast_tfr_mab(
  topic = "tfr",
  topic_data = input$tfr,
  trend_model = c(
    model = "lm", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
  ),
  temporal_model = c(
    model = "cubic", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
    z1_prop = 0
  ),
  temporal_end = NA,
  constant_model = c(model = "constant", start = 2056, end = 2075)
)
```

### MAB (mean age of the mother at birth) forecast

```{r, echo = TRUE}
forecast_mab <- forecast_tfr_mab(
  topic = "mab",
  topic_data = input$mab,
  trend_model = c(
    model = "lm", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
  ),
  temporal_model = c(
    model = "Bezier", start = 2027, end = 2055, trend_prop = 0.3, z0_prop = 0.7,
    z1_prop = 0
  ),
  temporal_end = NA,
  constant_model = c(model = "constant", start = 2056, end = 2075)
) 
```

### Forecast of the age-specific fertility rate

```{r, echo = TRUE}
forecast_fer <- forecast_fertility_rate(
  fer_dat = input$fer,
  tfr_dat = forecast_tfr,
  mab_dat = forecast_mab, 
  year_begin = 2024, 
  year_end = 2075)
```
