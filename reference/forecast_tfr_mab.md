# Forecast total fertility rate (TFR) or mean age of the mother at birth (MAB)

Forecast total fertility rate (TFR) or mean age of the mother at birth
(MAB)

## Usage

``` r
forecast_tfr_mab(
  topic,
  topic_data,
  trend_model,
  temporal_model,
  temporal_end = NA,
  constant_model
)
```

## Arguments

- topic:

  character, defines the parameter to calculate (`tfr` or `mab`).

- topic_data:

  data frame, input data tailored to the topic with variables:
  `spatial_unit`, `nat`, `year`, either `tfr` or `mab`. This data is
  obtained by the function
  [`create_input_data()`](https://statistik-aargau.github.io/propopbirth_test/reference/create_input_data.md).
  Columns for `spatial_unit` and/or `nat` are optional.

- trend_model:

  vector, specifies the model type (`ARIMA` or `lm`), the first
  (`start`) and last (`end`) year. If an `lm` model is used, the window
  of past years (`trend_past`) and the proportional amount of past years
  used to fit the model (`trend_prop`) can be specified.

- temporal_model:

  vector, model type (`cubic`, `Bezier` or `constant`), first and last
  year, proportion of trend (`trend_prop`), proportion for slopes
  (`z0_prop`) and proportion of the slope at the end point (`z1_prop`).

- temporal_end:

  data frame, contains y-values at the end of the temporal forecast
  period (`y_end`).

- constant_model:

  model type (`constant`), first and last year.

## Value

data frame, predictions for either `tfr` or `mab`.
