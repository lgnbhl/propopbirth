# Forecast age-specific fertility rates.

Forecast age-specific fertility rates.

## Usage

``` r
forecast_fertility_rate(
  fer_dat,
  tfr_dat,
  mab_dat,
  year_start,
  year_end,
  maxit = 1000,
  abstol = 0.001,
  digits_birth_rate = 5
)
```

## Arguments

- fer_dat:

  data frame, fertility data, tibble with variables `spatial_unit`,
  `nat`, `age`, `fer`.

- tfr_dat:

  tfr data, tibble with variables `spatial_unit`, `nat`, `year`, `tfr`.

- mab_dat:

  mab data, tibble with variables `spatial_unit`, `nat`, `year`, `mab`.

- year_start:

  numeric, start of prediction.

- year_end:

  numeric, end of prediction.

- maxit:

  numeric, maximum iterations of optimization.

- abstol:

  numeric, absolute tolerance of optimization.

- digits_birth_rate:

  numeric, number of digits of the birth rate.

## Value

birth rate, tibble with variables `spatial_unit`, `nat`, `age`,
`birth_rate`.
