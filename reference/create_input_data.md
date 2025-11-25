# Create model input data

Create model input data

## Usage

``` r
create_input_data(
  population,
  births,
  year_first,
  year_last,
  age_fert_min,
  age_fert_max,
  fert_hist_years,
  binational = TRUE,
  digits_tfr = 3,
  digits_mab = 3,
  digits_fer = 5
)
```

## Arguments

- population:

  data frame, female population at 'fertile age'; columns: spatial_unit,
  year, age, pop, with or without nat (nationality).

- births:

  data frame, births of females in the fertile age range; columns:
  spatial_unit, year, age, pop, with or without nat (nationality).

- year_first:

  numeric, first year.

- year_last:

  numeric, last year.

- age_fert_min:

  numeric, minimum age (of 'fertile age').

- age_fert_max:

  numeric, minimum age (of 'fertile age').

- fert_hist_years:

  how many years are used to calculate age-specific fertility rates?

- binational:

  boolean, `TRUE` indicates that projections discriminate between two
  groups of nationalities. `FALSE` indicates that the projection is run
  without distinguishing between nationalities.

- digits_tfr:

  numeric, number of digits for tfr (total fertility rate).

- digits_mab:

  numeric, number of digits for mab (mean age of the mother at birth).

- digits_fer:

  numeric, number of digits for the fertility rate.

## Value

list with:

- tfr (total fertility rate),

- mab (mean age of the mother at birth),

- fer (fertility rate of last year(s))

## Examples

``` r
create_input_data(
  population = fso_pop,
  births = fso_birth |>
    dplyr::filter(spatial_unit %in% c("Stadt Zuerich", "Frauenfeld", "Aarau")),
  year_first = 2011,
  year_last = 2023,
  age_fert_min = 15,
  age_fert_max = 49,
  fert_hist_years = 1,
  binational = TRUE
)
#> $tfr
#> # A tibble: 78 × 4
#>    spatial_unit nat    year   tfr
#>    <chr>        <chr> <dbl> <dbl>
#>  1 Aarau        ch     2011  1.33
#>  2 Aarau        ch     2012  1.29
#>  3 Aarau        ch     2013  1.31
#>  4 Aarau        ch     2014  1.11
#>  5 Aarau        ch     2015  1.31
#>  6 Aarau        ch     2016  1.56
#>  7 Aarau        ch     2017  1.16
#>  8 Aarau        ch     2018  1.13
#>  9 Aarau        ch     2019  1.24
#> 10 Aarau        ch     2020  1.20
#> # ℹ 68 more rows
#> 
#> $mab
#> # A tibble: 78 × 4
#>    spatial_unit nat    year   mab
#>    <chr>        <chr> <dbl> <dbl>
#>  1 Aarau        ch     2011  32.7
#>  2 Aarau        ch     2012  31.8
#>  3 Aarau        ch     2013  33.2
#>  4 Aarau        ch     2014  33.4
#>  5 Aarau        ch     2015  33.8
#>  6 Aarau        ch     2016  33.8
#>  7 Aarau        ch     2017  33.7
#>  8 Aarau        ch     2018  33.8
#>  9 Aarau        ch     2019  33.2
#> 10 Aarau        ch     2020  33.3
#> # ℹ 68 more rows
#> 
#> $fer
#> # A tibble: 210 × 4
#>    spatial_unit nat     age    fer
#>    <chr>        <chr> <dbl>  <dbl>
#>  1 Aarau        ch       15 0     
#>  2 Aarau        ch       16 0     
#>  3 Aarau        ch       17 0     
#>  4 Aarau        ch       18 0     
#>  5 Aarau        ch       19 0     
#>  6 Aarau        ch       20 0     
#>  7 Aarau        ch       21 0     
#>  8 Aarau        ch       22 0.0108
#>  9 Aarau        ch       23 0.0114
#> 10 Aarau        ch       24 0     
#> # ℹ 200 more rows
#> 
#> $fer_y
#> # A tibble: 2,730 × 5
#>    spatial_unit nat    year   age birth_rate
#>    <chr>        <chr> <dbl> <dbl>      <dbl>
#>  1 Aarau        ch     2011    15    0      
#>  2 Aarau        ch     2011    16    0      
#>  3 Aarau        ch     2011    17    0      
#>  4 Aarau        ch     2011    18    0      
#>  5 Aarau        ch     2011    19    0      
#>  6 Aarau        ch     2011    20    0      
#>  7 Aarau        ch     2011    21    0      
#>  8 Aarau        ch     2011    22    0.0253 
#>  9 Aarau        ch     2011    23    0.00930
#> 10 Aarau        ch     2011    24    0.0154 
#> # ℹ 2,720 more rows
#> 
```
