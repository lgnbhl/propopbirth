# Get FSO population (females at 'fertile' age)

Get FSO population (females at 'fertile' age)

## Usage

``` r
get_population_data(
  number_fso,
  year_first,
  year_last,
  age_fert_min,
  age_fert_max,
  spatial_code,
  spatial_unit,
  binational = TRUE
)
```

## Arguments

- number_fso:

  character, number of FSO table (STAT-TAB)

- year_first:

  numeric, first year.

- year_last:

  numeric, last year.

- age_fert_min:

  numeric, minimum age (of 'fertile age').

- age_fert_max:

  numeric, maximum age (of 'fertile age').

- spatial_code:

  character, vector, official FSO codes.

- spatial_unit:

  character, vector, spatial unit names (free choice).

- binational:

  boolean, `TRUE` indicates that projections discriminate between two
  groups of nationalities. `FALSE` indicates that the projection is run
  without distinguishing between nationalities.

## Value

female population at 'fertile age' at the end of the year, tibble
(spatial_unit, year, age, pop), with or without nat (nationality)

## Examples

``` r
get_population_data(
  number_fso = "px-x-0102010000_101",
  year_first = 2020,
  year_last = 2023,
  age_fert_min = 15,
  age_fert_max = 49,
  spatial_code = c("0261", "4566", "4001"),
  spatial_unit = c("Stadt Zuerich", "Frauenfeld", "Aarau"),
  binational = TRUE
)
#> # A tibble: 840 × 5
#>     year spatial_unit nat     age n_pop
#>    <dbl> <chr>        <chr> <dbl> <dbl>
#>  1  2020 Aarau        ch       15    64
#>  2  2020 Aarau        ch       16    69
#>  3  2020 Aarau        ch       17    72
#>  4  2020 Aarau        ch       18    64
#>  5  2020 Aarau        ch       19    68
#>  6  2020 Aarau        ch       20    69
#>  7  2020 Aarau        ch       21    78
#>  8  2020 Aarau        ch       22    84
#>  9  2020 Aarau        ch       23    89
#> 10  2020 Aarau        ch       24   134
#> # ℹ 830 more rows
```
