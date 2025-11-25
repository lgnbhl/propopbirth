# FSO data for births

Data was retrieved from the FSO.

## Usage

``` r
fso_birth
```

## Format

A data frame with 437054 rows and 5 variables:

- year:

  year within the range 2010-2023

- spatial_unit:

  Swiss municipalities by name

- nat:

  Nationality; either "ch" = Swiss or "int" = international

- age:

  Age in full years

- n_birth:

  number of births

## Examples

``` r
dplyr::glimpse(fso_birth)
#> Rows: 437,054
#> Columns: 5
#> $ year         <int> 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 201…
#> $ spatial_unit <chr> "Aadorf", "Aadorf", "Aadorf", "Aadorf", "Aadorf", "Aadorf…
#> $ nat          <chr> "ch", "ch", "ch", "ch", "ch", "ch", "ch", "ch", "ch", "ch…
#> $ age          <int> 21, 23, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 3…
#> $ n_birth      <int> 1, 1, 3, 1, 5, 2, 4, 8, 6, 5, 5, 4, 3, 3, 5, 1, 3, 2, 1, …
```
