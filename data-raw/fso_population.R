#' Population data from the Federal Statistical Office
#'
#' @description end year population from different spatial units.
#'
#' Update description in folder propop/R/

# Default way to create data, contingent on STATPOP availability
fso_pop <- get_population_data(
  number_fso = "px-x-0102010000_101",
  year_first = 2010,
  year_last = 2023,
  age_fert_min = 15,
  age_fert_max = 49,
  spatial_code = c("4001", "4566", "0261"),
  spatial_unit = c("Aarau", "Frauenfeld", "Stadt Zürich"),
  binational = TRUE
)

usethis::use_data(fso_pop, overwrite = TRUE)
