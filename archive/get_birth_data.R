#' FSO births
#'
#' @param year_first # first year
#' @param year_last # last year
#' @param age_fert_min # minimum age (of 'fertile age')
#' @param age_fert_max # maximum age (of 'fertile age')
#' @param spatial_code # official FSO codes, vector
#' @param spatial_unit # spatial unit names (free choice), vector
#' @param with_nationality # TRUE (with nationality), FALSE (without nationality)
#'
#' @return births, tibble (spatial_unit, year, nationality, age, pop),
#' with or without nat (nationality)
#'
#' @examples
# get_birth_data(
#         year_first = 2020,
#         year_last = 2023,
#         age_fert_min = 15,
#         age_fert_max = 49,
#         spatial_code = c("0261", "4566", "4001"),
#         spatial_unit = c("Stadt Zuerich", "Frauenfeld", "Aarau"),
#         with_nationality = TRUE)
get_birth_data <- function(year_first, year_last,
                           age_fert_min, age_fert_max,
                           spatial_code, spatial_unit,
                           with_nationality = TRUE) {
  # lookup ------------------------------------------------------------------

  # spatial units
  spatial_lookup <- tibble(
    spatial_code = as.numeric(spatial_code),
    spatial_unit = spatial_unit
  )

  # nationality
  nat_lookup <- tibble(
    nat_num = 1:2,
    nat = c("ch", "int")
  )

  # data --------------------------------------------------------------------

  load(file.path(here::here(), "data/fso_birth.rda"))
  
  bir_dat <- fso_birth |>
    dplyr::filter(
      res_mun %in% spatial_lookup$spatial_code,
      year >= year_first, year <= year_last,
      age >= age_fert_min, age <= age_fert_max
    ) |>
    dplyr::left_join(spatial_lookup, by = c("res_mun" = "spatial_code")) |>
    dplyr::left_join(nat_lookup, by = "nat_num")



  # with or without nationality ---------------------------------------------

  if (with_nationality) {
    fso_bir <- bir_dat |>
      dplyr::group_by(spatial_unit, year, nat, age) |>
      dplyr::summarize(bir = sum(bir, na.rm = TRUE), .groups = "drop") |>
      dplyr::select(spatial_unit, year, nat, age, bir) |>
      dplyr::arrange(spatial_unit, year, nat, age)
  } else {
    fso_bir <- bir_dat |>
      dplyr::group_by(spatial_unit, year, age) |>
      dplyr::summarize(bir = sum(bir, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(nat = "all") |>
      dplyr::select(spatial_unit, year, nat, age, bir) |>
      dplyr::arrange(spatial_unit, year, nat, age)
  }


  # output ------------------------------------------------------------------
  return(fso_bir)
}
