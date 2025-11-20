#' Get FSO population (females at 'fertile' age)
#'
#' @description Data was retrieved from the FSO.
#'
#' @export
#' @autoglobal
#'
#' @examples
#' get_birth_data()
get_birth_data <- function() {
  
  # Load data
  fso_birth_raw <- data.table::fread("data-raw/fso_birth_raw.csv") |>
    tibble::as_tibble()
  
  fso_municipality <- readxl::read_excel(
    "data-raw/fso_municipalities_01012023.xlsx"
  ) |>
    tibble::as_tibble() |>
    dplyr::select(
      spatial_unit_code = "BFS Gde-nummer", spatial_unit = Gemeindename
    )
  
  # Prepare births data
  fso_birth_data <- fso_birth_raw |>
    # define factor levels for nationality
    dplyr::mutate(
      nat = dplyr::case_when(
        nat_num == 1 ~ "ch",
        nat_num == 2 ~ "int",
        .default = "unknown"
      )
    ) |>
    # add municipality names
    dplyr::left_join(
      fso_municipality,
      by = c("res_mun" = "spatial_unit_code")
    ) |>
    # identify municipalities without matches (e.g. due to municipality fusions)
    dplyr::mutate(
      spatial_unit = dplyr::case_when(
        spatial_unit == stringi::stri_unescape_unicode("Z\u00fcrich") ~ 
          stringi::stri_unescape_unicode("Stadt Z\u00fcrich"),
        is.na(spatial_unit) ~ "unknown",
        .default = spatial_unit
      )
    ) |>
    dplyr::select(year, spatial_unit, nat, age, n_birth = bir) |>
    dplyr::arrange(year, spatial_unit, nat, age)
  
  
  # output
  return(fso_birth_data)
}
