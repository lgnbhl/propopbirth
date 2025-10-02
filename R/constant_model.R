#' Constant model
#'
#' @param in_dat data frame, with model results of the previous period by
#'  `spatial_unit` and `nat`.
#' @param year_start numeric, first year of the prediction.
#' @param year_end numeric, last year of the prediction.
#'
#' @return tibble with prediction data
#'
#' @noRd
constant_model <- function(
    in_dat, 
    year_start, 
    year_end) {
  # numeric input
  year_start <- as.numeric(year_start)
  year_end <- as.numeric(year_end)

  # last year of previous model
  last_year <- in_dat |>
    dplyr::filter(year == max(year)) |>
    dplyr::select(spatial_unit, nat, y)

  # prediction
  pred <- tidyr::expand_grid(
    year = year_start:year_end,
    spatial_unit = unique(in_dat$spatial_unit),
    nat = unique(in_dat$nat)
  ) |>
    dplyr::left_join(last_year, by = c("spatial_unit", "nat")) |>
    dplyr::mutate(
      category = "constant"
    ) |>
    dplyr::select(spatial_unit, nat, year, y, category)

  # output
  return(pred)
}
