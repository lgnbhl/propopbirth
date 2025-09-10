#' Constant model
#'
#' @param points_dat tibble with model results of the previous period by
#'  `spatial_unit` and `nat`.
#' @param year_begin first year of the prediction.
#' @param year_end last year of the prediction.
#'
#' @return tibble with prediction data
#' @export
#' @autoglobal
#'
#' @examples
constant_model <- function(points_dat, year_begin, year_end) {
  # years: numeric
  year_begin <- as.numeric(year_begin)
  year_end <- as.numeric(year_end)

  # last year of previous model
  last_year <- points_dat |>
    dplyr::filter(year == max(year)) |>
    dplyr::select(spatial_unit, nat, y)

  # prediction
  pred <- tidyr::expand_grid(
    year = year_begin:year_end,
    spatial_unit = unique(points_dat$spatial_unit),
    nat = unique(points_dat$nat)
  ) |>
    dplyr::left_join(last_year, by = c("spatial_unit", "nat")) |>
    dplyr::mutate(
      category = "constant"
    ) |>
    dplyr::select(spatial_unit, nat, year, y, category)

  # output
  return(pred)
}
