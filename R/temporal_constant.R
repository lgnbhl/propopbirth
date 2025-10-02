#' Temporal model: constant.
#'
#' @param points_dat data frame, with point information (`x0`, `y0`) by
#'        `spatial_unit`, `nat`.
#' @param year_start numeric, start of prediction.
#' @param year_end numeric, end of prediction.
#'
#' @return tibble with prediction data
#'
#' @noRd
temporal_constant <- function(
    points_dat,
    year_start,
    year_end) {
  # numeric input
  year_start <- as.numeric(year_start)
  year_end <- as.numeric(year_end)

  # prediction
  pred <- tidyr::expand_grid(
    year = year_start:year_end,
    spatial_unit = unique(points_dat$spatial_unit),
    nat = unique(points_dat$nat)
  ) |>
    dplyr::left_join(points_dat, by = c("spatial_unit", "nat")) |>
    dplyr::mutate(
      y = y0,
      category = "constant"
    ) |>
    dplyr::select(spatial_unit, nat, year, y, category)

  # output
  return(pred)
}
