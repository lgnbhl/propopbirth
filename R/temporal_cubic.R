#' Temporal model: cubic.
#'
#' @param points_dat data frame, with point information (`x0`, `y0`, `z0`, `x1`,
#'        `y1`, `z1`) by `spatial_unit` and `nat`.
#' @param year_start numeric, start of prediction.
#' @param year_end numeric, end of prediction.
#'
#' @return tibble with prediction data
#'
#' @noRd
temporal_cubic <- function(
    points_dat,
    year_start,
    year_end) {
  # numeric input
  year_start <- as.numeric(year_start)
  year_end <- as.numeric(year_end)

  # parameters
  cubic_para <- points_dat |>
    dplyr::mutate(
      a3 = (2 * ((y0 - y1) / ((x1 - x0)**3))) + ((z0 + z1) / ((x1 - x0)**2)),
      a2 = ((z0 - z1) / (2 * (x0 - x1))) - (1.5 * (x0 + x1) * a3),
      a1 = z0 - (2 * x0 * a2) - (3 * (x0**2) * a3),
      a0 = y0 - (x0 * a1) - (x0**2) * a2 - (x0**3) * a3
    )

  # prediction
  pred <- tidyr::expand_grid(
    year = year_start:year_end,
    spatial_unit = unique(points_dat$spatial_unit),
    nat = unique(points_dat$nat)
  ) |>
    dplyr::left_join(cubic_para, by = c("spatial_unit", "nat")) |>
    dplyr::mutate(
      y = pmax(0, a0 + (a1 * year) + (a2 * (year**2)) + (a3 * (year**3))),
      category = "cubic"
    ) |>
    dplyr::select(spatial_unit, nat, year, y, category)

  # output
  return(pred)
}
