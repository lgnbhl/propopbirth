#' Trend prediction with linear model.
#'
#' @param input_lm data frame with variables `year`, `y`, `spatial_unit`, `nat`.
#' @param year_start numeric, start of prediction.
#' @param year_end numeric, end of prediction.
#' @param trend_past numeric, number of past years that are used to fit the model.
#' @param trend_prop numeric, y value of the end point: proportion of trend vs. past.
#' @param digits_y numeric, number of digits of the y value
#'
#' @return tibble with prediction data
#'
#' @noRd
trend_lm <- function(
  input_lm,
  year_start,
  year_end,
  trend_past,
  trend_prop,
  digits_y = 3
) {
  # numeric input
  year_start <- as.numeric(year_start)
  year_end <- as.numeric(year_end)
  trend_past <- as.numeric(trend_past)
  trend_prop <- as.numeric(trend_prop)

  # last year of the past
  past_last <- input_lm |>
    dplyr::filter(year == max(year)) |>
    dplyr::rename(y_past_last = y) |>
    dplyr::select(spatial_unit, nat, y_past_last)

  # input data: select years before model fit
  in_dat <- input_lm |>
    dplyr::filter(year >= max(year) - trend_past + 1)

  # fit the model
  # several options for the lm function depending on the existence of several
  # factor levels (e.g. if only one spatial unit is present, the variable will
  # not be considered in the lm)
  if (length(unique(in_dat$spatial_unit)) > 1 & length(unique(in_dat$nat)) > 1) {
    lm_fit <- lm(y ~ year * spatial_unit * nat, data = in_dat)
  } else if (length(unique(in_dat$spatial_unit)) > 1) {
    lm_fit <- lm(y ~ year * spatial_unit, data = in_dat)
  } else if (length(unique(in_dat$nat)) > 1) {
    lm_fit <- lm(y ~ year * nat, data = in_dat)
  } else {
    lm_fit <- lm(y ~ year, data = in_dat)
  }

  # prediction (on purpose until the end of the temporal forecast)
  new_data <- tidyr::expand_grid(
    year = year_start:year_end,
    spatial_unit = unique(in_dat$spatial_unit),
    nat = unique(in_dat$nat)
  )

  lm_pred <- new_data |>
    dplyr::mutate(
      y_pred = pmax(0, predict(lm_fit, newdata = new_data)),
      category = "lm"
    ) |>
    dplyr::left_join(past_last, by = c("spatial_unit", "nat")) |>
    dplyr::mutate(
      y = round(pmax(0, y_past_last + trend_prop * (y_pred - y_past_last)), digits_y)
    ) |>
    dplyr::select(spatial_unit, nat, year, y, category)

  # output
  return(lm_pred)
}
