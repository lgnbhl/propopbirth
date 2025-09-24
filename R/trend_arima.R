#' Trend prediction with ARIMA model.
#'
#' @param year vector, years in input data.
#' @param y vector, numeric y values of input data (either `tfr` or `mab`).
#' @param year_start numeric, start of prediction.
#' @param year_end numeric, end of prediction.
#' @param trend_prop numeric, y value of the end point: proportion of trend vs. past.
#'
#' @return tibble prediction data
#' @export
#' @autoglobal
#'
#' @noRd
trend_arima <- function(
    year,
    y,
    year_start,
    year_end,
    trend_prop) {
  # numeric input
  year_start <- as.numeric(year_start)
  year_end <- as.numeric(year_end)
  trend_prop <- as.numeric(trend_prop)

  # last year of the past
  y_past_last <- tail(y, 1)

  # arima: fit and forecast
  arima_model <- forecast::auto.arima(y, d = 2, max.p = 3, max.q = 3)
  arima_forecast <- forecast::forecast(arima_model, h = year_end - year_start + 1)

  # output
  dat_out <- tibble(
    year = year_start:year_end,
    y_pred = as.numeric(pmax(0, arima_forecast$mean)),
    category = "ARIMA"
  ) |>
    dplyr::mutate(y = y_past_last + trend_prop * (y_pred - y_past_last))

  return(dat_out)
}
