#' forecast tfr or mab
#' @param topic tfr or mab
#' @param input_dataset input tibble with variables: spatial_unit, nat, year, y (tfr or mab)
#' @param trend_model model type (ARIMA or lm), begin, end, amount of years used to fit the lm model
#' @param temporal_model model type (cubic, Bezier or constant), begin, end,
#'  proportion of trend used, proportion slopes used,
#' @param temporal_end tibble with given y-values at the end of the temporal forecast period
#' @param constant_model model type (constant), begin, end
#'
#' @return predictions (tibble)
#' @export
#' @autoglobal
#'
#' @examples
forecast_tfr_mab <- function(topic,
                             input_dataset,
                             trend_model,
                             temporal_model,
                             temporal_end = NA,
                             constant_model) {
  # topic -------------------------------------------------------------------

  input_dat <- input_dataset |>
    dplyr::rename(y = !!sym(topic))


  # trend model -------------------------------------------------------------

  if (trend_model[1] == "ARIMA") {
    trend_dat <- input_dat |>
      dplyr::group_by(spatial_unit, nat) |>
      dplyr::group_split() |>
      purrr::map(~ {
        tibble(trend_arima(.x$year, .x$y, trend_model[2], temporal_model[3], trend_model[5]),
          spatial_unit = unique(.x$spatial_unit),
          nat = unique(.x$nat)
        )
      }) |>
      dplyr::bind_rows() |>
      dplyr::select(spatial_unit, nat, year, y, category)
  } else if (trend_model[1] == "lm") {
    trend_dat <- trend_lm(
      input_lm = input_dat,
      year_begin = trend_model[2],
      year_end = temporal_model[3],
      trend_past = trend_model[4],
      trend_prop = trend_model[5]
    )
  }


  # temporal model: points --------------------------------------------------

  if (trend_model[1] %in% c("ARIMA", "lm")) {
    # start and end point
    points_dat <- temporal_points(
      input_past = input_dat,
      input_trend = trend_dat,
      year_begin = temporal_model[2],
      year_end = temporal_model[3],
      trend_prop = temporal_model[4],
      z0_prop = temporal_model[5],
      z1_prop = temporal_model[6]
    )


    # if y-values at the end of the temporal forecast
    if (is_tibble_with_cols(temporal_end)) {
      points_dat <- points_dat |>
        dplyr::left_join(temporal_end, by = c("spatial_unit", "nat")) |>
        dplyr::mutate(y1 = y_end) |>
        dplyr::select(-y_end)
    }
  }



  # temporal model ----------------------------------------------------------


  if (temporal_model[1] == "cubic") {
    temporal_dat <- temporal_cubic(
      points_dat = points_dat,
      year_begin = temporal_model[2],
      year_end = temporal_model[3]
    )
  } else if (temporal_model[1] == "Bezier") {
    temporal_dat <- temporal_Bezier(
      points_dat = points_dat,
      year_begin = temporal_model[2],
      year_end = temporal_model[3]
    )
  } else if (temporal_model[1] == "constant") {
    temporal_dat <- temporal_constant(
      points_dat = points_dat,
      year_begin = temporal_model[2],
      year_end = temporal_model[3]
    )
  }


  # constant model ----------------------------------------------------------


  if (constant_model[1] == "constant") {
    constant_dat <- constant_model(
      points_dat = temporal_dat,
      year_begin = constant_model[2],
      year_end = constant_model[3]
    )
  }


  # time series (past and forecast) -----------------------------------------

  time_series <- bind_rows(
    dplyr::mutate(input_dat, category = "past"),
    dplyr::filter(trend_dat, year >= as.numeric(trend_model[2]), year <= as.numeric(trend_model[3])),
    dplyr::filter(temporal_dat, year >= as.numeric(temporal_model[2]), year <= as.numeric(temporal_model[3])),
    dplyr::filter(constant_dat, year >= as.numeric(constant_model[2]), year <= as.numeric(constant_model[3]))
  ) |>
    dplyr::rename(!!topic := y) |>
    dplyr::arrange(spatial_unit, nat, year)

  return(time_series)
}
