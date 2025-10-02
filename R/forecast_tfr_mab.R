#' Forecast total fertility rate (TFR) or mean age of the mother at birth (MAB)
#' 
#' @param topic character, defines the parameter to calculate (`tfr` or `mab`).
#' @param topic_data data frame, input data tailored to the topic with variables: 
#'        `spatial_unit`, `nat`, `year`, either `tfr` or `mab`. This data is
#'        obtained by the function `create_input_data()`. Columns for
#'        `spatial_unit` and/or `nat` are optional.
#' @param trend_model vector, specifies the model type (`ARIMA` or `lm`), the
#'        first (`start`) and last (`end`) year. If an `lm` model is used, the 
#'        window of past years (`trend_past`) and the proportional amount of 
#'        past years used to fit the model (`trend_prop`) can be specified. 
#' @param temporal_model vector, model type (`cubic`, `Bezier` or `constant`), 
#'        first and last year, proportion of trend (`trend_prop`), proportion 
#'        for slopes (`z0_prop`) and proportion of the slope at the end point
#'        (`z1_prop`).
#' @param temporal_end data frame, contains y-values at the end of the temporal 
#'        forecast period (`y_end`).
#' @param constant_model model type (`constant`), first and last year.
#'
#' @return data frame, predictions for either `tfr` or `mab`.
#' @export
#' @autoglobal
#'
#' @examples
forecast_tfr_mab <- function(
    topic,
    topic_data,
    trend_model,
    temporal_model,
    temporal_end = NA,
    constant_model) {
  # checks ------------------------------------------------------------------
  ## topic ------------------------------------------------------------------
  assertthat::assert_that(is.character(topic),
    msg = "The `topic` must be a character value."
  )
  assertthat::assert_that(topic == "tfr" | topic == "mab",
    msg = "The `topic` must be either `tfr` or `mab`."
  )
  ## topic_data -------------------------------------------------------------
  assertthat::assert_that("year" %in% names(topic_data),
    msg = "Column `year` is missing in `topic_data`."
  )
  assertthat::assert_that(is.numeric(topic_data$year),
    msg = "Column `year` in `topic_data` must be numeric."
  )
  assertthat::assert_that(topic %in% names(topic_data),
    msg = paste0("Column `", topic, "` is missing in `topic_data`.")
  )
  assertthat::assert_that(is.numeric(topic_data[[topic]]),
    msg = paste0("Column `", topic, "` in `topic_data` must be numeric.")
  )
  assertthat::assert_that("spatial_unit" %in% names(topic_data),
    msg = "Column `spatial_unit` is missing in `topic_data`."
  )
  assertthat::assert_that(is.character(topic_data$spatial_unit),
    msg = "Column `spatial_unit` in `topic_data` must be character."
  )
  assertthat::assert_that("nat" %in% names(topic_data),
    msg = "Column `nat` is missing in `topic_data`."
  )
  assertthat::assert_that(is.character(topic_data$nat),
    msg = "Column `nat` in `topic_data` must be character."
  )
  ## trend model ------------------------------------------------------------
  assertthat::assert_that("model" %in% names(trend_model),
    msg = "Please specify a `trend_model` (model = `lm` or model = `ARIMA`)."
  )
  assertthat::assert_that(is.character(trend_model["model"]),
    msg = "The model specified in `trend_model` must be character."
  )
  assertthat::assert_that("start" %in% names(trend_model),
    msg = "Please specify a start year in the `trend_model` (e.g. start = 2020)."
  )
  assertthat::assert_that("end" %in% names(trend_model),
    msg = "Please specify the last year in the `trend_model` (e.g. end = 2020)."
  )
  assertthat::assert_that("trend_past" %in% names(trend_model),
    msg = paste0(
      "Please specify the number of years from the past for the `trend_model` ",
      "(e.g. trend_past = 7)."
    )
  )
  assertthat::assert_that("trend_prop" %in% names(trend_model),
    msg = paste0(
      "Please specify the proportional amount of past years used to fit the ",
      "`trend_model` (e.g. trend_prop = 0.5)."
    )
  )
  ## temporal model ---------------------------------------------------------
  assertthat::assert_that("model" %in% names(temporal_model),
    msg = paste0(
      "Please specify a `temporal_model` (model = `lm` / `ARIMA` / `cubic` / ",
      "`Bezier` / `constant`)."
    )
  )
  assertthat::assert_that(is.character(temporal_model["model"]),
    msg = "The model specified in `temporal_model` must be character."
  )
  assertthat::assert_that("start" %in% names(temporal_model),
    msg = "Please specify a start year in the `temporal_model` (e.g. `start` = 2020)."
  )
  assertthat::assert_that("end" %in% names(temporal_model),
    msg = "Please specify the last year in the `temporal_model` (e.g. `end` = 2020)."
  )
  assertthat::assert_that("trend_prop" %in% names(temporal_model),
    msg = paste0(
      "Please specify the proportional amount of past years used to fit the ",
      "`temporal_model` (e.g. `trend_prop` = 0.5)."
   )
  )
  assertthat::assert_that("z0_prop" %in% names(temporal_model),
    msg = paste0(
      "Please specify the proportion for slopes used to fit the `temporal_model` ",
      "(e.g. `z0_prop` = 0.7)."
    )
  )
  assertthat::assert_that("z0_prop" %in% names(temporal_model),
    msg = paste0(
      "Please specify the proportion of the slope at the end point used to fit ",
      "the `temporal_model` (e.g. `z1_prop` = 0)."
    )
  )
  ## constant model ---------------------------------------------------------
  assertthat::assert_that("model" %in% names(constant_model),
    msg = "Please specify a `constant_model` (model = `constant`)."
  )
  assertthat::assert_that(is.character(constant_model["model"]),
    msg = "The model specified in `constant_model` must be character."
  )
  assertthat::assert_that("start" %in% names(constant_model),
    msg = "Please specify a start year in the `constant_model` (e.g. start = 2020)."
  )
  assertthat::assert_that("end" %in% names(constant_model),
    msg = "Please specify the last year in the `constant_model` (e.g. `end` = 2020)."
  )
  ## temporal end -----------------------------------------------------------
  if (length(temporal_end) > 1) {
  # convert to tibble if another table format is provided
  temporal_end <- tibble::as_tibble(temporal_end)
  
  assertthat::assert_that(is_tibble_with_cols(temporal_end),
    msg = paste0(
      "Please provide `temporal_end` in a format that can be converted into ",
      "a tibble, such as a data frame or datatable."
    )
  )
  assertthat::assert_that(
    isTRUE(all(unique(topic_data$spatial_unit) == unique(temporal_end$spatial_unit))),
    msg = paste0(
      "Spatial units specified in the `temporal_end` data must be identical ",
      "to the levels of spatial units in `topic_data`."
    )
  )
  assertthat::assert_that(
    isTRUE(all(unique(topic_data$nat) == unique(temporal_end$nat))),
    msg = paste0(
      "Nationalities specified in the `temporal_end` data must be identical ",
      "to the levels of nationalities in `topic_data`."
    )
  )
  }
  
  # topic -------------------------------------------------------------------
  # rename topic-column (either `tfr` or `mab`) to column `y`.
  input_data <- topic_data |>
    dplyr::rename(y = !!rlang::sym(topic))

  # calculate model periods -------------------------------------------------
  # first the trend model, then the temporal model and finally the constant model
  ## trend model ------------------------------------------------------------
  if ("ARIMA" %in% trend_model) {
    trend_data <- input_data |>
      dplyr::group_by(spatial_unit, nat) |>
      dplyr::group_split() |>
      purrr::map(~ {
        tibble::tibble(
          trend_arima(
            year = .x$year, 
            y = .x$y, 
            year_start = trend_model["start"], 
            year_end = temporal_model["end"], 
            trend_prop = trend_model["trend_prop"]
          ),
          spatial_unit = unique(.x$spatial_unit),
          nat = unique(.x$nat)
        )
      }) |>
      dplyr::bind_rows() |>
      dplyr::select(spatial_unit, nat, year, y, category)
  } else if ("lm" %in% trend_model) {
    trend_data <- trend_lm(
      input_lm = input_data,
      year_start = trend_model["start"],
      year_end = temporal_model["end"],
      trend_past = trend_model["trend_past"],
      trend_prop = trend_model["trend_prop"]
    )
  }

  ## temporal model: points -------------------------------------------------
  if (trend_model["model"] %in% c("ARIMA", "lm")) {
    # start and end point
    points_dat <- temporal_points(
      input_past = input_data,
      input_trend = trend_data,
      year_start = temporal_model["start"],
      year_end = temporal_model["end"],
      trend_prop = temporal_model["trend_prop"],
      z0_prop = temporal_model["z0_prop"],
      z1_prop = temporal_model["z1_prop"]
    )

    # if y-values at the end of the temporal forecast
    if (is_tibble_with_cols(temporal_end)) {
      points_dat <- points_dat |>
        dplyr::left_join(temporal_end, by = c("spatial_unit", "nat")) |>
        dplyr::mutate(y1 = y_end) |>
        dplyr::select(-y_end)
    }
  }

  ## temporal model ---------------------------------------------------------
  if (temporal_model["model"] == "cubic") {
    temporal_dat <- temporal_cubic(
      points_dat = points_dat,
      year_start = temporal_model["start"],
      year_end = temporal_model["end"]
    )
  } else if (temporal_model["model"] == "Bezier") {
    temporal_dat <- temporal_Bezier(
      points_dat = points_dat,
      year_start = temporal_model["start"],
      year_end = temporal_model["end"]
    )
  } else if (temporal_model["model"] == "constant") {
    temporal_dat <- temporal_constant(
      points_dat = points_dat,
      year_start = temporal_model["start"],
      year_end = temporal_model["end"]
    )
  }

  ## constant model ---------------------------------------------------------
  if (constant_model["model"] == "constant") {
    constant_dat <- constant_model(
      in_dat = temporal_dat,
      year_start = constant_model["start"],
      year_end = constant_model["end"]
    )
  }

  # time series (past and forecast) -----------------------------------------
  time_series <- dplyr::bind_rows(
    dplyr::mutate(input_data, category = "past"),
    dplyr::filter(
      trend_data, 
      year >= as.numeric(trend_model["start"]), 
      year <= as.numeric(trend_model["end"])
    ),
    dplyr::filter(
      temporal_dat, 
      year >= as.numeric(temporal_model["start"]), 
      year <= as.numeric(temporal_model["end"])
    ),
    dplyr::filter(
      constant_dat, 
      year >= as.numeric(constant_model["start"]), 
      year <= as.numeric(constant_model["end"])
    )
  ) |>
    dplyr::rename(!!topic := y) |>
    dplyr::arrange(spatial_unit, nat, year)

  return(time_series)
}
