#' Forecast age-specific fertility rates.
#'
#' @param fer_dat data frame, fertility data, tibble with variables `spatial_unit`,
#'        `nat`, `age`, `fer`.
#' @param tfr_dat tfr data, tibble with variables `spatial_unit`, `nat`, `year`, `tfr`.
#' @param mab_dat mab data, tibble with variables `spatial_unit`, `nat`, `year`, `mab`.
#' @param year_start numeric, start of prediction.
#' @param year_end numeric, end of prediction.
#' @param maxit numeric, maximum iterations of optimization.
#' @param abstol numeric, absolute tolerance of optimization.
#' @param digits_birth_rate numeric, number of digits of the birth rate.
#'
#' @return birth rate, tibble with variables `spatial_unit`, `nat`, `age`, `birth_rate`.
#' @export
#' @autoglobal
#'
forecast_fertility_rate <- function(
  fer_dat,
  tfr_dat,
  mab_dat,
  year_start,
  year_end,
  maxit = 1000,
  abstol = 0.001,
  digits_birth_rate = 5
) {
  # checks ------------------------------------------------------------------
  ## fertility data ---------------------------------------------------------
  assertthat::assert_that("spatial_unit" %in% names(fer_dat),
    msg = "Column `spatial_unit` is missing in `fer_dat`."
  )
  assertthat::assert_that(is.character(fer_dat$spatial_unit),
    msg = "Column `spatial_unit` in `fer_dat` must be character."
  )
  assertthat::assert_that("nat" %in% names(fer_dat),
    msg = "Column `nat` is missing in `fer_dat`."
  )
  assertthat::assert_that(is.character(fer_dat$nat),
    msg = "Column `nat` in `fer_dat` must be character."
  )
  assertthat::assert_that("age" %in% names(fer_dat),
    msg = "Column `age` is missing in `fer_dat`."
  )
  assertthat::assert_that(is.numeric(fer_dat$age),
    msg = "Column `age` in `fer_dat` must be numeric."
  )
  assertthat::assert_that("fer" %in% names(fer_dat),
    msg = "Column `fer` is missing in `fer_dat`."
  )
  assertthat::assert_that(is.numeric(fer_dat$fer),
    msg = "Column `fer` in `fer_dat` must be numeric."
  )
  ## tfr forecast data ------------------------------------------------------
  assertthat::assert_that("year" %in% names(tfr_dat),
    msg = "Column `year` is missing in `tfr_dat`."
  )
  assertthat::assert_that(is.numeric(tfr_dat$year),
    msg = "Column `year` in `tfr_dat` must be numeric."
  )
  assertthat::assert_that("spatial_unit" %in% names(tfr_dat),
    msg = "Column `spatial_unit` is missing in `tfr_dat`."
  )
  assertthat::assert_that(is.character(tfr_dat$spatial_unit),
    msg = "Column `spatial_unit` in `tfr_dat` must be character."
  )
  assertthat::assert_that("nat" %in% names(tfr_dat),
    msg = "Column `nat` is missing in `tfr_dat`."
  )
  assertthat::assert_that(is.character(tfr_dat$nat),
    msg = "Column `nat` in `tfr_dat` must be character."
  )
  assertthat::assert_that("tfr" %in% names(tfr_dat),
    msg = "Column `tfr` is missing in `tfr_dat`."
  )
  assertthat::assert_that(is.numeric(tfr_dat$tfr),
    msg = "Column `tfr` in `tfr_dat` must be numeric."
  )
  assertthat::assert_that("category" %in% names(tfr_dat),
    msg = "Column `category` is missing in `tfr_dat`."
  )
  assertthat::assert_that(is.character(tfr_dat$category),
    msg = "Column `category` in `tfr_dat` must be character."
  )
  ## mab forecast data ------------------------------------------------------
  assertthat::assert_that("year" %in% names(mab_dat),
    msg = "Column `year` is missing in `mab_dat`."
  )
  assertthat::assert_that(is.numeric(mab_dat$year),
    msg = "Column `year` in `mab_dat` must be numeric."
  )
  assertthat::assert_that("spatial_unit" %in% names(mab_dat),
    msg = "Column `spatial_unit` is missing in `mab_dat`."
  )
  assertthat::assert_that(is.character(mab_dat$spatial_unit),
    msg = "Column `spatial_unit` in `mab_dat` must be character."
  )
  assertthat::assert_that("nat" %in% names(mab_dat),
    msg = "Column `nat` is missing in `mab_dat`."
  )
  assertthat::assert_that(is.character(mab_dat$nat),
    msg = "Column `nat` in `mab_dat` must be character."
  )
  assertthat::assert_that("mab" %in% names(mab_dat),
    msg = "Column `mab` is missing in `mab_dat`."
  )
  assertthat::assert_that(is.numeric(mab_dat$mab),
    msg = "Column `mab` in `mab_dat` must be numeric."
  )
  assertthat::assert_that("category" %in% names(mab_dat),
    msg = "Column `category` is missing in `mab_dat`."
  )
  assertthat::assert_that(is.character(mab_dat$category),
    msg = "Column `category` in `mab_dat` must be character."
  )
  ## further arguments ------------------------------------------------------
  assertthat::assert_that(length(year_start) == 1,
    msg = "Please specify exactly one year for `year_start`."
  )
  assertthat::assert_that(is.numeric(year_start),
    msg = "Argument `year_start` must be numeric."
  )
  assertthat::assert_that(length(year_end) == 1,
    msg = "Please specify exactly one year for `year_end`."
  )
  assertthat::assert_that(is.numeric(year_start),
    msg = "Argument `year_end` must be numeric."
  )

  # fertility rate: regression for last data year ---------------------------
  # minimum fertility (without zero)
  min_fer <- min(fer_dat$fer[fer_dat$fer != 0])

  # subjective correction (e.g. instead of zero: 10 % of lowest value) because
  # logarithm is not possible with zeros
  corr_value <- min_fer * 0.1

  # age: min, max
  age_min <- min(fer_dat$age)
  age_max <- max(fer_dat$age)

  # cumulative, standardized, log
  cum_sta <- fer_dat |>
    dplyr::group_by(spatial_unit, nat) |>
    dplyr::mutate(
      fer_corr = dplyr::if_else(fer == 0, corr_value, fer),
      tx_cum = cumsum(fer_corr),
      tx_sta = tx_cum / sum(fer_corr),
      y = log(-log(tx_sta)),
      x1 = age + 0.5,
      x2 = x1^2,
      x3 = x1^3,
      x4 = x1^4,
      x5 = x1^5
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(tx_sta != 1)

  # regression model
  dat_reg <- cum_sta |>
    dplyr::group_by(spatial_unit, nat) |>
    dplyr::group_split() |>
    purrr::map(~ {
      tibble::tibble(
        pred_regression(
          .x$y, .x$x1, .x$x2, .x$x3, .x$x4, .x$x5,
          age_min, age_max
        ),
        spatial_unit = unique(.x$spatial_unit),
        nat = unique(.x$nat)
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      tx_sta_reg = exp(-exp(y_reg)),
      delta = c(0, diff(tx_sta_reg))
    )

  # fertility rate: future years --------------------------------------------
  # input from regression (last data year)
  input_reg <- dat_reg |>
    dplyr::mutate(y_no_a0 = a1 * x1 + a2 * x2 + a3 * x3 + a4 * x4 + a5 * x5) |>
    dplyr::select(spatial_unit, nat, age, a0, y_no_a0)

  # optimization
  mab_opt <- mab_dat |>
    dplyr::filter(year >= year_start) |>
    dplyr::select(year, spatial_unit, nat, mab) |>
    dplyr::left_join(input_reg, by = c("spatial_unit", "nat"), relationship = "many-to-many") |>
    dplyr::arrange(year, spatial_unit, nat, age) |>
    dplyr::group_by(year, spatial_unit, nat) |>
    dplyr::group_split() |>
    purrr::map(~ {
      tibble::tibble(
        opt_fun(
          mab_proj = .x$mab[1],
          a0 = .x$a0[1],
          age = .x$age,
          y_no_a0 = .x$y_no_a0,
          maxit = maxit,
          abstol = abstol
        ),
        year = unique(.x$year),
        spatial_unit = unique(.x$spatial_unit),
        nat = unique(.x$nat)
      )
    }) |>
    dplyr::bind_rows()

  # standardized fertility rate per year (standardized)
  fer_rate_sta <- dat_reg |>
    dplyr::select(age, spatial_unit, nat, matches("^[ax]\\d+$")) |>
    dplyr::left_join(
      mab_opt,
      by = c("spatial_unit", "nat"),
      relationship = "many-to-many"
    ) |>
    dplyr::arrange(year, age, spatial_unit, nat) |>
    dplyr::group_by(spatial_unit, nat) |>
    # par (new a0 parameter) and not a0
    dplyr::mutate(
      y = par + a1 * x1 + a2 * x2 + a3 * x3 + a4 * x4 + a5 * x5,
      tx_sta = pmin(1, pmax(0, exp(-exp(y)))),
      fer_rate_sta = pmax(0, c(0, diff(tx_sta)))
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(age >= age_min) |>
    dplyr::select(year, age, spatial_unit, nat, tx_sta, fer_rate_sta)

  # fertility rate (without standardization)
  birth_rate <- tfr_dat |>
    dplyr::filter(year >= year_start) |>
    dplyr::right_join(fer_rate_sta, by = c("year", "spatial_unit", "nat")) |>
    dplyr::mutate(birth_rate = round(fer_rate_sta * tfr, digits_birth_rate)) |>
    dplyr::select(spatial_unit, nat, year, age, birth_rate) |>
    dplyr::arrange(spatial_unit, nat, year, age)

  return(birth_rate)
}
