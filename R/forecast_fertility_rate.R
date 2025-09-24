#' Forecast age-specific fertility rates.
#'
#' @param fer_dat fertility data, tibble with variables spatial_unit, nat, age, fer
#' @param tfr_dat tfr data, tibble with variables spatial_unit, nat, year, tfr
#' @param mab_dat mab data, tibble with variables spatial_unit, nat, year, mab
#' @param year_start forecast begin
#' @param year_end forecast end
#' @param maxit maximum iterations of optimization
#' @param abstol absolute tolerance of optimization
#'
#' @return birth rate, tibble with variables spatial_unit, nat, age, birth_rate
#' @export
#' @autoglobal
#'
#' @examples
forecast_fertility_rate <- function(
    fer_dat,
    tfr_dat,
    mab_dat,
    year_start,
    year_end,
    maxit = 1000,
    abstol = 0.001) {
  # fertility rate: regression for last data year ---------------------------

  # minimum fertility (without zero)
  min_fer <- min(fer_dat$fer[fer_dat$fer != 0])

  # subjective correction (e.g. instead of zero: 10 % of lowest value)
  # Whg? logarithm not possible with zero
  corr_value <- min_fer * 0.1

  # age: min, max
  age_min <- min(fer_dat$age)
  age_max <- max(fer_dat$age)

  # cumulative, standardized, log
  cum_sta <- fer_dat |>
    dplyr::group_by(spatial_unit, nat) |>
    dplyr::mutate(
      fer_corr = if_else(fer == 0, corr_value, fer),
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

  # ggplot(cum_sta) +
  # geom_step(aes(age, tx_sta, color = nat)) +
  # facet_wrap(~spatial_unit, nrow = 1)


  # regression model
  dat_reg <- cum_sta |>
    dplyr::group_by(spatial_unit, nat) |>
    dplyr::group_split() |>
    purrr::map(~ {
      tibble(
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

  # ggplot() +
  # geom_point(aes(age, y, color = nat), data = cum_sta) +
  # geom_line(aes(age, y_reg, color = nat), data = dat_reg) +
  # facet_wrap(~spatial_unit, nrow = 1)

  # ggplot() +
  # geom_point(aes(age, tx_sta, color = nat), data = cum_sta) +
  # geom_line(aes(age, tx_sta_reg, color = nat), data = dat_reg) +
  # facet_wrap(~spatial_unit, nrow = 1)


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
      tibble(
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

  # ggplot(mab_opt) +
  # geom_point(aes(year, value, color = nat)) +
  # facet_wrap(~spatial_unit, nrow = 1)

  # standardized fertility rate per year (standardized)
  fer_rate_sta <- dat_reg |>
    dplyr::select(age, spatial_unit, nat, matches("^[ax]\\d+$")) |>
    dplyr::left_join(mab_opt, by = c("spatial_unit", "nat"), relationship = "many-to-many") |>
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

  # fer_rate_sta |>
  #    dplyr::filter(year %% 10 == 0) |>
  #    dplyr::mutate(year = factor(year)) |>
  #    ggplot() +
  #    geom_line(aes(age, tx_sta, color = year)) +
  #    facet_grid(nat ~ spatial_unit)

  # fer_rate_sta |>
  #    dplyr::filter(year %% 10 == 0) |>
  #    dplyr::mutate(year = factor(year)) |>
  #    ggplot() +
  #    geom_line(aes(age, fer_rate_sta, color = year)) +
  #    facet_grid(nat ~ spatial_unit)

  # fertility rate (without standardization, 'birth rate': same name as in propop)
  birth_rate <- tfr_dat |>
    filter(year >= year_start) |>
    right_join(fer_rate_sta, by = c("year", "spatial_unit", "nat")) |>
    mutate(birth_rate = fer_rate_sta * tfr) |>
    select(spatial_unit, nat, year, age, birth_rate) |>
    arrange(spatial_unit, nat, year, age)

  # birth_rate |>
  #    filter(year %% 10 == 0) |>
  #    mutate(year = factor(year)) |>
  #    ggplot() +
  #    geom_line(aes(age, birth_rate, color = year)) +
  #    facet_grid(nat ~ spatial_unit)

  return(birth_rate)
}
