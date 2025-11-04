#' Create model input data
#'
#' @param population data frame, female population at 'fertile age'; columns:
#'   spatial_unit, year, age, pop, with or without nat (nationality).
#' @param births data frame, births of females in the fertile age range; columns:
#'   spatial_unit, year, age, pop, with or without nat (nationality).
#' @param year_first numeric, first year.
#' @param year_last numeric, last year.
#' @param age_fert_min numeric, minimum age (of 'fertile age').
#' @param age_fert_max numeric, minimum age (of 'fertile age').
#' @param fert_hist_years how many years are used to calculate age-specific
#'  fertility rates?
#' @param binational boolean, `TRUE` indicates that projections discriminate
#'  between two groups of nationalities. `FALSE` indicates that the
#'  projection is run without distinguishing between nationalities.
#' @param digits_tfr numeric, number of digits for tfr (total fertility rate).
#' @param digits_mab numeric, number of digits for mab (mean age of the mother at birth).
#' @param digits_fer numeric, number of digits for the fertility rate.
#'
#' @return list with:
#'  * tfr (total fertility rate),
#'  * mab (mean age of the mother at birth),
#'  * fer (fertility rate of last year(s))
#' @export
#' @autoglobal
#'
#' @examples
#' data("fso_birth", package = "propopbirth")
#' create_input_data(
#'   population = fso_pop,
#'   births = fso_birth |>
#'     dplyr::filter(spatial_unit %in% c("Stadt Zuerich", "Frauenfeld", "Aarau")),
#'   year_first = 2011,
#'   year_last = 2023,
#'   age_fert_min = 15,
#'   age_fert_max = 49,
#'   fert_hist_years = 1,
#'   binational = TRUE
#' )
create_input_data <- function(
  population,
  births,
  year_first,
  year_last,
  age_fert_min,
  age_fert_max,
  fert_hist_years,
  binational = TRUE,
  digits_tfr = 3,
  digits_mab = 3,
  digits_fer = 5
) {
  # checks ------------------------------------------------------------------
  # birth data
  assertthat::assert_that("year" %in% names(births),
    msg = "Column `year` is missing in `births`."
  )
  assertthat::assert_that(is.numeric(births$year),
    msg = "Column `year` in `births` must be numeric."
  )
  assertthat::assert_that("spatial_unit" %in% names(births),
    msg = "Column `spatial_unit` is missing in `births`."
  )
  assertthat::assert_that(is.character(births$spatial_unit),
    msg = "Column `spatial_unit` in `births` must be character."
  )
  assertthat::assert_that("age" %in% names(births),
    msg = "Column `age` is missing in `births`."
  )
  assertthat::assert_that(is.numeric(births$age),
    msg = "Column `age` in `births` must be numeric."
  )
  assertthat::assert_that("n_birth" %in% names(births),
    msg = paste0("The column `n_birth` is missing in `births`.")
  )
  assertthat::assert_that(is.numeric(births$n_birth),
    msg = "Column `n_birth` in `births` must be numeric."
  )

  # population data
  assertthat::assert_that("year" %in% names(population),
    msg = "Column `year` is missing in `population`."
  )
  assertthat::assert_that(is.numeric(population$year),
    msg = "Column `year` in `population` must be numeric."
  )
  assertthat::assert_that("spatial_unit" %in% names(population),
    msg = "Column `spatial_unit` is missing in `population`."
  )
  assertthat::assert_that(is.character(population$spatial_unit),
    msg = "Column `spatial_unit` in `population` must be character."
  )
  assertthat::assert_that("age" %in% names(population),
    msg = "Column `age` is missing in `population`."
  )
  assertthat::assert_that(is.numeric(population$age),
    msg = "Column `age` in `population` must be numeric."
  )
  assertthat::assert_that("n_pop" %in% names(population),
    msg = paste0("The column `n_pop` is missing in `population`.")
  )
  assertthat::assert_that(is.numeric(population$n_pop),
    msg = "Column `n_pop` in `population` must be numeric."
  )

  # further arguments
  assertthat::assert_that(is.numeric(year_first),
    msg = "The argument `year_first` must be numeric."
  )
  assertthat::assert_that(is.numeric(year_last),
    msg = "The argument `year_last` must be numeric."
  )
  assertthat::assert_that(is.numeric(age_fert_min),
    msg = "The argument `age_fert_min` must be numeric."
  )
  assertthat::assert_that(is.numeric(age_fert_max),
    msg = "The argument `age_fert_max` must be numeric."
  )
  assertthat::assert_that(is.numeric(fert_hist_years),
    msg = "The argument `fert_hist_years` must be numeric."
  )

  # nationality
  if (binational) {
    assertthat::assert_that("nat" %in% names(births),
      msg = "Column `nat` is missing in `births`."
    )
    assertthat::assert_that(is.character(births$nat),
      msg = "Column `nat` in `births` must be character."
    )
    assertthat::assert_that("nat" %in% names(population),
      msg = "Column `nat` is missing in `population`."
    )
    assertthat::assert_that(is.character(population$nat),
      msg = "Column `nat` in `population` must be character."
    )
  } else {
    # dummy column with only one nationality "ch"
    births <- births |>
      dplyr::mutate(nat = "ch")

    population <- population |>
      dplyr::mutate(nat = "ch")
  }


  # mean annual population --------------------------------------------------
  # population at the end of the year, all possible variable combinations
  pop_end_year <- tidyr::expand_grid(
    spatial_unit = unique(population$spatial_unit),
    year = (year_first - 1):year_last,
    nat = unique(population$nat),
    age = (age_fert_min - 1):age_fert_max
  ) |>
    dplyr::left_join(population, by = c("spatial_unit", "year", "nat", "age")) |>
    tidyr::replace_na(list(population = 0))

  # population next year
  pop_next <- pop_end_year |>
    dplyr::rename(pop_previous = n_pop) |>
    dplyr::mutate(
      year_next = year + 1,
      age_next = age + 1
    ) |>
    dplyr::select(spatial_unit, year_next, age_next, nat, pop_previous)

  # mean annual population
  pop_mean <- pop_end_year |>
    dplyr::filter(year >= year_first, age >= age_fert_min) |>
    dplyr::left_join(
      pop_next,
      by = c("spatial_unit", "year" = "year_next", "nat", "age" = "age_next")
    ) |>
    tidyr::replace_na(list(pop_previous = 0)) |>
    dplyr::mutate(pop = (pop_previous + n_pop) / 2) |>
    dplyr::select(spatial_unit, year, nat, age, pop)


  # fertility rate, all years -----------------------------------------------
  fer_y <- pop_mean |>
    dplyr::left_join(births, by = c("spatial_unit", "year", "nat", "age")) |>
    tidyr::replace_na(list(n_birth = 0)) |>
    dplyr::mutate(fer = dplyr::if_else(pop == 0, NA_real_, n_birth / pop))


  # TFR (total fertility rate) ----------------------------------------------
  tfr <- fer_y |>
    dplyr::group_by(spatial_unit, nat, year) |>
    dplyr::summarize(tfr = round(sum(fer, na.rm = TRUE), digits_tfr), .groups = "drop") |>
    dplyr::arrange(spatial_unit, nat, year)


  # MAB (mean age of the mother at birth) -----------------------------------
  mab <- fer_y |>
    dplyr::group_by(spatial_unit, nat, year) |>
    dplyr::filter(!is.na(fer)) |>
    dplyr::summarize(mab = round(weighted.mean(age, fer, na.rm = TRUE), digits_mab), .groups = "drop") |>
    dplyr::arrange(spatial_unit, nat, year)


  # fertility rate, last year(s) --------------------------------------------

  # if fert_hist_years == 1, only the last year
  # if fert_hist_years == 3, average over the last three years
  fer <- fer_y |>
    dplyr::select(-fer) |>
    dplyr::filter(year >= (max(year) - fert_hist_years + 1)) |>
    dplyr::group_by(spatial_unit, nat, age) |>
    dplyr::summarize(
      pop = sum(pop, na.rm = TRUE),
      n_birth = sum(n_birth, na.rm = TRUE), .groups = "drop"
    ) |>
    dplyr::mutate(fer = dplyr::if_else(pop == 0, NA_real_, round(n_birth / pop, digits_fer))) |>
    dplyr::select(spatial_unit, nat, age, fer) |>
    dplyr::arrange(spatial_unit, nat, age)


  # Output ------------------------------------------------------------------
  # select output columns with or without nationality
  if (binational) {
    cols_out <- c("spatial_unit", "nat", "year", "age", "birth_rate")
  } else {
    cols_out <- c("spatial_unit", "year", "age", "birth_rate")
  }

  # fer_y (fertility per year) is in the output to make plots
  # (at the very end of the fertility rate forecast)
  fer_y_out <- fer_y |>
    dplyr::rename(birth_rate = fer) |>
    dplyr::select(cols_out)

  return(list(tfr = tfr, mab = mab, fer = fer, fer_y = fer_y_out))
}
