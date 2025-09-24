#' Get FSO population (females at 'fertile' age)
#'
#' @param number_fso character, number of FSO table (STAT-TAB)
#' @param year_first numeric, first year.
#' @param year_last numeric, last year.
#' @param age_fert_min numeric, minimum age (of 'fertile age').
#' @param age_fert_max numeric, maximum age (of 'fertile age').
#' @param spatial_code character, vector, official FSO codes.
#' @param spatial_unit character, vector, spatial unit names (free choice).
#' @param binational boolean, `TRUE` indicates that projections discriminate
#'        between two groups of nationalities. `FALSE` indicates that the
#'        projection is run without distinguishing between nationalities.
#'
#' @return female population at 'fertile age' at the end of the year,
#' tibble (spatial_unit, year, age, pop), with or without nat (nationality)
#' @export
#' @autoglobal
#'
#' @examples
#' get_population_data(
#'   number_fso = "px-x-0102010000_101",
#'   year_first = 2020,
#'   year_last = 2023,
#'   age_fert_min = 15,
#'   age_fert_max = 49,
#'   spatial_code = c("0261", "4566", "4001"),
#'   spatial_unit = c("Stadt Zuerich", "Frauenfeld", "Aarau"),
#'   binational = TRUE
#' )
get_population_data <- function(
    number_fso,
    year_first, 
    year_last,
    age_fert_min, 
    age_fert_max,
    spatial_code, 
    spatial_unit,
    binational = TRUE) {
  
  # arguments
  assertthat::assert_that(is.character(number_fso),
    msg = "The argument `number_fso` must be character."
  )
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
  assertthat::assert_that(is.logical(binational),
    msg = "The argument `binational` must be logical (TRUE or FALSE)."
  )
  # Get metadata ------------------------------------------------------------
  fso_metadata <- BFS::bfs_get_metadata(
    number_bfs = number_fso,
    language = "de"
  )

  # Get years ---------------------------------------------------------------
  query_year <- fso_metadata |>
    dplyr::filter(grepl("Jahr", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::mutate(year_num = as.numeric(values)) |>
    dplyr::filter(year_num >= year_first, year_num <= year_last) |>
    dplyr::pull(values)


  # spatial units -----------------------------------------------------------

  # lookup: codes and names
  spatial_lookup <- tibble::tibble(
    spatial_code = spatial_code,
    spatial_unit = spatial_unit
  )

  # preparation
  spatial_prep <- fso_metadata |>
    dplyr::filter(grepl("Kanton", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts))

  # selected spatial units
  spatial_selected <- spatial_prep |>
    dplyr::filter(trimws(values) %in% spatial_code) |>
    dplyr::left_join(spatial_lookup, by = c("values" = "spatial_code"))

  # spatial units: query
  query_spatial <- spatial_selected |>
    dplyr::pull(values)


  # Get age -----------------------------------------------------------------
  query_age <- fso_metadata |>
    dplyr::filter(grepl("Alter", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::mutate(age_num = as.numeric(values)) |>
    dplyr::filter(age_num >= age_fert_min, age_num <= age_fert_max) |>
    dplyr::pull(values)


  # Get sex -----------------------------------------------------------------
  query_sex <- fso_metadata |>
    dplyr::filter(grepl("Geschlecht", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::filter(values == "2") |>
    dplyr::pull(values)


  # Get nationality ---------------------------------------------------------

  # nationality lookup
  nat_lookup <- tibble::tibble(
    nat_code = c("1", "2", "-99999"),
    nat_text = c("ch", "int", "all"),
    nat_fso_text = c(
      "Schweiz", "Ausland",
      stringi::stri_unescape_unicode("Staatsangeh\\u00f6rigkeit (Kategorie) - Total")
    )
  )

  # binational? filter and text
  if (isTRUE(binational)) {
    nat_filter <- nat_lookup$nat_code[1:2]
    nat_text <- nat_lookup$nat_text[1:2]
  } else {
    nat_filter <- nat_lookup$nat_code[3]
    nat_text <- nat_lookup$nat_text[3]
  }

  # nationality query
  query_nat <- fso_metadata |>
    dplyr::filter(grepl(stringi::stri_unescape_unicode(
      "Staatsangeh\\u00f6rigkeit"), 
      code
    )) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::filter(values %in% nat_filter) |>
    dplyr::pull(values)


  # Get population ----------------------------------------------------------
  query_pop <- fso_metadata |>
    dplyr::filter(grepl(stringi::stri_unescape_unicode(
      "Bev\\u00f6lkerungstyp"), 
      code
    )) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::filter(valueTexts == stringi::stri_unescape_unicode(
      "St\\u00E4ndige Wohnbev\\u00f6lkerung"
    )) |>
    dplyr::pull(values)


  # query -------------------------------------------------------------------
  # query parameters
  query_parameter <- structure(
    list(
      query_year,
      query_spatial,
      query_pop,
      query_nat,
      query_sex,
      query_age
    ),
    names = c(
      "Jahr",
      "Kanton (-) / Bezirk (>>) / Gemeinde (......)",
      stringi::stri_unescape_unicode("Bev\\u00f6lkerungstyp"),
      stringi::stri_unescape_unicode("Staatsangeh\\u00f6rigkeit (Kategorie)"),
      "Geschlecht",
      "Alter"
    )
  )

  # query
  fso_data_import <- BFS::bfs_get_data(
    number_bfs = number_fso,
    language = "de",
    query = query_parameter
  )

  # output ------------------------------------------------------------------
  # fso population
  fso_pop <- fso_data_import |>
    dplyr::left_join(
      spatial_selected,
      by = c("Kanton (-) / Bezirk (>>) / Gemeinde (......)" = "valueTexts")
    ) |>
    dplyr::left_join(
      nat_lookup |> 
        dplyr::rename(!!stringi::stri_unescape_unicode(
          "Staatsangeh\\u00f6rigkeit (Kategorie)") := nat_fso_text
      )
    ) |>
    dplyr::mutate(
      year = as.numeric(Jahr),
      age = as.numeric(substr(Alter, 1, 2))
    ) |>
    dplyr::rename(
      pop = stringi::stri_unescape_unicode(
        "St\\u00E4ndige und nichtst\\u00E4ndige Wohnbev\\u00f6lkerung"
      ),
      nat = nat_text
    ) |>
    dplyr::select(year, spatial_unit, nat, age, n_pop = pop) |>
    dplyr::arrange(spatial_unit, year, nat, age)


  # output
  return(fso_pop)
}
