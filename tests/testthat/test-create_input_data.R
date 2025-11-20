test_that("Simple snapshot test for creating input data", {
  
    expect_snapshot(dput(
      create_input_data(
        population = fso_pop,
        births = fso_birth |>
          dplyr::filter(spatial_unit %in% c("Aarau")),
        year_first = 2011,
        year_last = 2014,
        age_fert_min = 15,
        age_fert_max = 49,
        fert_hist_years = 3,
        binational = TRUE
      )
    ))
  })
  