test_that("snapshot test for get_population_data()", {
  # create snapshot for one municipality
  get_population_snapshot <- get_population_data(
    number_fso = "px-x-0102010000_101",
    year_first = 2020,
    year_last = 2021,
    age_fert_min = 15,
    age_fert_max = 49,
    spatial_code = c("4001"),
    spatial_unit = c("Aarau"),
    binational = TRUE
  )
  
  expect_snapshot(constructive::construct(get_population_snapshot))
})
