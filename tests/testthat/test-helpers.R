test_that("tests for is_tibble_with_cols()", {
  # valid tibble with required columns
  df_valid <- tibble::tibble(spatial_unit = 1:3, nat = c("a", "b", "c"))
  expect_true(is_tibble_with_cols(df_valid))

  # tibble missing one required column
  df_missing <- tibble::tibble(spatial_unit = 1:3)
  expect_false(is_tibble_with_cols(df_missing))

  # tibble with extra columns but still has required ones
  df_extra <- tibble::tibble(spatial_unit = 1:3, nat = letters[1:3], other = 10:12)
  expect_true(is_tibble_with_cols(df_extra))

  # data.frame with correct columns but not tibble
  df_not_tibble <- data.frame(spatial_unit = 1:3, nat = letters[1:3])
  expect_false(is_tibble_with_cols(df_not_tibble))

  # completely wrong input type
  expect_false(is_tibble_with_cols("not a tibble"))
  expect_false(is_tibble_with_cols(list(spatial_unit = 1, nat = "x")))

  # snapshot
  expect_snapshot(cat(is_tibble_with_cols(df_valid), "\n"))
})
