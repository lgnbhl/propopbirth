test_that("snapshot test for temporal_constant()", {
  
  # Prepare input ----
  points_dat <- tibble::tibble(
    spatial_unit = rep(c("Aarau", "Frauenfeld", "Stadt Z\U{FC}rich"), each = 2L),
    nat = rep(c("ch", "int"), 3),
    x0 = rep(2027, 6L),
    y0 = c(
      0.7040489953546032, 1.8822970980478209, 0.6954023072502299,
      1.7894731591442672, 0.8040872905710529, 1.048976766956025
    ),
    z0 = c(
      -0.053909059278934524, 0.009730125113773313, -0.053135309878894886,
      -0.019186193651699133, -0.02717758744147899, -0.025127073633841278
    ),
    x1 = rep(2055, 6L),
    y1 = c(
      0.6072604575976803, 1.7213637896498275, 0.6182643435251416, 1.455537476815324,
      0.6055142929536337, 0.7238518298011473
    ),
    z1 = c(0, -0, 0, -0, 0, 0),
  )
  
  expect_snapshot(constructive::construct(
    temporal_constant(
      points_dat = points_dat,
      year_start = 2027,
      year_end = 2055
    )
  ))
})
