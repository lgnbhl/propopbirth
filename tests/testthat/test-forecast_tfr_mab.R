test_that("Snapshot tests for forecasting tfr and mab", {
  # create input data ----
  temporal_end_tfr <- tidyr::expand_grid(
    spatial_unit = c("Aarau"), 
    nat = c("ch", "int")) |> 
    dplyr::mutate(y_end = c(0.8, 1.5))
  
  input_tfr <- tibble::tibble(
    spatial_unit = rep("Aarau", 26L),
    nat = rep(c("ch", "int"), each = 13L),
    year = rep(seq(2011, 2023, by = 1), 2),
    tfr = c(
      1.3317180993542383, 1.2905418882383055, 1.3109546955687157, 1.111916627253612,
      1.30666869372653, 1.5624560873216693, 1.1577805428762786, 1.1259319259132576,
      1.2422815308477662, 1.1988829661921492, 1.2442585827987873,
      1.1661266463168989, 1.0121007626628005, 1.9372076690835396, 1.755635500403377,
      1.5330832216352193, 2.600794521482836, 1.8576157827302706, 1.894356294917059,
      2.3848535073420325, 1.990104826882635, 1.6125283802807857, 1.9003827175532302,
      1.9986812756844305, 1.8652936716833775, 1.8814830581283348
    )
  )
  
  input_mab <- tibble::tibble(
    spatial_unit = rep("Aarau", 26L),
    nat = rep(c("ch", "int"), each = 13L),
    year = rep(seq(2011, 2023, by = 1), 2),
    mab = c(
      32.73168671367248, 31.758908450546425, 33.18058082482918, 33.436254295979666,
      33.777815942185384, 33.81297190258293, 33.69255743722953, 33.78006589626685,
      33.19203655784898, 33.315090996253886, 33.37085331622384, 32.184920166214916,
      33.63685123663244, 29.559140595041534, 30.585210078173493, 30.480482821224165,
      28.750457479710438, 30.299696836601633, 29.78395139462011, 29.021948928279375,
      29.332473385592372, 29.639471501814764, 29.683602453149142,
      31.857553809416384, 30.450291043211088, 31.093898105510792
    )
  )
  
  # snapshots for TFR ----
  ## lm and cubic ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "tfr",
      topic_data = input_tfr,
      trend_model = c(
        model = "lm", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "cubic", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## lm and constant ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "tfr",
      topic_data = input_tfr,
      trend_model = c(
        model = "lm", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "constant", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## lm and Bezier ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "tfr",
      topic_data = input_tfr,
      trend_model = c(
        model = "lm", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "Bezier", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## ARIMA and cubic ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "tfr",
      topic_data = input_tfr,
      trend_model = c(
        model = "ARIMA", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "cubic", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## ARIMA and constant ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "tfr",
      topic_data = input_tfr,
      trend_model = c(
        model = "ARIMA", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "constant", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## ARIMA and Bezier ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "tfr",
      topic_data = input_tfr,
      trend_model = c(
        model = "ARIMA", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "Bezier", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))

  # snapshots for MAB ----
  ## lm and cubic ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "mab",
      topic_data = input_mab,
      trend_model = c(
        model = "lm", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "cubic", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## lm and constant ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "mab",
      topic_data = input_mab,
      trend_model = c(
        model = "lm", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "constant", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## lm and Bezier ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "mab",
      topic_data = input_mab,
      trend_model = c(
        model = "lm", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "Bezier", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## ARIMA and cubic ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "mab",
      topic_data = input_mab,
      trend_model = c(
        model = "ARIMA", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "cubic", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## ARIMA and constant ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "mab",
      topic_data = input_mab,
      trend_model = c(
        model = "ARIMA", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "constant", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))
  
  ## ARIMA and Bezier ----
  expect_snapshot(dput(
    forecast_tfr_mab(
      topic = "mab",
      topic_data = input_mab,
      trend_model = c(
        model = "ARIMA", start = 2024, end = 2026, trend_past = 7, trend_prop = 0.5
      ),
      temporal_model = c(
        model = "Bezier", start = 2027, end = 2055, trend_prop = 0.8, z0_prop = 0.7,
        z1_prop = 0
      ),
      temporal_end = temporal_end_tfr,
      constant_model = c(model = "constant", start = 2056, end = 2075)
    )
  ))

})
