# snapshot test for temporal_points()

    Code
      constructive::construct(temporal_points(input_past = input_data, input_trend = trend_data,
        year_start = 2027, year_end = 2055, trend_prop = 0.8, z0_prop = 0.7, z1_prop = 0))
    Output
      tibble::tibble(
        spatial_unit = c("Aarau", "Aarau"),
        nat = c("ch", "int"),
        x0 = c(2027, 2027),
        y0 = c(1.0436707723180714, 1.7428530364171726),
        z0 = c(-0.004433410598531928, -0.017169759532948435),
        x1 = c(2055, 2055),
        y1 = c(0.8954876312340053, 1.2211467357050685),
        z1 = c(-0, -0),
      )

