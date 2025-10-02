# snapshot test for temporal_constant()

    Code
      constructive::construct(temporal_constant(points_dat = points_dat, year_start = 2027,
        year_end = 2055))
    Output
      tibble::tibble(
        spatial_unit = rep(rep(c("Aarau", "Frauenfeld", "Stadt Z\U{FC}rich"), 29), each = 2L),
        nat = rep(c("ch", "int"), 87),
        year = rep(2027:2055, each = 6L),
        y = rep(
          c(
            0.7040489953546032, 1.8822970980478209, 0.6954023072502299,
            1.7894731591442672, 0.8040872905710529, 1.048976766956025
          ),
          29
        ),
        category = rep("constant", 174L),
      )

