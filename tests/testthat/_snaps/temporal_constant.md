# snapshot test for temporal_constant()

    Code
      constructive::construct(temporal_constant(points_dat = points_dat, year_start = 2027,
        year_end = 2055))
    Output
      tibble::tibble(
        spatial_unit = rep(rep(c("Aarau", "Frauenfeld", "Stadt Z\U{FC}rich"), 29), each = 2L),
        nat = rep(c("ch", "int"), 87),
        year = rep(2027:2055, each = 6L),
        y = rep(c(0.704, 1.882, 0.695, 1.789, 0.804, 1.049), 29),
        category = rep("constant", 174L),
      )

