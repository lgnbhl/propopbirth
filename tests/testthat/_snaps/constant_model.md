# Tests for constant_model()

    Code
      constructive::construct(constant_model(in_dat = temporal_dat, year_start = 2065,
        year_end = 2075))
    Output
      tibble::tibble(
        spatial_unit = rep("Aarau", 22L),
        nat = rep(c("ch", "int"), 11),
        year = rep(2065:2075, each = 2L),
        y = rep(c(0.6072604575976803, 1.7213637896498275), 11),
        category = rep("constant", 22L),
      )

