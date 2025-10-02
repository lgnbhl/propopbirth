#' regression model: fit and prediction
#'
#' @param y regression fit
#' @param x1 explanatory variable (age + 0.5
#' @param x2 explanatory variable (x1 ^ 2)
#' @param x3 explanatory variable (x1 ^ 3)
#' @param x4 explanatory variable (x1 ^ 4)
#' @param x5 explanatory variable (x1 ^ 5)
#' @param age_min minimum fertile age
#' @param age_max maximum fertile age
#'
#' @return
#' @export
#' @autoglobal
#'
#' @noRd
pred_regression <- function(y, x1, x2, x3, x4, x5, age_min, age_max) {
  # fit
  mod_fit <- lm(y ~ x1 + x2 + x3 + x4 + x5)

  # new data
  dat_new <- tibble::tibble(age = (age_min - 1):age_max) |>
    dplyr::mutate(
      x1 = age + 0.5,
      x2 = x1^2,
      x3 = x1^3,
      x4 = x1^4,
      x5 = x1^5,
      a0 = coef(mod_fit)[1],
      a1 = coef(mod_fit)[2],
      a2 = coef(mod_fit)[3],
      a3 = coef(mod_fit)[4],
      a4 = coef(mod_fit)[5],
      a5 = coef(mod_fit)[6]
    )


  # prediction
  output <- dat_new |>
    dplyr::mutate(y_reg = a0 + a1 * x1 + a2 * x2 + a3 * x3 + a4 * x4 + a5 * x5)

  # output
  return(output)
}
