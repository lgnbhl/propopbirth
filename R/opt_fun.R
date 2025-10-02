#' Optimize the mab-function (`mab_fun()`).
#'
#' @param mab_proj numeric, objective mab of the optimization.
#' @param a0 numeric, intercept (to be optimized).
#' @param age vector, age (from `age_min` - 1 until `age_max`).
#' @param y_no_a0 vector, regression result without intercept.
#' @param maxit numeric, maximum iterations (of optim function).
#' @param abstol numeric, absolute tolerance (of optim function).
#'
#' @return tibble with optimized parameter and objective function.
#'
#' @noRd
opt_fun <- function(
    mab_proj,
    a0,
    age,
    y_no_a0,
    maxit,
    abstol) {
  # optimize
  res <- stats::optim(
    par = a0,
    mab_fun,
    age = age,
    y_no_a0 = y_no_a0,
    mab_proj = mab_proj,
    method = "BFGS",
    control = list(
      maxit = maxit,
      abstol = abstol
    )
  )

  out <- tibble::tibble(
    par = res$par,
    value = res$value
  )

  return(out)
}
