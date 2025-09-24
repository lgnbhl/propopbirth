#' Check: tibble with columns spatial_unit and nat.
#' @param x tibble
#' @return boolean, `TRUE` if tibble with these columns, else `FALSE.`
#' @export
#' @autoglobal
#' @noRd
is_tibble_with_cols <- function(x) {
  inherits(x, "tbl_df") && all(c("spatial_unit", "nat") %in% names(x))
}
