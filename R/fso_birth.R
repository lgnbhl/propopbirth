#' FSO data for births
#'
#' @description Data was retrieved from the FSO.
#' 
#' @format A data frame with 437054 rows and 5 variables:
#' \describe{
#'   \item{year}{year within the range 2010-2023}
#'   \item{spatial_unit}{Swiss municipalities by name}
#'   \item{nat}{Nationality; either "ch" = Swiss or "int" = international}
#'   \item{age}{Age in full years}
#'   \item{n_birth}{number of births}
#'  }
#'
#' @name fso_birth
#' 
#' @docType data
#' 
#' @keywords datasets
#' 
#' @examples
#' dplyr::glimpse(fso_birth)
#'
"fso_birth"