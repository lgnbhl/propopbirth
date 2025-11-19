#' FSO data for births
#'
#' @description Data was retrieved from the FSO.
#' 
#' @docType data
#'

fso_birth <- get_birth_data()

# Add data frames to package
usethis::use_data(fso_birth, overwrite = TRUE)
