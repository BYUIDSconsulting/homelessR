#' @title download Unemployment data
#' @import gdata
#' @import janitor
#' @import dplyr
#' @import tidyr
#' @param url1 - The URL pulls from Iowa state university data set of annual unemployment. 

unemployment <- function(url1 = 'http://www.icip.iastate.edu/sites/default/files/uploads/tables/employment/emp-unemployment.xls'){
  unemployment = gdata::read.xls(data_source, sheet = 2)

  unemployment <- unemployment[-c(1,2,3, 5, 57,58,59,60,61,62,63),]
  unemployment <- unemployment |>
    janitor::row_to_names(row_number=1)

  unemployment <- dplyr::rename(unemployment, state = Area)
  unemployment <- unemployment[-1]
  unemp <- unemployment |>
    tidyr::pivot_longer(!State, names_to = 'Year', values_to = 'Unimployment')
}

