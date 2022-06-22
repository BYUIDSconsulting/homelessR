#' @title download census data
#' @import tidycensus
#' @import stringr
#' @import dplyr
#' @param table, the name of the table 
#' @param start_year the first year you would like data from
#' @param end_year the latest year you would like data from
#' @param output tidy or wide
#' @param geography state, county, ect.
#' @export 
get_census_data <- function(table = "B01001", start_year=2010, end_year=2019,  output="wide", geography = "state", survey = "acs1"){
  
  years = start_year:end_year
  temp <- data.frame(matrix(ncol = 0, nrow = 0))
  
  
  for (year in years) {
    
    #send request for data from a specific year
    data3 <- tidycensus::get_acs(
      geography = geography,
      table = table,
      year = year,
      endyear = NULL,
      output = "wide",
      state = NULL,
      county = NULL,
      zcta = NULL,
      geometry = FALSE,
      keep_geo_vars = FALSE,
      shift_geo = FALSE,
      summary_var = NULL,
      key = NULL,
      moe_level = 90,
      survey = "acs1",
      show_call = FALSE
    )
    
    data4 <- data3 %>% tidyr::pivot_longer(starts_with("B0"), names_to = "name")
    
    var <- tidycensus::load_variables(year, "acs1")
    
    new_var <- subset(var, select = -c(concept))
    
    
    new_names <- dplyr::left_join(data4, new_var, by = "name")
    
    data5 <- data4 |> 
      dplyr::filter(stringr::str_sub(name, -1, -1) == "E") |> 
      dplyr::mutate(name = substr(name,1,nchar(name)-1) 
      )
    #I am a resting hippo
    data6 <- dplyr::left_join(data5, new_var, by="name") |>
      dplyr::mutate(year = year) |>
      dplyr::mutate(name = label) |>
      dplyr::select(-c(label))
    
    data7 <- data6 |> dplyr::rename(Category = name) |> dplyr::select(-value, value) |> 
      dplyr::rename(state = NAME) |> dplyr::rename(Year = year)
    colnames(data7) = gsub(pattern = ":", replacement = "", x = colnames(data7))
    
    
    if (nrow(temp) == 0) {
      temp <- data7
    } else {
      temp <- rbind(temp, data7)
    }
  }
  temp2 <- temp |> tidyr:pivot_wider(names_from = Category, values_from = value)
  return(temp2)
}

#' @import tidycensus
#' @title enter api key
#' @param apikey, your personal api key, you must ask for this on the census website
establish_census_api <- function(apikey) {
  tidy_census::census_api_key(apikey, install = TRUE, overwrite = TRUE)
  readRenviron("~/.Renviron")
}
