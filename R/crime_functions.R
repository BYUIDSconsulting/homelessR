#' @import tidyverse
#' @import readxl
#' @import tidyselect
#' @import dplyr
#' @import stringr
#' @import downloader
#' @import janitor
#' @import tm
#' @import imputeTS
#' @import plyr

#'@title get_url
#'@param year year of data desired from 2006-2019 excluding 2008 and 2016
#'@author Becca Ebersole
#'@example get_url(2019)
#'@export
## the the url for the data
get_url <- function(year){
  year = year
  if (year >= 2017) {
    url = paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year, "/crime-in-the-u.s.-", year, "/tables/table-10/table-10.xls")
  }
  else if (year == 2015 | year == 2013){
    url = paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year, "/crime-in-the-u.s.-", year, "/tables/table-10/table_10_offenses_known_to_law_enforcement_by_state_by_metropolitan_and_nonmetropolitan_counties_", year, ".xls")
  }
  else if (year == 2014){
    url = paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year, "/crime-in-the-u.s.-", year, "/tables/table-10/Table_10_Offenses_Known_to_Law_Enforcement_by_State_by_Metropolitan_and_Nonmetropolitan_Counties_", year, ".xls")
  }
  else if (year == 2012){
    url = paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year, "/crime-in-the-u.s.-", year, "/tables/10tabledatadecpdf/table_10_offenses_known_to_law_enforcement_by_state_by_metropolitan_and_nonmetropolitan_counties_", year, ".xls") 
  }
  else if (year == 2011){
    url = paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year, "/crime-in-the-u.s.-", year, "/tables/table_10_offenses_known_to_law_enforcement_by_state_by_metropolitan_and_nonmetropolitan_counties_", year, ".xls")
  }
  else if (year == 2010) {
    url <- paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year, "/crime-in-the-u.s.-", year, "/tables/10tbl10.xls")
  }
  else if (year == 2009 | year == 2008 | year == 2007 | year ==2006){
    last_two = substring(year, 2)
    url = paste0("https://www2.fbi.gov/ucr/cius", year, "/data/documents/", substring(year, 3), "tbl10.xls")
  }
  temp = tempfile()
  download.file(url, destfile = temp, mode = "wb")
  data <- read_excel(temp)
  final_data <- clean_data(data, year)
  return(final_data)
}

#'@title clean_data
#'@param data calls the data from the get_url function and cleans that data frame so its ready for the user
#'@param year calls the year from the get_url function and used it in `if` statements to execute certain code based on the year the data is on

## clean the data 
clean_data <- function(data, year){
  ## remove the first few rows and make the new first row the column names
  df <- data[-c(1, 2, 3),]
  df1 <- df %>%
    row_to_names(row_number = 1) %>%
    clean_names()
  
  ## split the State column into two columns for State and Metropolitan area
  df1 <- df1 %>%
    separate(state, c("state", "area_type"))
  
  ## have all states and area types repeat in the empty rows beneath them 
  df2 <- df1 %>%
  fill(state, .direction = 'down')
  df2 <- df2 %>%
  fill(area_type, .direction = 'down')
  
  ## remove the numbers from State, Area Type, and County
  df2$state <- str_replace_all(df2$state, "[:digit:]", "")
  df2$county <- str_replace_all(df2$county, "[:digit:]", "")
  
  ## remove the numbers from the column
  df2 <- df2 %>%
  rename_with(~ gsub('[[:digit:]]', '', .x))
  
  ## remove the rows about the subscripts
  states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
  df3 <- df2 %>%
    filter(state %in% str_to_upper(states))
  
  ## add a column with the year of data
  df3$year <- year
  
  ## rename forible rape to rape
  if (year <= 2015 & year >= 2013) {
    df3$`rape_legacy_definition_` <- as.numeric(df3$`rape_legacy_definition_`)
    df3$`rape_revised_definition_` <- as.numeric(df3$`rape_revised_definition_`)
    df3[["rape_legacy_definition_"]][is.na(df3[["rape_legacy_definition_"]])] <- 0
    df3[["rape_revised_definition_"]][is.na(df3[["rape_revised_definition_"]])] <- 0
    df3$rape <- df3$rape_legacy_definition_ + df3$rape_revised_definition_
    df3 <- df3[, -c(6:7)]
  }
  else if (year == 2012 | year == 2011 | year == 2010 | year == 2009 | year == 2007 | year == 2006) {
    df3 <- df3 %>%
      rename(c('forcible_rape' = 'rape'))
  }
  #View(df3)
  return(df3)
}

