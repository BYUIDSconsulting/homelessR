#' These functions need to have the httr package to make the GET request
#' against the bea.gov API

#' @import httr
#' @import dplyr
#' @import rjson
#' @title call_to_list
#' @param call_url the API url that will be sent to the bea.gov API
 
call_to_list <- function(call_url){
  results <- httr::GET(call_url)
  return(rjson::fromJSON(as.character(results)))
}

#' @title make_get_url
#' @param dataset_name - The macro type of dataset. For most of the bea functions, the default is Regional
#' @param dataset - the section of the data for the bea data
#' @param line_code - this is the specific slice of data that you would like
#' @param api_key - Required for all data calls to the bea website. 
#' @return An API url that can be used to make a call against the bea.gov API. 

make_get_url <- function(dataset_name = '', dataset = '', line_code = '', api_key='') {
  base_url <- paste0('https://apps.bea.gov/api/data?&UserID=', api_key, '&method=')
  method_type <- "getdata"
  name_of_dataset <- '&datasetname='
  year_code <- '&year=all'
  geofips_code <- '&geofips=county'
  result_form <- '&ResultFormat=JSON'
  table_name <- "&tablename="
  line_code_section <- "&linecode="
  
  base_get_data <- paste0(base_url
                          , method_type
                          , name_of_dataset
                          , dataset_name
                          , year_code
                          , geofips_code
                          , result_form
                          , table_name
                          , dataset
                          , line_code_section
                          , line_code)
  return(base_get_data)
}

#' @title filter_year
#' @param data - The data from the bea.gov API that needs to be filtered to the specified years
#' @param start - The starting year for the dataset. Default is set to 0000. 
#' @param end - Then ending year for the dataset. Default is set to 9999. 
#' @return Dataset filtered to the specified years. 

filter_year <- function(data, start = 0000, end = 9999) {
  if (start == 0000 & end == 9999) {
    return(data)
  } else if (start != 0000 & end != 9999) {
    if (end>start){
      data %>%
        filter(as.numeric(TimePeriod) > start & as.numeric(TimePeriod)<end)
      return(data)
      }
  } # may need to test new exceptions
}

#' @title agg_state_year
#' @param data - the dataset from the bea.gov API that needs to be aggregated.
#' @return returns the dataset aggregated to data values per state per year.  

agg_state_year <- function(data) {
  return(data %>% 
           mutate(DataValue = str_replace_all(DataValue, ',', ''), 
                  TimePeriod = as.numeric(TimePeriod), 
                  DataValue = str_replace(DataValue, '\\(NA\\)', '0'),
                  DataValue = as.numeric(DataValue),
                  DataValue = replace_na(DataValue, 0),
                  GeoName = str_replace(GeoName, '\\*', ''),
                  state = str_extract(GeoName, '[[:upper:]$]{2}')) %>%
           # separate(col = GeoName
           #          , into = c('county', 'state')
           #          , sep = ','
           #          , extra = 'merge') %>%
           # View()
           select(-GeoFips, -GeoName, -NoteRef) %>%
           group_by(state, TimePeriod) %>% 
           mutate(DataValue = sum(DataValue)) %>%
           distinct()
    )
}


#' @title tot_employ_bea
#' @param api_key - The API key from the bea.gov website. The following link will take you to the bea.gov website to register for an API key: https://apps.bea.gov/API/signup/index.cfm
#' @param start_year The year that the data should start. The earliest data is from 2000.
#' @param end_year The year that the data should end. The latest data is from 2020. 
#' @return The number of people that were employed for a year in a state to the desired years
#' @export
tot_employ_bea <- function(api_key ='', start_year = 0000, end_year = 9999) {
  
  dataset_name <- "Regional"
  dataset <- "CAEMP25N"
  line_code <- '10'
  
  url <- make_get_url(dataset_name
                           , dataset
                           , line_code
                           , api_key)
  
  req <- call_to_list(url)
  dat <- as.data.frame(req$BEAAPI$Results$Data[1])
  for (j in 2:length(req$BEAAPI$Results$Data)) {
    # Append the API result to the data frame
    r <- as.data.frame(req$BEAAPI$Results$Data[j])
    dat <- dplyr::bind_rows(dat,r)
  }
  
  dat <- agg_state_year(data = dat)
  dat <- filter_year(data = dat, start = start_year, end = end_year)
  return(dat)
}

#' @title gdp_cur_bea
#' @param api_key - The API key from the bea.gov website. The following link will take you to the bea.gov website to register for an API key: https://apps.bea.gov/API/signup/index.cfm
#' @param start_year The year that the data should start. The earliest data is from 2000.
#' @param end_year The year that the data should end. The latest data is from 2020. 
#' @return The number of people that were employed for a year in a state to the desired years
#' @export

gdp_cur_bea <- function(api_key ='', start_year = 0000, end_year = 9999) {
  
  dataset_name <- "Regional"
  dataset <- "CAGDP1"
  line_code <- '3'
  
  url <- make_get_url(dataset_name
                           , dataset
                           , line_code
                           , api_key)
  
  req <- call_to_list(url)
  dat <- as.data.frame(req$BEAAPI$Results$Data[1])
  for (j in 2:length(req$BEAAPI$Results$Data)) {
    # Append the API result to the data frame
    r <- as.data.frame(req$BEAAPI$Results$Data[j])
    dat <- dplyr::bind_rows(dat,r)
  }
  
  dat <- agg_state_year(data = dat)
  dat <- filter_year(data = dat, start = start_year, end = end_year)
  return(dat)
}

