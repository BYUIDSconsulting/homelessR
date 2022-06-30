#' These functions need to have the httr package to make the GET request
#' against the bea.gov API

#' @import httr
#' @import dplyr
#' @import rjson
#' @import tidyr
#' @import stringr
#' @import magrittr
#' 
#' 
#' @title call_to_list
#' @param call_url the API url that will be sent to the bea.gov API

call_to_list <- function(call_url){
  results <- httr::GET(call_url)
  return(rjson::fromJSON(as.character(results)))
}

#' @title make_get_url
#' @param dataset_name - The macro type of dataset. For most of the bea functions, the default is Regional
#' @param dataset - the section of the %>% for the bea data
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
      data |>
        dplyr::filter(as.numeric(TimePeriod) > start & as.numeric(TimePeriod)<end)
      return(data)
    }
  } # may need to test new exceptions
}

#' @title agg_state_year
#' @param data - the dataset from the bea.gov API that needs to be aggregated.
#' @return returns the dataset aggregated to data values per state per year.  

agg_state_year <- function(data, type_of_dataset = '') {
  col_name <- paste0("num_of_", type_of_dataset)
  return(data |>
           dplyr::mutate(DataValue = str_replace_all(DataValue, ',', ''), 
                         TimePeriod = as.numeric(TimePeriod), 
                         DataValue = stringr::str_replace(DataValue, '\\(NA\\)', '0'),
                         DataValue = as.numeric(DataValue),
                         DataValue = tidyr::replace_na(DataValue, 0),
                         GeoName = stringr::str_replace(GeoName, '\\*', ''),
                         state = stringr::str_extract(GeoName, '[[:upper:]$]{2}')) |>
           dplyr::select(TimePeriod, DataValue, state) |>
           dplyr::group_by(state, TimePeriod) |>
           dplyr::mutate(DataValue = sum(DataValue)) |>
           dplyr::rename(Year = TimePeriod) |>
           dplyr::rename(col_name = DataValue) |>
           dplyr::distinct()
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
  
  
  print('Making URL...')
  
  url <- make_get_url(dataset_name
                      , dataset
                      , line_code
                      , api_key)
  
  
  print('URL made!')
  print('Making API call...')
  
  req <- call_to_list(url)
  
  print('Call made! Response received.')
  print('Formatting JSON to a dataframe...')
  
  dat <- as.data.frame(req$BEAAPI$Results$Data[1])
  for (j in 2:length(req$BEAAPI$Results$Data)) {
    # Append the API result to the data frame
    r <- as.data.frame(req$BEAAPI$Results$Data[j])
    dat <- dplyr::bind_rows(dat,r)
  }
  
  print('Formatting complete!')
  print('Aggregating to state and year...')
  
  dat <- agg_state_year(data = dat, 'jobs')
  
  
  print('Filtering to desired year...')
  
  dat <- filter_year(data = dat, start = start_year, end = end_year)
  dat <- ST_to_State(dat)
  print('Finished!')
  return(dat)
}

#' @title gdp_cur_bea
#' @param api_key - The API key from the bea.gov website. The following link will take you to the bea.gov website to register for an API key: https://apps.bea.gov/API/signup/index.cfm
#' @param start_year The year that the data should start. The earliest data is from 2000.
#' @param end_year The year that the data should end. The latest data is from 2020. 
#' @return The real gdp for a year in a state to the desired years
#' @export

gdp_cur_bea <- function(api_key ='', start_year = 0000, end_year = 9999) {
  
  dataset_name <- "Regional"
  dataset <- "CAGDP1"
  line_code <- '3'
  
  
  print('Making URL...')
  
  url <- make_get_url(dataset_name
                      , dataset
                      , line_code
                      , api_key)
  
  
  print('URL made!')
  print('Making API call...')
  
  req <- call_to_list(url)
  
  
  print('Call made! Response received.')
  print('Formatting JSON to a dataframe...')
  
  dat <- as.data.frame(req$BEAAPI$Results$Data[1])
  for (j in 2:length(req$BEAAPI$Results$Data)) {
    # Append the API result to the data frame
    r <- as.data.frame(req$BEAAPI$Results$Data[j])
    dat <- dplyr::bind_rows(dat,r)
  }
  
  print('Formatting complete!')
  print('Aggregating to state and year...')
  
  dat <- agg_state_year(dat, 'gdp')
  
  print('Filtering to desired year...')
  dat <- filter_year(data = dat, start = start_year, end = end_year)
  dat <- ST_to_State(dat)
  print('Finished!')
  return(dat)
}

#' @title ST_to_State
#' @param dataframe a df with a column named "state"
#' @return The same dataframe but with the full statename
ST_to_State < function(dataframe) {
  state_name <- append(append(append(append(append
                (append(append(append(append(state.name, 'American Samoa'), 'Guam'),
                'Marshall Islands'), 'Micronesia'), 'Northern Mariana Islands'),'Palau'),
                'Puerto Rico'), 'Virgin Islands'), 'District of Columbia')
  
  state_abb <- append(append(append(append(append(
               append(append(append(append(state.abb,
               'AS'),'GU'),'MH'),'FM'),'MP'),'PW'),'PR'),'VI'), 'DC')
  
  fixed_df <- dataframe |> dplyr::mutate(state = state_name[match(dataframe$state, state_abb)])
  return(fixed_df)
} 


