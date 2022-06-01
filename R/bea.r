#' @title call_to_list
#' @param call_url
call_to_list <- function(call_url){
  results <- GET(call_url)
  return(fromJSON(as.character(results)))
}

#' @title make_get_url
#' @param dataset_name, dataset, line_code, api_key

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

#' @title tot_employ
#' @param 
tot_employ <- function(api_key ='') {
  
  dataset_name <- "Regional"
  dataset <- "CAEMP25N"
  line_code <- '10'
  
  url <- base_get_data_url(dataset_name
                           , dataset
                           , line_code
                           , api_key)
  
  req <- get_call_to_list(url)
  dat <- as.data.frame(req$BEAAPI$Results$Data[1])
  for (j in 2:length(req$BEAAPI$Results$Data)) {
    # Append the API result to the data frame
    r <- as.data.frame(req$BEAAPI$Results$Data[j])
    dat <- bind_rows(dat,r)
  }
  return(dat)
}
