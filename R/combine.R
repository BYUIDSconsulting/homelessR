#' @import dplyr
#' @title get_everything
#' @author Hunter Rogers
#' @example get_everything()
#' @export
get_everything <- function(cen_api){
  
  establish_census_api(cen_api)
  
  data <- get_census_data()
  print('1 is good')
  data2 <- hud_data
  print('2 is good')
  data3 <- total_employment_data
  print('3 is good')
  data4 <- gdp_data
  print('4 is good')
  data5 <- get_url()
  print('5 is good')
  data6 <- unemploy_rate_data
  print('6 is good')
  data7 <- get_census_data(table='B05001')
  print('7 is good')
  
  data12 <- merge(x=data,y=data2,by=c("state","Year"), all=TRUE)
  print('1 is good')
  data123 <- merge(x=data12,y=data3,by=c("state","Year"), all=TRUE)
  print('2 is good')
  data1234 <- merge(x=data123,y=data4,by=c("state","Year"), all=TRUE)
  print('3 is good')
  data12345 <- merge(x=data1234,y=data5,by=c("state","Year"), all=TRUE)
  print('4 is good')
  data123456 <- merge(x=data12345,y=data6,by=c("state","Year"), all=TRUE)
  print('5 is good')
  data1234567 <- merge(x=data123456,y=data7,by=c("state","Year"), all=TRUE)
  print('6 is good')

  
  
  return(data1234567)
}