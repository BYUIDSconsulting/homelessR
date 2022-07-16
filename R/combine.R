#' @import dplyr
#' @title get_everything
#' @author Hunter Rogers
#' @example get_everything()
#' @export
get_everything <- function(cen_api, bea_api){
  
  establish_census_api(cen_api)
  
  data <- get_census_data()
  data2 <- gather_hud_data()
  data3 <- tot_employ_bea(bea_api)
  data4 <- gdp_cur_bea(bea_api)
  data5 <- get_url()
  data6 <- unemployment()
  #data7 <- get_census_data(table='B04003')
  data <- get_census_data(table='B05001')
  
  data12 <- merge(x=data,y=data2,by=c("state","Year"), all=TRUE)
  data123 <- merge(x=data12,y=data3,by=c("state","Year"), all=TRUE)
  data1234 <- merge(x=data123,y=data4,by=c("state","Year"), all=TRUE)
  data12345 <- merge(x=data1234,y=data5,by=c("state","Year"), all=TRUE)
  data123456 <- merge(x=data12345,y=data6,by=c("state","Year"), all=TRUE)
  data1234567 <- merge(x=data123456,y=data7,by=c("state","Year"), all=TRUE)
  #data12345678 <- merge(x=data1234567,y=data8,by=c("state","Year"), all=TRUE)
  
  
  return(data1234567)
}