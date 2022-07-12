#' @import dplyr
#' @title get_url
#' @param y
#' @author Hunter Rogers
#' @example get_url(2019)
#' @export
get_url <- function(){
  
  data <- get_census_data()
  data2 <- get_hud()
  data3 <- tot_employ_bea()
  data4 <- gdp_cur_bea()
  data5 <- get_url()
  
  data12 = left_join(x=data,y=data2,by=c("state","year"))
  data123 = left_join(x=data12,y=data3,by=c("state","year"))
  data1234 = left_join(x=data123,y=data4,by=c("state","year"))
  data12345 = left_join(x=data1234,y=data5,by=c("state","year"))
  return(data12345)
}