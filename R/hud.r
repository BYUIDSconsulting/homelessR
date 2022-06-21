#' @title download hud data
#' @import openxlsx
#' @param y - The year that you want to find data. Current avalable years are between 2007 - 2020
#' @param url1 - The URL pulls from hud dataset of number of homelessness by state

hud_data <- function(y, url1= 'https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-PIT-Counts-by-State.xlsx'){
  hud = openxlsx::read.xlsx(url1,sheet=y)
  names(hud) <- gsub(".", " ", names(hud), fixed = TRUE)
  hud$Year <- as.numeric(y)
  names(hud) = gsub(pattern = ",.*", replacement = "", x = names(hud))
  hud <- hud[-57,]
}



#' @title Gather data from 2007 - 2020
#' @examples df <- gather_hud_data()
#' @import dplyr
#' @export 

gather_hud_data <- function(){
  da2007 <- hud_data('2007')
  columns <-  colnames(da2007)
  da2008 <- dplyr::select(hud_data('2008'),columns)
  da2009 <- dplyr::select(hud_data('2009'),columns)
  da2010 <- dplyr::select(hud_data('2010'),columns)
  da2011 <- dplyr::select(hud_data('2011'),columns)
  da2012 <- dplyr::select(hud_data('2012'),columns)
  da2013 <- dplyr::select(hud_data('2013'),columns)
  da2014 <- dplyr::select(hud_data('2014'),columns)
  da2015 <- dplyr::select(hud_data('2015'),columns)
  da2016 <- dplyr::select(hud_data('2016'),columns)
  da2017 <- dplyr::select(hud_data('2017'),columns)
  da2018 <- dplyr::select(hud_data('2018'),columns)
  da2019 <- dplyr::select(hud_data('2019'),columns)
  da2020 <- dplyr::select(hud_data('2020'),columns)
  
  
  result <- rbind(da2007, da2008, da2009, da2010, da2011, da2012, da2013,
                  da2014, da2015, da2016, da2017, da2018, da2019, da2020) |>
    dplyr::rename(state = State)
}

