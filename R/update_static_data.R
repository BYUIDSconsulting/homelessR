# testing out the homelessR package


#Hud Data
### WORKS ###
# hud_data <- homelessR::gather_hud_data()
# usethis::use_data(hud_data, overwrite = TRUE)

dat <- hud_data
View(hud_data)

# Crime Data
### ###
crime_data <- homelessR::get_url()
usethis::use_data(crime_data, overwrite = TRUE)

dat2 <- crime_data
View(dat2)
