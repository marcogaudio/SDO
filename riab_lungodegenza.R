# 2- Riabilitazione 2.3.6 and 2.3.7
# 3- Lungodegenze 2.3.8

# function read_sdo_check() can be re-used.

files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

sheets_names <- rep(list("Tav_2.3.6"), 4)
Years <- list("2016", "2017", "2018", "2019")

#Riabilitazione - regime ordinario

riabilitazione_ord <- 
  purrr::pmap(list(sheets_names, files_names, Years), read_sdo_check) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Attività = "Riabilitazione - Regime ordinario") 


#Riabilitazione - regime diurno

sheets_names <- rep(list("Tav_2.3.7"), 4)

riabilitazione_diurno <- 
  purrr::pmap(list(sheets_names, files_names, Years), read_sdo_check) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Attività = "Riabilitazione - Regime diurno") 

#Lungodegenza

sheets_names <- rep(list("Tav_2.3.8"), 4)

data_lungodegenza <- 
  purrr::pmap(list(sheets_names, files_names, Years), read_sdo_check) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Attività = "Lungodegenza") 

