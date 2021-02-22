# 2- Riabilitazione 2.3.6 and 2.3.7
# 3- Lungodegenze 2.3.8

# function read_sdo_check() can be re-used.

files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

Years <- list("2016", "2017", "2018", "2019")

sheets_names <- rep(list("Tav_2.3.6"), 4)


#Riabilitazione - regime ordinario

SDO_ordinario_riab <- 
  purrr::pmap(list(sheets_names, files_names, Years), read_sdo_check) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Attività = "Riabilitazione - Regime ordinario") 


#Riabilitazione - regime diurno

sheets_names <- rep(list("Tav_2.3.7"), 4)

SDO_diurno_riab <- 
  purrr::pmap(list(sheets_names, files_names, Years), read_sdo_check) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Attività = "Riabilitazione - Regime diurno") 

#Lungodegenza

sheets_names <- rep(list("Tav_2.3.8"), 4)

SDO_lungodegenza <- 
  purrr::pmap(list(sheets_names, files_names, Years), read_sdo_check) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Attività = "Lungodegenza") 

