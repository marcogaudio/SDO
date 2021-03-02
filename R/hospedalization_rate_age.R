# read tables: 5.11, 5.13, 5.15, 5.17, 5.19 (they are similar)

hosp_reader_age <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y, sheet = x,
                                skip = 3) 

  raw_data <- raw_data %>% 
    dplyr::filter(!is.na(`REGIONE DI RESIDENZA`)) %>% 
    tidyr::drop_na() %>% 
    dplyr::slice(-n()) %>% 
    dplyr::mutate(ANNO = year) %>%
    dplyr::mutate(TAVOLA = x) 
  
}

Years <- c("2016", "2017", "2018", "2019")

# write a function to automate previous code.

sdo_autom <- function(table, activity, function_name){
  
  list_sheets <- rep(list(table),4)
  sdo_table <- purrr::pmap(list(list_sheets, files_names, Years),
                          function_name) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(ATTIVITÀ = activity)
  
}

table_list <- list("Tav_5.11","Tav_5.13","Tav_5.15", "Tav_5.17", "Tav_5.19")

activity_list <- list("Acuti - Regime ordinario", "Acuti - Regime diurno",
                      "Riabilitazione - Regime ordinario", 
                      "Riabilitazione - Regime diurno",
                      "Lungodegenza")


HR_age <- purrr::pmap(list(table_list, activity_list), sdo_autom, 
                      function_name = hosp_reader_age) %>%
  dplyr::bind_rows() %>%
  view()

# sheet_names = rep(list("Tav_5.11"), 4)
# 
# hosp_rate_5.11 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Acuti - Regime ordinario") 
# 
# 
# sheet_names = rep(list("Tav_5.13"), 4)
# 
# hosp_rate_5.13 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Acuti - Regime diurno") 
# 
# sheet_names = rep(list("Tav_5.15"), 4)
# 
# hosp_rate_5.15 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Riabilitazione - Regime ordinario")
# 
# 
# sheet_names = rep(list("Tav_5.17"), 4)
# 
# hosp_rate_5.17 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Riabilitazione - Regime diurno")     
#   
# 
# sheet_names = rep(list("Tav_5.19"), 4)
# 
# hosp_rate_5.19 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Lungodegenza")  
#   
# 
# HR_age_old <- dplyr::bind_rows(hosp_rate_5.11, hosp_rate_5.13, hosp_rate_5.15,
#                            hosp_rate_5.17, hosp_rate_5.19)



