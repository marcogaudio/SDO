# read tables: 5.11, 5.13, 5.15, 5.17, 5.19 (they are similar)
# Tasso di ospedalizzazione per fasce di età 
# (per 1.000 abitanti) - Attività per Acuti in Regime ordinario
hosp_reader_age <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y, sheet = x,
                                skip = 3) 

  raw_data <- raw_data %>% 
    dplyr::filter(!is.na(`REGIONE DI RESIDENZA`)) %>% 
    tidyr::drop_na() %>% 
   
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


