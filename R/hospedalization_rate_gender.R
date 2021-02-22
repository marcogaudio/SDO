
# Tavola 5.1 - Tassi di ospedalizzazione per regione, tipo attività,
# regime di ricovero e genere (per 1.000 abitanti) 

hosp_reader_gender <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y,
                                sheet = x,
                                skip = 3) 
  raw_data1 <- raw_data %>% 
    dplyr::rename(MASCHI = "ACUTI",
           FEMMINE = "...3") %>%
    dplyr::select(`REGIONE DI RESIDENZA`, MASCHI, FEMMINE) %>%
    dplyr::mutate(Attività = "Acuti - Regime ordinario") %>% 
    select(where(~!all(is.na(.x)))) %>%
    tidyr::drop_na() %>%
    dplyr::slice(-n()) 
  
  raw_data2 <- raw_data %>% 
    dplyr::rename(MASCHI = "...4",
                  FEMMINE = "...5") %>%
    dplyr::select(`REGIONE DI RESIDENZA`, MASCHI, FEMMINE) %>%
    dplyr::mutate(Attività = "Acuti - Regime diurno") %>% 
    select(where(~!all(is.na(.x)))) %>%
    tidyr::drop_na() %>%
    dplyr::slice(-n()) 
  
  raw_data3 <- raw_data %>% 
    dplyr::rename(MASCHI = "RIABILITAZIONE",
                  FEMMINE = "...8") %>%
    dplyr::select(`REGIONE DI RESIDENZA`, MASCHI, FEMMINE) %>%
    dplyr::mutate(Attività = "Riabilitazione - Regime ordinario") %>% 
    select(where(~!all(is.na(.x)))) %>%
    tidyr::drop_na() %>%
    dplyr::slice(-n()) 
  
  raw_data4 <- raw_data %>% 
    dplyr::rename(MASCHI = "...9",
                  FEMMINE = "...10") %>%
    dplyr::select(`REGIONE DI RESIDENZA`, MASCHI, FEMMINE) %>%
    dplyr::mutate(Attività = "Riabilitazione - Regime diurno") %>% 
    select(where(~!all(is.na(.x)))) %>%
    tidyr::drop_na() %>%
    dplyr::slice(-n()) 
  
  raw_data5 <- raw_data %>% 
    dplyr::rename(MASCHI = "LUNGODEGENZA",
                  FEMMINE = "...13") %>%
    dplyr::select(`REGIONE DI RESIDENZA`, MASCHI, FEMMINE) %>%
    dplyr::mutate(Attività = "Lungodegenza") %>% 
    select(where(~!all(is.na(.x)))) %>%
    tidyr::drop_na() %>%
    dplyr::slice(-n()) 
  
  raw_data <- dplyr::bind_rows(raw_data1, raw_data2,
                               raw_data3, raw_data4, 
                               raw_data5) %>% 
    dplyr::mutate(Year = year)
  
}

# apply for all years
files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

sheet_names = rep(list("Tav_5.1"), 4)

HR_gender <- purrr::pmap(list(sheet_names, files_names, Years), 
                                    hosp_reader_gender) %>%
  dplyr::bind_rows()
view(HR_gender)




