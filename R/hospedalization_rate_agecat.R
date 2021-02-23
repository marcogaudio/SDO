# function to read tables 5.10
# Tavola 5.10 - Tassi di ospedalizzazione per fasce di età, 
# tipo attività e regime di ricovero (per 1.000 abitanti) 

hosp_reader_5.10 <- function(x, y, year) {
  
  raw_data <- readxl::read_xlsx(path = y,
                                sheet = x,
                                skip = 4) 
  raw_data <- raw_data %>%
    select(where(~!all(is.na(.x)))) %>%
    tidyr::drop_na() 
  
  columns <- c("Categoria età",
               "Acuti - Regime ordinario",
               "Acuti - Regime diurno",
               "Riabilitazione - Regime ordinario",
               "Riabilitazione - Regime diurno",
               "Lungodegenza")
  
  colnames(raw_data) <- columns 
  
  raw_data <- raw_data %>% 
    dplyr::mutate(Year = year, Sheet = x)
  
}


sheet_names <- rep(list("Tav_5.10"), 4)

files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

Years <- c("2016", "2017", "2018", "2019")

HR_age_cat <- purrr::pmap(list(sheet_names, files_names, Years), 
                         hosp_reader_5.10) %>%
  dplyr::bind_rows()
view(HR_age_cat)



