
# function to get table "2.2.5" for each of the four years.
read_sdo_check <- function(x, y, year) {
  
  raw_data <- readxl::read_xlsx(path = y,
                                sheet = x, skip = 3)
  names(raw_data)
  
  raw_data <- raw_data %>%
    dplyr::rename(MDC_class = "MDC")
  
  raw_data <- raw_data %>% 
    tidyr::drop_na() %>%
    dplyr::mutate(Year = year)%>% 
    slice(-n()) 
  
    
    return(raw_data)
  
}


# example
file_path <- "./data/C_17_tavole_34_2_0_file.xlsx" 
raw_sheets <- readxl::excel_sheets(file_path)[64]

read_sdo_check(x = raw_sheets, y = file_path, year = "2019") 


# get the table to match for all the 4 files.
files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

sheets_names <- list("Tav_2.2.5", "Tav_2.2.5", "Tav_2.2.5", "Tav_2.2.5")
Years <- list("2016", "2017", "2018", "2019")

matching_tables <- 
  purrr::pmap(list(sheets_names, files_names, Years), read_sdo_check) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Attività = "Acuti - Regime ordinario") %>%
  tibble::view()


