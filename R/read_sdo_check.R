

read_sdo_MDC <- function(x, y, year) {
  
  raw_data <- readxl::read_xlsx(path = y,
                                sheet = x, skip = 3)
  names(raw_data)
  
  raw_data <- raw_data %>% 
    tidyr::drop_na() %>%
    mutate(Year = year) 
    return(raw_data)
  
}


# example
file_path <- "./data/C_17_tavole_34_2_0_file.xlsx" 
raw_sheets <- readxl::excel_sheets(file_path)[64]

read_sdo_MDC(x = raw_sheets, y = file_path, year = "2019") %>% view()


# get the table to match for all the 4 files.
files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

sheets_names <- list("Tav_2.2.5", "Tav_2.2.5", "Tav_2.2.5", "Tav_2.2.5")
Years <- list("2016", "2017", "2018", "2019")

matching_tables <- 
  purrr::pmap(list(sheets_names, files_names, Years), read_sdo_MDC) %>%
  dplyr::bind_rows(matching_tables) %>%
  dplyr::mutate(Attività = "Acuti - Regime ordinario") %>%
  tibble::view()

