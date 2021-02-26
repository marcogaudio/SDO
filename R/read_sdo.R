# in case of multiple page for one MDC, the MDC string starts with "segue..."
# delete row starting with "Il valore soglia, specifico per ciascun DRG"
# regex

# function to read from table 2.2.6 to table 2.2.6(22) (Acuti regime ordinario per DRG)
library(tidyverse)
library(readxl)

read_sdo <- function(x, y, year) {
  
  raw_data <- readxl::read_xlsx(path = y,
                                sheet = x, skip = 3)
  names(raw_data)
  
  raw_data <- raw_data %>%
    dplyr::rename(code1 = "...1",
                  type = "...2") %>%
    dplyr::mutate(id_row = 1:n()) %>% 
    dplyr::filter(!is.na(code1)) 
  
  
  MDC_class <- raw_data %>%
    dplyr::select(code1, type, id_row) %>%
    dplyr::filter(is.na(type)) %>%
    dplyr::pull(code1)
  MDC_class_start_row <- raw_data %>%
    dplyr::select(code1, type, id_row) %>%
    dplyr::filter(is.na(type)) %>%
    dplyr::pull(id_row)
  
  raw_data <- raw_data %>%
    dplyr::mutate(MDC_class = if_else(id_row < MDC_class_start_row[2], MDC_class[1],
                                      if_else(id_row < MDC_class_start_row[3], MDC_class[2],
                                              if_else(id_row < MDC_class_start_row[4], MDC_class[3], MDC_class[4])))) %>% 
    dplyr::filter(!is.na(type)) %>% 
    dplyr::mutate(MDC_class = stringr::str_remove_all(string = MDC_class, pattern = "\\(|\\)|Segue ")) %>%
    dplyr::mutate(Year = year) %>% 
    dplyr::mutate(Sheet = x)
    
  return(raw_data)

}

# get all the .xlsx files in the data folder.

files_names <- as.list(list.files(path = "./data/", pattern = ".xlsx",
                                  full.names = TRUE))


# apply the function for all the 4 years.

Years <- list("2016", "2017", "2018", "2019")

all_raw_sheets <- as.character(readxl::excel_sheets(path = files_names[[1]]))

sheet_names <- all_raw_sheets[str_detect(all_raw_sheets, "Tav_2.2.6")]

# write a function to replace the previous code.
# the function will expand the grid, then use pmap to apply for all the years.
sdo_grid <- function(file, year){
  
  grid <- expand.grid(sheet_names, file, year)
  
}

grid_list <- pmap(list(files_names, Years), sdo_grid) %>%
  bind_rows()

# previous code

# all_raw_sheets <- map(files_names, excel_sheets)
# 
# 
# sheets_name_2016 <- c(all_raw_sheets[[1]][27:48])   
# sheets_name_2017 <- c(all_raw_sheets[[2]][65:86]) 
# sheets_name_2018 <- c(all_raw_sheets[[3]][65:86])  
# sheets_name_2019 <- c(all_raw_sheets[[4]][65:86])  
# 
# Years <- c("2016", "2017", "2018", "2019")
# 
# first_grid  <- expand.grid(sheets_name_2016, files_names[1], Years[1])
# second_grid <- expand.grid(sheets_name_2017, files_names[2], Years[2])
# third_grid  <- expand.grid(sheets_name_2018, files_names[3], Years[3])
# fourth_grid <- expand.grid(sheets_name_2019, files_names[4], Years[4])
# 
# total_grid <- dplyr::bind_rows(first_grid,
#                                second_grid,
#                                third_grid,
#                                fourth_grid)

# function pmap takes on a list of lists.
# 
# list_a <- as.list(as.character(total_grid$Var1))
# list_b <- as.list(as.character(total_grid$Var2))
# list_c <- as.list(as.character(total_grid$Var3))

list_a <- as.list(as.character(grid_list$Var1))
list_b <- as.list(as.character(grid_list$Var2))
list_c <- as.list(as.character(grid_list$Var3))

all_cleaned_datas <- pmap(list(list_a, list_b, list_c), read_sdo)

merged_data <- dplyr::bind_rows(all_cleaned_datas) %>%
  dplyr::mutate(id_row = NULL) %>%
  dplyr::mutate(Attività = "Acuti - Regime ordinario")
