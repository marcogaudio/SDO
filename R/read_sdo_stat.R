# create a function to read from table 2.2.7 to table 2.2.7(17)

library(tidyverse)
library(readxl)

read_sdo_stat <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y, sheet = x, skip = 3)
  
  raw_data <- raw_data %>%  
    tidyr::drop_na() %>% 
    dplyr::mutate(Year = year) %>%
    dplyr::rename(SOGLIA = paste0("Soglia ", year)) %>%
    dplyr::mutate(Sheet = x)
  return(raw_data)
}

# Previous code.

# files_names = list.files(path = "./data/", pattern = ".xlsx",
#                          full.names = TRUE)
# 
# all_raw_sheets <- purrr::map(files_names, excel_sheets)
# 
# sheets_name_2016 <- c(all_raw_sheets[[1]][49:65])   
# sheets_name_2017 <- c(all_raw_sheets[[2]][87:103]) 
# sheets_name_2018 <- c(all_raw_sheets[[3]][87:103])  
# sheets_name_2019 <- c(all_raw_sheets[[4]][87:103])  
# 
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

# use function sdo_grid() to automate stuff.
files_names <- as.list(list.files(path = "./data/", pattern = ".xlsx",
                                  full.names = TRUE))


# apply the function for all the 4 years.

Years <- list("2016", "2017", "2018", "2019")

all_raw_sheets <- as.character(readxl::excel_sheets(path = files_names[[1]]))

sheet_names <- all_raw_sheets[str_detect(all_raw_sheets, "Tav_2.2.7")]

grid_list <- pmap(list(files_names, Years), sdo_grid) %>%
  bind_rows()

# function pmap takes on a list of lists.
list_a <- as.list(as.character(grid_list$Var1))
list_b <- as.list(as.character(grid_list$Var2))
list_c <- as.list(as.character(grid_list$Var3))


all_cleaned_datas_2_7 <- purrr::pmap(list(list_a, list_b, list_c), read_sdo_stat)

merged_data_2_7 <- dplyr::bind_rows(all_cleaned_datas_2_7) %>%
  dplyr::rename(code1 = "DRG",
                type = "Tipo DRG",
                DRG = "Descrizione") 


# join tables 2.2.6 with tables 2.2.7
SDO_ordinario <- dplyr::left_join(x = merged_data, y = merged_data_2_7, 
                                 by = c("DRG", "Year", "type", "code1")) %>%
  dplyr::select(-"Degenza media") %>% # there is two time
  tidyr::unite(Sheet, Sheet.x, Sheet.y, remove = TRUE, sep = "/") %>%
  view()

