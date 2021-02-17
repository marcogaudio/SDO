library(tidyverse)

# cambiare nome colonna soglia 2019/2018/2017/2016?
y <- "./data/C_17_tavole_34_2_0_file.xlsx" 
x <- readxl::excel_sheets(file_path)[87]

read_sdo_stat <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y, sheet = x, skip = 3)
  
  raw_data <- raw_data %>%  tidyr::drop_na() %>% 
    dplyr::mutate(Year = year) %>%
    rename(SOGLIA = paste0("Soglia ", year))
  return(raw_data)
}

files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

all_raw_sheets <- map(files_names, excel_sheets)

sheets_name_2016 <- c(all_raw_sheets[[1]][49:65])   
sheets_name_2017 <- c(all_raw_sheets[[2]][87:103]) 
sheets_name_2018 <- c(all_raw_sheets[[3]][87:103])  
sheets_name_2019 <- c(all_raw_sheets[[4]][87:103])  


Years <- c("2016", "2017", "2018", "2019")

first_grid  <- expand.grid(sheets_name_2016, files_names[1], Years[1])
second_grid <- expand.grid(sheets_name_2017, files_names[2], Years[2])
third_grid  <- expand.grid(sheets_name_2018, files_names[3], Years[3])
fourth_grid <- expand.grid(sheets_name_2019, files_names[4], Years[4])

total_grid <- dplyr::bind_rows(first_grid,
                               second_grid,
                               third_grid,
                               fourth_grid)

# function pmap takes on a list of lists.
list_a <- as.list(as.character(total_grid$Var1))
list_b <- as.list(as.character(total_grid$Var2))
list_c <- as.list(as.character(total_grid$Var3))


all_cleaned_datas_2_7 <- pmap(list(list_a, list_b, list_c), read_sdo_stat)
merged_data_2_7 <- dplyr::bind_rows(all_cleaned_datas_2_7) %>%
  view()


