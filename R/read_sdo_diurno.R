# function to read from table 2.2.12 to table 2.2.12(17)

read_sdo_diurno <- function(x, y, year) {
  # x: excel sheet name
  # y: file path
  raw_data <- readxl::read_xlsx(path = y,
                                sheet = x, skip = 3) 
  
  raw_data <- raw_data %>%
    dplyr::rename(type  = "Tipo DRG",
                  code1 = "DRG",
                  DRG = "Descrizione")
  
  raw_data <- raw_data %>% 
    tidyr::drop_na() %>%
    dplyr::mutate(Year = year) %>%
    dplyr::mutate(Attività = "Acuti - Regime diurno")
  
  return(raw_data)
  
}

# get the sheets name of all the 4 files
all_raw_sheets <- map(files_names, excel_sheets)


sheets_name_2016 <- c(all_raw_sheets[[1]][73:89])   
sheets_name_2017 <- c(all_raw_sheets[[2]][111:127]) 
sheets_name_2018 <- c(all_raw_sheets[[3]][111:127])  
sheets_name_2019 <- c(all_raw_sheets[[4]][111:127])  


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


all_cleaned_datas <- pmap(list(list_a, list_b, list_c), read_sdo_diurno)

merged_data_2.12 <- dplyr::bind_rows(all_cleaned_datas) %>% 
  view()


# functions names as table numbers???
# multiple scripts?
# syntax ok?

