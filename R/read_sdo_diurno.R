# function to read from table 2.2.12 to table 2.2.12(17)

read_sdo_diurno <- function(x, y, year) {
  # x: excel sheet name
  # y: file path
  raw_data <- readxl::read_xlsx(path = y,
                                sheet = x, skip = 3) 
  
  raw_data <- raw_data %>%
    dplyr::rename(TIPO  = "Tipo DRG",
                  CODICE = "DRG",
                  DRG = "Descrizione")
  
  raw_data <- raw_data %>% 
    tidyr::drop_na() %>%
    dplyr::mutate(ANNO = year) %>%
    dplyr::mutate(ATTIVITÀ = "Acuti - Regime diurno") %>%
    dplyr::mutate(TAVOLA = x)
  
  return(raw_data)
  
}


# use function sdo_grid() to automate stuff.
files_names <- as.list(list.files(path = "./data/", pattern = ".xlsx",
                                  full.names = TRUE))


# apply the function for all the 4 years.

Years <- list("2016", "2017", "2018", "2019")

all_raw_sheets <- as.character(readxl::excel_sheets(path = files_names[[1]]))

sheet_names <- all_raw_sheets[str_detect(all_raw_sheets, "Tav_2.2.12")]

grid_list <- pmap(list(files_names, Years), sdo_grid) %>%
  bind_rows()

# function pmap takes on a list of lists.
list_a <- as.list(as.character(grid_list$Var1))
list_b <- as.list(as.character(grid_list$Var2))
list_c <- as.list(as.character(grid_list$Var3))


all_cleaned_datas <- purrr::pmap(list(list_a, list_b, list_c), read_sdo_diurno)

merged_data_2.12 <- dplyr::bind_rows(all_cleaned_datas) 


