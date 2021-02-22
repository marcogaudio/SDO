# read tables: 5.11, 5.13, 5.15, 5.17, 5.19 (they are similar)
# find a way to add a column with ATTIVITA'

hosp_reader_age <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y, sheet = x,
                                skip = 3) 

  raw_data <- raw_data %>% 
    dplyr::filter(!is.na(`REGIONE DI RESIDENZA`)) %>% 
    tidyr::drop_na() %>% 
    dplyr::slice(-n()) %>% 
    dplyr::mutate(Year = year) %>%
    dplyr::mutate(Sheet = x) 
  
}

sheet_names = list("Tav_5.11","Tav_5.13","Tav_5.15",
                       "Tav_5.17", "Tav_5.19")

Years <- c("2016", "2017", "2018", "2019")

first_grid  <- expand.grid(sheet_names, files_names[1], Years[1])
second_grid <- expand.grid(sheet_names, files_names[2], Years[2])
third_grid  <- expand.grid(sheet_names, files_names[3], Years[3])
fourth_grid <- expand.grid(sheet_names, files_names[4], Years[4])

total_grid <- dplyr::bind_rows(first_grid,
                               second_grid,
                               third_grid,
                               fourth_grid)

# function pmap takes on a list of lists.

list_a <- as.list(as.character(total_grid$Var1))
list_b <- as.list(as.character(total_grid$Var2))
list_c <- as.list(as.character(total_grid$Var3))


hosp_rate <- purrr::pmap(list(list_a, list_b, list_c), hosp_reader_age) %>%
  dplyr::bind_rows() %>% view()
