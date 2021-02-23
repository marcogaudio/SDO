# write a function to read from table 8.1Pie to table 8.1Sar


read_sdo_region <- function(x, y, year, region){
  
  raw_data <- readxl::read_xlsx(path = y, sheet = x, skip = 2)
  
  raw_data <- raw_data %>% 
    dplyr::rename(code1 = "...1",
                  type = "...2") %>% 
    dplyr::filter(!is.na(code1)) %>%
    dplyr::mutate(id_row = 1:n()) %>%
    dplyr::slice(-n()) 
  
  MDC_class <- raw_data %>%
    dplyr::select(code1, type, id_row) %>%
    dplyr::filter(is.na(type)) %>%
    dplyr::pull(code1)
  
  MDC_class_start_row <- raw_data %>%
    dplyr::select(code1, type, id_row) %>%
    dplyr::filter(is.na(type)) %>%
    dplyr::pull(id_row)
  # function to get the MDC classes names
  MDC_name <- function(start_id_row, mdc_class){
    
    x <- replicate(start_id_row, mdc_class)
    return(x)
  }
  
  nreps <- diff(MDC_class_start_row)
  
  list_idrow<- as.list(nreps)
  list_mdc <- as.list(MDC_class)[-length(MDC_class)]
  
  MDC_names <- purrr::pmap(list(list_idrow, list_mdc), MDC_name) %>%
    unlist() 
  
  MDC_names <- c(MDC_names, rep(tail(MDC_class, n = 1), 10))
  
  raw_data <- raw_data %>% dplyr::mutate(MDC_class = MDC_names) %>%
    dplyr::filter(!is.na(type)) %>% 
    dplyr::mutate(Year = year) %>% 
    dplyr::mutate(Sheet = x) %>%
    dplyr::mutate(Regione = region)
  
  
  
}

# get the names of regions.
Regioni <- c(HR_age$`REGIONE DI RESIDENZA`[1:21])

# apply the function for all the 4 years.

Years <- c("2016", "2017", "2018", "2019")

all_raw_sheets <- map(files_names, excel_sheets)


sheets_name_2016 <- c(all_raw_sheets[[1]][317:337])   
sheets_name_2017 <- c(all_raw_sheets[[2]][358:378]) 
sheets_name_2018 <- c(all_raw_sheets[[3]][358:378])  
sheets_name_2019 <- c(all_raw_sheets[[4]][358:378])  


first_grid  <- cbind(expand.grid(sheets_name_2016, files_names[1], Years[1]), Regioni)
second_grid <- cbind(expand.grid(sheets_name_2017, files_names[2], Years[2]), Regioni)
third_grid  <- cbind(expand.grid(sheets_name_2018, files_names[3], Years[3]), Regioni)
fourth_grid <- cbind(expand.grid(sheets_name_2019, files_names[4], Years[4]), Regioni)

total_grid <- dplyr::bind_rows(first_grid,
                               second_grid,
                               third_grid,
                               fourth_grid)

# function pmap takes on a list of lists.

list_a <- as.list(as.character(total_grid$Var1))
list_b <- as.list(as.character(total_grid$Var2))
list_c <- as.list(as.character(total_grid$Var3))
list_d <- as.list(as.character(total_grid$Regioni))

region_datas <- purrr::pmap(list(list_a, list_b, list_c, list_d), read_sdo_region)

SDO_ordinario_regioni <- dplyr::bind_rows(region_datas) %>%
  dplyr::mutate(id_row = NULL) %>%
  dplyr::mutate(Attività = "Acuti - Regime ordinario") %>%
  view()





