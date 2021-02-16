# in case of multiple page for one MDC, the MDC string starts with "segue..."
# delete row starting with "Il valore soglia, specifico per ciascun DRG"
# regex


library(tidyverse)
library(readxl)

x <- raw_sheets[2]
read_sdo <- function(x) {
  
  raw_data <- readxl::read_xlsx(path = "./data/C_17_tavole_34_2_0_file.xlsx",
                                sheet = x, skip = 3)
  names(raw_data)
  
  raw_data <- raw_data %>%
    dplyr::rename(code1 = "...1",
                  type = "...2") %>%
    dplyr::mutate(id_row = 1:n()) %>% 
    dplyr::filter(!is.na(code1))
  
  raw_data %>%
    dplyr::select(code1, type, id_row)
  
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
    dplyr::mutate(MDC_class = stringr::str_remove_all(string = MDC_class, pattern = "\\(|\\)|Segue "))
  
  return(raw_data)
  
  # MDC_raw <- raw_data %>%
  #   dplyr::select(code1) 
  # 
  # MDC <- MDC_raw%>% 
  #   dplyr::filter(base::nchar(code1) > 20 & base::nchar(code1) < 195) 
  # 
  # MDC_string <- base::as.character(MDC$code1)
  # # get the index position outside the for
  # 
  # if (length(MDC_string) == 2) {
  #   
  #   inds <- function(x) which(MDC_raw == x)
  #   row_indexes <- purrr::map_int(MDC_string,inds)
  #   
  #   
  #   n <- dim(raw_data)[1]
  #   n_index <- length(row_indexes)
  #   
  #   raw_data1 <- raw_data %>%
  #     dplyr::slice(1 : row_indexes[2]) %>%
  #     dplyr::mutate(MDC = MDC_string[1]) %>% 
  #     dplyr::filter(!is.na(type))
  #   
  #     raw_data2 <- raw_data %>%
  #       dplyr::slice(row_indexes[n_index] : n)
  #     raw_data2 <- raw_data2 %>%
  #       dplyr::mutate(MDC = MDC_string[n_index])%>% 
  #       dplyr::filter(!is.na(type))
  #     
  #     return(add_row(raw_data1,raw_data2))
  #   }
  # 
  # 
  # else{
  # raw_data <- raw_data %>%
  #   dplyr::mutate(MDC = MDC_string)
  # 
  # raw_data %>% 
  #   dplyr::filter(!is.na(type)) %>% view()
  # }
}

# inside r project work with relative path instead of absolute

file_path <- "./data/C_17_tavole_34_2_0_file.xlsx" 
raw_sheets <- readxl::excel_sheets(file_path)[65:86]

# as example
raw_data <- readxl::read_xlsx(path = file_path,
                              sheet = raw_sheets[2], skip = 3)
names(raw_data)

cleaned_data <- read_sdo(raw_sheets[2])
cleaned_data <- read_sdo(raw_sheets[21])

# apply the function to all the sheets. (example)

cleaned_datas <- map(raw_sheets,read_sdo)

# get all the .xlsx files in the data folder.
files = paste0("./data/" ,list.files(path = "./data", pattern = ".xlsx"))

all_raw_sheets <- map(files, excel_sheets) 

all_raw_datas <- list(all_raw_sheet[[1]][27:48], #2016  
                      all_raw_sheet[[2]][65:86], #2017
                      all_raw_sheet[[3]][65:86], #2018 
                      all_raw_sheet[[3]][65:86]) #2019

Year <- c("2016", "2017", "2018", "2019")

add_year <- function(year,raw){
  raw_new <- raw %>% tibble::add_column(Year = year) 
}
map(all_raw_datas, ~add_year(., Year))

# add the corresponding year to each tibble.

cleaned_datas_16<- purrr::map(all_raw_datas[[1]], read_sdo) %>%
  purrr::map(~ add_column(.x, Year = "2016"))
cleaned_datas_17<- purrr::map(all_raw_datas[[2]], read_sdo) %>%
  purrr::map(~ add_column(.x, Year = "2017"))
cleaned_datas_18<- purrr::map(all_raw_datas[[3]], read_sdo) %>%
  purrr::map(~ add_column(.x, Year = "2018"))
cleaned_datas_19<- purrr::map(all_raw_datas[[4]], read_sdo) %>%
  purrr::map(~ add_column(.x, Year = "2019"))

# trying to find a more elegant way:
# add the corresponding year to each tibble.
add_clean <- function(raw_lists, years){
  add_clean0 <- function(raw_lists, years){
    
    cleaned <- purrr::map(raw_lists, read_sdo) %>%
      purrr::map(~add_column(.x, Year = years))
    return(cleaned)
  }
  sapply(raw_lists, add_clean0, years)
}
cleaned_datas <- add_clean( raw_lists = all_raw_datas, years = Year)
