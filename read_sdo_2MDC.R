library(tidyverse)
library(readxl)

read_sdo <- function(x) {
  
  raw_data <- readxl::read_xlsx(path = "C:/Users/marco/github/SDO/C_17_tavole_34_2_0_file.xlsx",
                                sheet = x, skip = 3)
  names(raw_data)
  
  raw_data <- raw_data %>%
    dplyr::rename(code1 = "...1",
                  type = "...2")
  
  MDC_raw <- raw_data %>%
    dplyr::select(code1) 
  
  MDC <- MDC_raw%>% 
    dplyr::filter(base::nchar(code1) > 20 & base::nchar(code1) < 195) 
  MDC_string <- base::as.character(MDC$code1)
  
  if (length(MDC_string) == 2) {
    
    inds <- function(x) which(MDC_raw == x)
    row_indexes <- purrr::map_int(MDC_string,inds)
    
    
    n <- dim(raw_data)[1]
    n_index <- length(row_indexes)
    
    raw_data1 <- raw_data %>%
      dplyr::slice(1 : row_indexes[2]) %>%
      dplyr::mutate(MDC = MDC_string[1]) %>% 
      dplyr::filter(!is.na(type))
    
      raw_data2 <- raw_data %>%
        dplyr::slice(row_indexes[n_index] : n)
      raw_data2 <- raw_data2 %>%
        dplyr::mutate(MDC = MDC_string[n_index])%>% 
        dplyr::filter(!is.na(type))
      return(add_row(raw_data1,raw_data2))
    }
  
  
  else{
  raw_data <- raw_data %>%
    dplyr::mutate(MDC = MDC_string)
  
  raw_data %>% 
    dplyr::filter(!is.na(type)) %>% view()
  }
}

file_path <- "C:/Users/marco/github/SDO/C_17_tavole_34_2_0_file.xlsx" 
raw_sheets <- readxl::excel_sheets(file_path)[65:86]

# as example
raw_data <- readxl::read_xlsx(path = file_path,
                              sheet = raw_sheets[2], skip = 3)
names(raw_data)

cleaned_data <- read_sdo(raw_sheets[2])

