# in case of multiple page for one MDC, the MDC string starts with "segue..."
# delete row starting with "Il valore soglia, specifico per ciascun DRG"
# regex

# recall to change the file name in read_sdo()
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
    dplyr::mutate(MDC_class = stringr::str_remove_all(string = MDC_class, pattern = "\\(|\\)|Segue ")) %>%
    dplyr::mutate(Year = year) %>%
    
  return(raw_data)

}

# inside r project work with relative path instead of absolute

file_path <- "./data/C_17_tavole_34_2_0_file.xlsx" 
raw_sheets <- readxl::excel_sheets(file_path)[65:86]

# as example
# raw_data <- readxl::read_xlsx(path = file_path,
#                              sheet = raw_sheets[2], skip = 3)
# examples
# cleaned_data <- read_sdo(x = raw_sheets[2], y = file_path, year = "2019")
# cleaned_data2 <- read_sdo(x = raw_sheets[21], y = file_path, year = "2019")


# get all the .xlsx files in the data folder.
files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

# get the sheets name of all the 4 files
all_raw_sheets <- map(files_names, excel_sheets)


sheets_name_2016 <- c(all_raw_sheets[[1]][27:48])   
sheets_name_2017 <- c(all_raw_sheets[[2]][65:86]) 
sheets_name_2018 <- c(all_raw_sheets[[3]][65:86])  
sheets_name_2019 <- c(all_raw_sheets[[4]][65:86])  


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


all_cleaned_datas <- pmap(list(list_a, list_b, list_c), read_sdo)

merged_data <- dplyr::bind_rows(all_cleaned_datas) %>%
  dplyr::mutate(id_row = NULL) %>%
  dplyr::mutate(Attività = "Acuti - Regime ordinario")

extracted_table <- merged_data %>% 
  group_by(MDC_class, Year) %>%
  filter(Year == 2016) %>%
  dplyr::mutate(MDC_class = stringr::str_remove_all(string = MDC_class, pattern = "\\(|\\)|MDC ")) %>%
  summarise(DIMISSIONI =sum(DIMISSIONI)) 
  
view(extracted_table)
  
extracted_table <- extracted_table %>% 
  rename(DIMISSIONI = "sum(DIMISSIONI)") %>% view()

test = left_join(x = extracted_table, y = tomatch, by = "MDC_class") %>%view()

if(all(test$DIMISSIONI.x%%test$DIMISSIONI.y == 0, na.rm = TRUE )) 

df1 <- tibble(x = 1:3)
df2 <- tibble(x = c(1, 1, 2), y = c("first", "second", "third"))
df1 %>% left_join(df2)

