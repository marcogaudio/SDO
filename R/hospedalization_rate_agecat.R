# read tables: 5.12, 5.14, 5.16, 5.18, 5.20 (they are similar)
# find a way to add a column with ATTIVITA'

hosp_reader_agecat <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y, sheet = x,
                                skip = 3) 
  
  raw_data <- raw_data %>% select(where(~!all(is.na(.)))) %>% 
    drop_na() %>% 
    slice(-n()) 
  
  name <- names(raw_data)
  
  
  age_index <- which(stringr::str_detect(string = name, pattern = "ann"))
  age_categ <- name[age_index] 
  m <- length(age_index)
  
  list_tibbles <- rep(list(raw_data), m)
  list_index <- as.list(age_index)

    splitting <- function(data, index){
      splitted_data <- data %>% select(1, index:(index + 1))
      return(splitted_data)
    }
    
  all_data <- purrr::pmap(list(list_tibbles, list_index), splitting)
 
  # add age category
  age_cat_list <- as.list(age_categ)
  
  all_data1 <- purrr::pmap(list(all_data, age_cat_list), function(x, y) add_column(x, Age_category = y))
  
  # changing column names to all tibbles, then appending them
  column_names <- c("REGIONE DI RESIDENZA",
                    "MASCHI", "FEMMINE", 
                    "CATEGORIA ETÀ")
  
  renames <- function(x, y){
    
    colnames(x) <- y
    
    return(x)
  }
  
  list_column_names <- rep(list(column_names), 8)
  all_data1 <- purrr::pmap(list(all_data1, list_column_names), renames)
  
  all_data1 <- all_data1 %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Year = year)
  
}

# apply for all years
files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

sheet_names = list("Tav_5.12", "Tav_5.14", "Tav_5.16",
                       "Tav_5.18", "Tav_5.20")

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


hospedalization_rate_agecat <-purrr::pmap(list(list_a, list_b, list_c), hosp_reader_agecat) %>%
  dplyr::bind_rows() %>% view()



