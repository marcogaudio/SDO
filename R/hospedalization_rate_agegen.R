# read tables: 5.12, 5.14, 5.16, 5.18, 5.20 (they are similar)
# Tasso di ospedalizzazione per fasce di età e genere 
# (per 1.000 abitanti) Attività per Acuti in Regime ordinario 
hosp_reader_agegen <- function(x, y, year){
  
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
    dplyr::mutate(ANNO = year)
  
}

# apply for all years
files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)


Years <- c("2016", "2017", "2018", "2019")

# use the function sdo_autom() to automate previous code.

table_list <- list("Tav_5.12","Tav_5.14","Tav_5.16", "Tav_5.18", "Tav_5.20")

activity_list <- list("Acuti - Regime ordinario", "Acuti - Regime diurno",
                      "Riabilitazione - Regime ordinario", 
                      "Riabilitazione - Regime diurno",
                      "Lungodegenza")

HR_agecat_gen <- purrr::pmap(list(table_list, activity_list), sdo_autom, 
                              function_name = hosp_reader_agegen ) %>%
  dplyr::bind_rows() %>%
  view()


# sheet_names = rep(list("Tav_5.12"), 4)
# 
# hosp_rate_5.12 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_agegen) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Acuti - Regime ordinario") 
# 
# 
# sheet_names = rep(list("Tav_5.14"), 4)
# 
# hosp_rate_5.14 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_agegen) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Acuti - Regime diurno") 
# 
# sheet_names = rep(list("Tav_5.16"), 4)
# 
# hosp_rate_5.16 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_agegen) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Riabilitazione - Regime ordinario")
# 
# 
# sheet_names = rep(list("Tav_5.18"), 4)
# 
# hosp_rate_5.18 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_agegen) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Riabilitazione - Regime diurno")     
#   
#   
# sheet_names = rep(list("Tav_5.20"), 4)
# 
# hosp_rate_5.20 <- purrr::pmap(list(sheet_names, files_names, Years), hosp_reader_agegen) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Lungodegenza")  
# 
# HR_agecat_gen_old <- dplyr::bind_rows(hosp_rate_5.12, hosp_rate_5.14, hosp_rate_5.16,
#                               hosp_rate_5.18, hosp_rate_5.20)
# view(HR_agecat_gen)
