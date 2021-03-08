# function to read tables 5.1
# Tavola 5.1 - Tassi di ospedalizzazione per regione, tipo attività,
# regime di ricovero e genere (per 1.000 abitanti) 

hosp_reader_gender <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y,
                                sheet = x,
                                skip = 3) 
  
  
  raw_data <- raw_data %>% 
    dplyr::select(where(~!all(is.na(.x))))
  
  ncols <- ncol(raw_data)
  
  index <- list(c(2, 3), c(4,5), c(6,7),
                c(8,9), c(10,11))
  
  raw_reader <- function(data, ind, activity){
    first_col <- ind[1]
    second_col <- ind[2]
    raw_data1 <- data %>% 
      dplyr::rename(MASCHI = first_col,
                    FEMMINE = second_col) %>%
      dplyr::select(`REGIONE DI RESIDENZA`, MASCHI, FEMMINE) %>%
      dplyr::mutate(ATTIVITÀ = activity) %>% 
      tidyr::drop_na() 
    
  }
  
  activity_list <- list("Acuti - Regime ordinario", "Acuti - Regime diurno",
                        "Riabilitazione - Regime ordinario", 
                        "Riabilitazione - Regime diurno",
                        "Lungodegenza")
  
  list_sdo <- purrr::pmap(list(index, activity_list), raw_reader, data = raw_data)
  
  output_data <- dplyr::bind_rows(list_sdo) %>% 
    dplyr::mutate(ANNO = year) %>%
    dplyr::mutate(TAVOLA = "Tav_5.1")
  
  return(output_data)
}

# apply for all years
files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

sheet_names = rep(list("Tav_5.1"), 4)

Years <- c("2016", "2017", "2018", "2019")

HR_gender <- purrr::pmap(list(sheet_names, files_names, Years), 
                         hosp_reader_gender) %>%
  dplyr::bind_rows()
view(HR_gender)
