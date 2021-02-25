# function to read from table 6.7 to table 6.11 (dimissioni per fasce di età e genere)


read_sdo_gender_age <- function(x, y, year){
  
  raw_data <- readxl::read_xlsx(path = y, sheet = x, skip = 4)
  
  
  raw_data <- raw_data %>% 
    dplyr::rename("Categoria età" = "...1",
                  MASCHI = "Maschi...2",
                  FEMMINE = "Femmine...3") 
  
  raw_data <- raw_data %>% 
    dplyr::select(1:3) %>%
    tidyr::drop_na() %>%
    dplyr::slice(-n()) %>%
    dplyr::mutate(Year = year, Sheet = x)
  
}

# read tables for all the 4 years, then merge them

files_names = list.files(path = "./data/", pattern = ".xlsx",
                         full.names = TRUE)

Years <- c("2016", "2017", "2018", "2019")

# Use the function sdo_autom() to automate previous code.

table_list <- list("Tav_6.7","Tav_6.8","Tav_6.9", "Tav_6.10", "Tav_6.11")

activity_list <- list("Acuti - Regime ordinario", "Acuti - Regime diurno",
                      "Riabilitazione - Regime ordinario", 
                      "Riabilitazione - Regime diurno",
                      "Lungodegenza")

sdo_age_gender <- purrr::pmap(list(table_list, activity_list), sdo_autom, 
                              function_name = read_sdo_gender_age) %>%
  dplyr::bind_rows() %>%
  view()

# # Tables 6.7
# 
# sheet_names = rep(list("Tav_6.7"),4)
# 
# sdo_AeG_ordinario <- purrr::pmap(list(sheet_names, files_names, Years),
#                                  read_sdo_gender_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Acuti - Regime ordinario")
# 
# # Tables 6.8
# 
# sheet_names = rep(list("Tav_6.8"),4)
# 
# sdo_AeG_diurno <- purrr::pmap(list(sheet_names, files_names, Years),
#                                  read_sdo_gender_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Acuti - Regime diurno")
# 
# # Tables 6.9
# 
# sheet_names = rep(list("Tav_6.9"),4)
# 
# sdo_AeG_riab_ord <- purrr::pmap(list(sheet_names, files_names, Years),
#                               read_sdo_gender_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Riabilitazione - Regime ordinario")
# 
# # Tables 6.10
# 
# sheet_names = rep(list("Tav_6.10"),4)
# 
# sdo_AeG_riab_diurno <- purrr::pmap(list(sheet_names, files_names, Years),
#                                 read_sdo_gender_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Riabilitazione - Regime diurno")
# 
# # Tables 6.11
# 
# sheet_names = rep(list("Tav_6.11"),4)
# 
# sdo_AeG_lungodegenza <- purrr::pmap(list(sheet_names, files_names, Years),
#                                 read_sdo_gender_age) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(Attività = "Lungodegenza")
# 
sdo_age_gender <- dplyr::bind_rows(sdo_AeG_ordinario,
                            sdo_AeG_diurno,
                            sdo_AeG_riab_ord,
                            sdo_AeG_riab_diurno,
                            sdo_AeG_lungodegenza)
view(sdo_age_gender)






