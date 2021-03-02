# read table provived by i.stat related to discharge for age category and 
# discharge for MDC, for 2019 only.


library(tidyverse)
library(readxl)

file_path <- list.files( path = "./data/istat", pattern = ".xlsx",
                         full.names = TRUE)


raw_data <- readxl::read_xlsx(path = file_path[2], 
                              sheet = "sheet1")

istat_discharge <- raw_data %>%
  dplyr::select(6, 8, 10, 12:13, 15) %>%
  dplyr::rename(ANNO = "TIME",
         DIMISSIONI = "Value",
         `AREA DI CITTADINANZA` = "Aree di cittadinanza e principali paesi",
         `CLASSE DI ETÀ` = "Classe di età",
         GENERE = "Sesso",
         `REGIME DI RICOVERO` =  "Regime di ricovero")


# to see the total number of discharge, consider the following for each "attività"
istat_discharge %>%
  dplyr::filter(`AREA DI CITTADINANZA` == "Mondo",
         GENERE == "totale",
         `REGIME DI RICOVERO` == "ordinario",
         `CLASSE DI ETÀ` != "totale") %>%
  dplyr::select(DIMISSIONI) %>%
  dplyr::pull() %>%
  sum()


# consider now the istat file with discharge by 


raw_data2 <- readxl::read_xlsx(path = file_path[1], 
                              sheet = "sheet1")
view(raw_data2)

istat_MDC <- raw_data2 %>%
  dplyr::select(6, 8, 10, 12:13, 15) %>%
  dplyr::rename(ANNO = "TIME",
                DIMISSIONI = "Value",
                `AREA DI CITTADINANZA` = "Aree di cittadinanza e principali paesi",
                `CLASSE MDC` = "Diagnosi principale",
                GENERE = "Sesso",
                `REGIME DI RICOVERO` =  "Regime di ricovero")

istat_MDC %>% dplyr::filter(`AREA DI CITTADINANZA` == "Mondo",
                             GENERE == "totale",
                            `REGIME DI RICOVERO` == "ordinario",
                             `CLASSE MDC` == "tutte le voci") %>%
  dplyr::select(DIMISSIONI) %>%
  dplyr::pull() %>%
  sum()
