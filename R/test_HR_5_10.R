# test hospedalization rate
# use the 2001 population 


# write a function for hospedalization rates testing (considering table 5.10).
# test the hospedalization rates per Attività.

test_HR <- function(year, population, checking_table, sdo_table){
  
  # extract attività name for sdo_table, used then to extract result from\
  # checking table.
  attività <- sdo_table %>%
    dplyr::slice(1) %>%
    dplyr::select(Attività) %>%
    as.character()
     
    result <- checking_table %>% 
    dplyr::filter(Year == year , checking_table[,1] == "TOTALE") %>%
      dplyr::select(attività) %>% 
      as.numeric() %>%
      round(digits = 2)
  
  tot_dimiss <- sdo_table %>% 
    dplyr::filter(Year == year) %>%
    dplyr::summarise(sum(DIMISSIONI))%>%
    as.numeric()
  
  HR_test_rate <- round((tot_dimiss / population) * 1000, digits = 2)
  
  if_else(HR_test_rate == result, TRUE, FALSE)
  # rapporto <- (HR_test_rate / result) - 1 
  # return(c(result, HR_test_rate, rapporto))
}

pop2001 <- 56993742 # source: istat
pop2011 <- 60785753	
test_HR(sdo_table = SDO_diurno_riab, checking_table = HR_age_cat, year = "2019",
        population = pop2011)


# some of them are false, check again
purrr::map(Years, test_HR, sdo_table = SDO_ordinario, 
    checking_table = HR_age_cat, population = pop2011)

purrr::map(Years, test_HR, sdo_table = SDO_diurno, 
    checking_table = HR_age_cat, population = pop2011)

purrr::map(Years, test_HR, sdo_table = SDO_ordinario_riab, 
    checking_table = HR_age_cat, population = pop2011)

purrr::map(Years, test_HR, sdo_table = SDO_diurno_riab, 
    checking_table = HR_age_cat, population = pop2011)

purrr::map(Years, test_HR, sdo_table = SDO_lungodegenza, 
    checking_table = HR_age_cat, population = pop2011)






