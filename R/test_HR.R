# test hospedalization rate

# write a function for hospedalization rates testing (considering table 5.10).
# test the hospedalization rates per Attività.

test_HR_5.10 <- function(year, population, attività, 
                    checking_table, sdo_table){
  
  # extract attività name for sdo_table, used then to extract result from\
  # checking table.
  
  result <- checking_table %>% 
    dplyr::filter(Year == year , checking_table[,1] == "TOTALE") %>%
    dplyr::select(attività) %>% 
    as.numeric() %>%
    round(digits = 2)
  
  Female <- sdo_table %>% 
    filter(Year == year, 
           Attività == attività) %>%
    select(FEMMINE) %>%
    pull() %>%
    sum()
  Male <- sdo_table %>% 
    filter(Year == year, 
           Attività == attività) %>%
    select(MASCHI) %>%
    pull() %>%
    sum()
  
  
  tot_dimiss <- Female + Male #2019 = 5860816
  
  HR_test_rate <- round((tot_dimiss / population) * 1000, digits = 2)
  
  #if_else(HR_test_rate == result, TRUE, FALSE)
  difference <- (HR_test_rate / result) - 1 
  return(c(result, HR_test_rate, difference))
}

# pop2001 <- 56993742 # source: istat
# pop2011 <- 59433744	
# pop2010 <- 60340328
pop2019 <- 59816673
pop2018 <- 60483973
pop2017 <- 60589445
pop2016 <- 60665551

# example
test_HR_5.10(year = "2019", population = pop2019, sdo_table = sdo_age_gender,
        checking_table = HR_age_cat, attività = "Acuti - Regime diurno")

activity_list <- list("Acuti - Regime ordinario", "Acuti - Regime diurno",
                      "Riabilitazione - Regime ordinario", 
                      "Riabilitazione - Regime diurno",
                      "Lungodegenza")

pop_list <- list(pop2016, pop2017, pop2018, pop2019)
Years <- list("2016", "2017", "2018", "2019")


# some of them are false, check again

purrr::pmap(list(Years, pop_list), test_HR_5.10, 
            sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
            attività = "Acuti - Regime ordinario")

purrr::pmap(list(Years, pop_list), test_HR_5.10, 
            sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
            attività = "Acuti - Regime diurno")

purrr::pmap(list(Years, pop_list), test_HR_5.10, 
            sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
            attività = "Riabilitazione - Regime ordinario")

purrr::pmap(list(Years, pop_list), test_HR_5.10, 
            sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
            attività = "Riabilitazione - Regime diurno")


purrr::pmap(list(Years, pop_list), test_HR_5.10, 
            sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
            attività = "Lungodegenza")


# pop 2001
purrr::map(Years, test_HR_5.10, population = pop2001, 
            sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
            attività = "Acuti - Regime diurno")

purrr::map(Years, test_HR_5.10, population = pop2001, 
           sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
           attività = "Acuti - Regime ordinario")

purrr::map(Years, test_HR_5.10, population = pop2001, 
           sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
           attività = "Riabilitazione - Regime ordinario")

purrr::map(Years, test_HR_5.10, population = pop2001, 
           sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
           attività = "Riabilitazione - Regime diurno")

purrr::map(Years, test_HR_5.10, population = pop2001, 
           sdo_table = sdo_age_gender, checking_table = HR_age_cat, 
           attività = "Lungodegenza")
