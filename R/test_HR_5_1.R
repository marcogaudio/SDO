# considere 5.1 (MALE and FEMALE)

test_HR_5.1 <- function(year, male_pop, female_pop, 
                        attività, checking_table, sdo_table){
  
  # extract attività name for sdo_table, used then to extract result from\
  # checking table.
  
  result <- checking_table %>% 
    dplyr::filter(Year == year , checking_table[,1] == "ITALIA",
                  Attività == attività) %>%
    dplyr::select(MASCHI, FEMMINE) %>% 
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
  
  
  HR_test_male <- round((Male / male_pop) * 1000, digits = 2)
  HR_test_female <- round((Female / female_pop) * 1000, digits = 2)
  
  HR_test = c(HR_test_male, HR_test_female)
  
  #if_else(HR_test == result, TRUE, FALSE)
  difference <- (HR_test / result) - 1 
  return(c(result, HR_test, difference))
}

# source: istat
male2019 <- 29131195
female2019 <- 30685478

male2018 <- 29427607
female2018 <- 31056366

male2017 <- 29445741
female2017 <- 31143704

male2016 <- 29456321
female2016 <- 31209230

test_HR_5.1(year = "2018", male_pop = male2018, female_pop = female2018,
            sdo_table = sdo_age_gender, checking_table = HR_gender, 
            attività = "Acuti - Regime diurno")

activity_list <- list("Acuti - Regime ordinario", "Acuti - Regime diurno",
                      "Riabilitazione - Regime ordinario", 
                      "Riabilitazione - Regime diurno",
                      "Lungodegenza")

pop_list_m <- list(male2016, male2017, male2018, male2019)
pop_list_f <- list(female2016, female2017, female2018, female2019)

Years <- list("2016", "2017", "2018", "2019")


purrr::pmap(list(Years, pop_list_m, pop_list_f), test_HR_5.1, 
            sdo_table = sdo_age_gender, checking_table = HR_gender, 
            attività = "Acuti - Regime ordinario")

purrr::pmap(list(Years, pop_list_m, pop_list_f), test_HR_5.1, 
            sdo_table = sdo_age_gender, checking_table = HR_gender,
            attività = "Acuti - Regime diurno")

purrr::pmap(list(Years, pop_list_m, pop_list_f), test_HR_5.1, 
            sdo_table = sdo_age_gender, checking_table = HR_gender,
            attività = "Riabilitazione - Regime ordinario")

purrr::pmap(list(Years, pop_list_m, pop_list_f), test_HR_5.1, 
            sdo_table = sdo_age_gender, checking_table = HR_gender,
            attività = "Riabilitazione - Regime diurno")


purrr::pmap(list(Years, pop_list_m, pop_list_f), test_HR_5.1, 
            sdo_table = sdo_age_gender, checking_table = HR_gender,
            attività = "Lungodegenza")
