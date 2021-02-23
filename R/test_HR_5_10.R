# test hospedalization rate
# use the 2018 population ?


# write a function for hospedalization rates testing (considering table 5.10).
# test the hospedalization rates per Attività.

test_HR <- function(sdo_table, population, checking_table, year){
  
  # extract attività name for sdo_table, used then to extract result from\
  # checking table.
  attività <- sdo_table %>%
    slice(1) %>%
    select(Attività) %>%
    as.character()
     
    result <- checking_table %>% 
    filter(Year == year , checking_table[,1] == "TOTALE") %>%
      select(attività) %>% 
      as.numeric() %>%
      round(digits = 2)
  
  tot_dimissions <- sdo_table %>% 
    filter(Year == year) %>%
    summarise(sum(DIMISSIONI))%>%
    as.numeric()
  
  HR_test_rate <- round((tot_dimissions / population) * 1000, digits = 2)
  
  if_else(HR_test_rate == result, TRUE, FALSE)
  return(c(result, HR_test_rate))
}


pop2017 <- 60589445
pop2016 <- 60665551
pop2018 <- 60483973
pop2019 <- 59816673

test_HR(sdo_table = SDO_ordinario, checking_table = HR_age_cat, year = "2019",
        population = pop2019)

