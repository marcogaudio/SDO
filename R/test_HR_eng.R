# check surgical incidence England


check_HR_eng <- function(age_category){
  
 population_female <-  eng_pop2012 %>%
   filter(Categoria_età == age_category,
          Sesso == "femmine") %>%
   pull(popolazione) %>%
   sum()
 
 population_male <-  eng_pop2012 %>%
   dplyr::filter(Categoria_età == age_category,
          Sesso == "maschi") %>%
   dplyr::pull(popolazione) %>%
   sum()
 
 pop <- c(population_male, population_female)
  
 discharges <- eng_surgery %>%
   dplyr::mutate_at(vars(MALE, FEMALE), as.numeric) %>%
   dplyr::filter(`AGE CATEGORY` == if_else(
     stringr::str_detect( string = age_category,
                          pattern = "85"), "85  &  over", paste0(age_category, " Years" ))) %>%
   dplyr::select(MALE, FEMALE) %>%
   as_vector()
 
 
 surgical_incidence <- eng_surgery_incidence %>%
   dplyr::mutate_at(vars(MALE, FEMALE), as.numeric) %>%
   dplyr::filter(`AGE CATEGORY` == if_else(
     stringr::str_detect( string = age_category,
          pattern = "85"), "85  &  over", paste0(age_category, " Years" ))) %>%
   dplyr::select(MALE, FEMALE)
   
 round(discharges / pop, digits = 3) == round(surgical_incidence, digits = 3)
 
 
  
}



age_cat_list <- list("0 - 4", "5 - 9", "10 - 14", "15 - 19", 
                     "20 - 24","25 - 29","30 - 34", "35 - 39",
                     "40 - 44", "45 - 49", "50 - 54", "55 - 59",
                     "60 - 64","65 - 69", "70 - 74","75 - 79",
                     "80 - 84", "85+")


purrr::map(age_cat_list, check_HR_eng)
