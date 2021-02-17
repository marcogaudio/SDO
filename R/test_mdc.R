#create a test to check data collected in R and overall summary in 2.2.5

test_mdc <- function(year, data, matching_table){
  
  extracted_table <- data %>% 
    group_by(MDC_class, Year) %>%
    filter(Year == year) %>%
    dplyr::mutate(MDC_class = stringr::str_remove_all(string = MDC_class, pattern = "\\(|\\)|MDC ")) %>%
   summarise(DIMISSIONI =sum(DIMISSIONI)) 
  
  table_to_match <- matching_table %>%
    filter(Year == year) %>%  
    dplyr::mutate(MDC_class = stringr::str_remove_all(string = MDC_class, pattern = "\\(|\\)|MDC ")) %>%
    select(MDC_class, DIMISSIONI, Year) 
  
  test_table = left_join(x = extracted_table, y = table_to_match, by = "MDC_class") 
  test = test_table$DIMISSIONI.x %% test_table$DIMISSIONI.y
  
  # returns TRUE if the all the reminders are 0, FALSE otherwise.
  ifelse(all(test == 0), yes = TRUE, no = FALSE)
  
}

map(Years, test_mdc, data = merged_data, matching_table = matching_tables)
