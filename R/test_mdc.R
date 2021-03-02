# create a test to check data collected in with read_sdo() and overall summary in 2.2.5 
# it tests the total discharge for each MDC class collected in 2.2.6, with 2.2.5

test_mdc <- function(year, data, matching_table){
  
  extracted_table <- data %>% 
    dplyr::group_by(CLASSE_MDC, ANNO) %>%
    dplyr::filter(ANNO == year) %>%
    dplyr::mutate(CLASSE_MDC = stringr::str_remove_all(string = CLASSE_MDC,
                                                       pattern = "\\(|\\)|MDC ")) %>%
    dplyr::summarise(DIMISSIONI = sum(DIMISSIONI)) 
  
  table_to_match <- matching_table %>%
    dplyr::filter(ANNO == year) %>%  
    dplyr::mutate(CLASSE_MDC = stringr::str_remove_all(string = CLASSE_MDC, pattern = "\\(|\\)|MDC ")) %>%
    dplyr::select(CLASSE_MDC, DIMISSIONI, ANNO) 
  
  test_table =dplyr::left_join(x = extracted_table, y = table_to_match, by = "CLASSE_MDC") 
  test = test_table$DIMISSIONI.x %% test_table$DIMISSIONI.y
  
  # returns TRUE if the all the reminders are 0, FALSE otherwise.
  ifelse(all(test == 0), yes = TRUE, no = FALSE)
  
}

purrr::map(Years, test_mdc, data = merged_data, matching_table = matching_tables)
