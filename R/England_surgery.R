# read table "HES crude results" for the England number of surgeries and surgical incidence.

file_path <- list.files( path = "./data/UK", pattern = ".xlsx",
                         full.names = TRUE)

raw_data <- readxl::read_xlsx(path = file_path[1], 
                              sheet = "HES crude results", skip = 3)
view(raw_data)



# the following tibble represents the total number of surgery in England(2019)
# is the year correct? 

eng_surgery <- raw_data %>%
  dplyr::slice(1:18) %>%
  dplyr::select(1:3) %>%
  dplyr::rename(`AGE CATEGORY` = "Age...1",
                MALE = "Male...2",
                FEMALE = "Female...3") %>%
  dplyr::mutate(YEAR = "2019")


# the following tibble represents the surgical incidences in England(2019)

eng_surgery_incidence <- raw_data %>%
  dplyr::slice(1:18) %>%
  dplyr::select(5:7) %>%
  dplyr::rename(`AGE CATEGORY` = "Age...5",
                MALE = "Male...6",
                FEMALE = "Female...7") %>%
  dplyr::mutate(YEAR = "2019")


