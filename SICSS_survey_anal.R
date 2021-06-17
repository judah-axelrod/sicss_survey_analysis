library(tidyverse)
library(haven)
setwd('~/Desktop/SICSS/sicss_survey_analysis')
brit_elec <- read_dta('bes_rps_2019_1.1.1 2.dta') %>% as_tibble()

#############################
# Calculate group means for
# original data
############################
breakdown <- brit_elec %>% 
  select(
         # gender = y09, region = Q19_CSES, age=Age,
         x01_1, x01_2, x01_3, x01_4, x01_5, x01_6, b01, b02, b04) %>%
  summarise_all(~mean(., na.rm = T)) %>%
  pivot_longer(x01_1:b04, names_to='qid', values_to='mean') #These are the response means in the original survey
  


########################
#Our survey results
########################
prolific_data <- read_csv('sample_results.csv')

weights <- prolific_data %>% 
  count(SC_age, SC_region, SC_gender, SC_ethnicity, SC_education) %>%
  mutate(weight = n/sum(n))

prolific_data <- prolific_data %>%
  tail(-2) %>%
  select(polknowledge_1:B02, pid1) %>%
  mutate_at(.vars = vars(polknowledge_1:polknowledge_6),
            .funs = funs(case_when(. == TRUE ~ 1,
                              . == FALSE ~ 2,
                              TRUE ~ -1))) %>%
  mutate(B01 = case_when(
    B01 == "Yes, I voted." ~ 1,
    B01 == "No, I did not vote" ~ 2 #(to be continued)
  ))

                