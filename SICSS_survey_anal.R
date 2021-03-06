library(tidyverse)
library(haven)
setwd('~/Desktop/SICSS/sicss_survey_analysis')
brit_elec <- read_dta('bes_rps_2019_1.1.1 2.dta') %>% as_tibble()

#############################
# Calculate group means for
# original data
############################


brit_elec_recoded <- brit_elec %>%
mutate(
    b01 = case_when(
      b01 == 1 ~ 1,
      TRUE ~ 0),
    b02 = case_when(
      b02 == 2 ~ 1,
      TRUE ~ 0
    ),
    x01_1 = case_when(
      x01_1 == 1 ~ 1,
      TRUE ~ 0),
    x01_2 = case_when(
      x01_2 == 1 ~ 1,
      TRUE ~ 0),
    x01_3 = case_when(
      x01_3 == 2 ~ 1,
      TRUE ~ 0),
    x01_4 = case_when(
      x01_4 == 2 ~ 1,
      TRUE ~ 0),
    x01_5 = case_when(
      x01_5 == 1 ~ 1,
      TRUE ~ 0),
    x01_6 = case_when(
      x01_6 == 2 ~ 1,
      TRUE ~ 0),
    pol_score = x01_1 + x01_2 + x01_3 + x01_4 + x01_5 + x01_6
  )

brit_elec_recoded$Q24_CSES <- ifelse(brit_elec_recoded$Q24_CSES < 31, 1,
                                     ifelse(brit_elec_recoded$Q24_CSES > 30 & 
                                              brit_elec_recoded$Q24_CSES < 61, 2, 3))

breakdown <- brit_elec_recoded %>% 
  select(
         # gender = y09, region = Q19_CSES, age=Age,
         x01_1, x01_2, x01_3, x01_4, x01_5, x01_6, b01, b02, b04) %>%
  summarise_all(~mean(., na.rm = T)) %>%
  pivot_longer(x01_1:b04, names_to='qid', values_to='mean') #These are the response means in the original survey
  
weights_brit_elec <- brit_elec_recoded %>% 
  tail(-2) %>%
  select(Q24_CSES, Q19_CSES, Q23_CSES) %>%
  filter(Q24_CSES > -2 & Q19_CSES > -2 & Q23_CSES  > -2) %>%
  count(Q24_CSES, Q19_CSES, Q23_CSES)%>%
  mutate(weight = n/sum(n))


########################
#Our survey results
########################
prolific_data <- read_csv('SICSS-survey_June 17, 2021_15.16.csv')

prolific_data <- tail(prolific_data, -61)

prolific_data_recoded <- prolific_data %>%
  tail(-2) %>%
  select(polknowledge_1:B02, SC_region, SC_gender, SC_age) %>%
  mutate(
    B01 = case_when(
      B01 == "Yes, I voted." ~ 1,
      TRUE ~ 0),
    B02 = case_when(
      B02 == "Conservative Party" ~ 1,
      TRUE ~ 0
    ),
    SC_region = case_when(
      SC_region == "England" ~ 1,
      SC_region == "Scotland" ~ 2,
      SC_region == "Wales" ~ -3,
      SC_region == "Northern Ireland" ~ 4,
      SC_region == "Other" ~5,
      TRUE ~ -999
    ),
    SC_gender = case_when(
      SC_gender == "Male" ~ 1,
      SC_gender == "Female" ~ 2,
      SC_gender == " In another way" ~ 3,
      SC_gender == "Prefer not to say" ~ 4,
      TRUE ~ -999
    ),
      polknowledge_1 = case_when(
        polknowledge_1 == 'True' ~ 1,
        TRUE ~ 0),
      polknowledge_2 = case_when(
        polknowledge_2 == 'True' ~ 1,
        TRUE ~ 0),
      polknowledge_3 = case_when(
        polknowledge_3 == 'False' ~ 1,
        TRUE ~ 0),
      polknowledge_4 = case_when(
        polknowledge_4 == 'False' ~ 1,
        TRUE ~ 0),
      polknowledge_5 = case_when(
        polknowledge_5 == 'True' ~ 1,
        TRUE ~ 0),
      polknowledge_6 = case_when(
        polknowledge_6 == 'False' ~ 1,
        TRUE ~ 0),
      pol_score = polknowledge_1 + polknowledge_2 + polknowledge_3 + polknowledge_4 + polknowledge_5 + polknowledge_6)
  


prolific_data_recoded$SC_age <- ifelse(prolific_data_recoded$SC_age < 31, 1,
                                     ifelse(prolific_data_recoded$SC_age > 30 & 
                                              prolific_data_recoded$SC_age < 61, 2, 3))

weights <- prolific_data_recoded %>% 
  tail(-2) %>%
  count(SC_age, SC_region, SC_gender) %>%
  mutate(weight = n/sum(n))

prolific_merge <- bind_cols(prolific_data, weights)
                
