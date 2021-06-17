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
prolific_data <- read_csv('SICSS-survey_June 17, 2021_14.06.csv')

weights <- prolific_data %>% 
  tail(-2) %>%
  count(SC_age, SC_region, SC_gender, SC_ethnicity, SC_education) %>%
  mutate(weight = n/sum(n))

prolific_data <- prolific_data %>%
  tail(-2) %>%
  select(polknowledge_1:B02, pid1) %>%
  mutate_at(.vars = vars(polknowledge_1:polknowledge_6),
            .funs = funs(case_when(. == TRUE ~ 1,
                              . == FALSE ~ 2,
                              TRUE ~ -1))) %>%
  mutate(
    B01 = case_when(
      B01 == "Yes, I voted." ~ 1,
      B01 == "No, I did not vote" ~ 2,
      B01 == "Don't know" ~ -1,
      B01 == "Prefer not to say" ~ -2,
      TRUE ~ -999),
    B02 = case_when(
      B02 == "Labour Party" ~ 1,
      B02 == "Conservative Party" ~ 2,
      B02 == "Liberal Democrats" ~ 3,
      B02 == "Scottish National Party" ~ 4,
      B02 == "Plaid Cymru" ~ 5,
      B02 == "Green Party" ~ 6,
      B02 == "United Kingdom Independence Party (UKIP)" ~ 7,
      B02 == "Brexit Party" ~ 8,
      B02 == "Other" ~ 9,
      B02 == "Prefer not to say" ~ -2,
      B02 == "Don't remember/Don't know" ~ -1,
      TRUE ~ -999
     ),
    pid1 = case_when(
      pid1 == "Labour" ~ 1,
      pid1 == "Conservative" ~ 2,
      pid1 == "Liberal Democrat" ~ 3,
      pid1 == "SNP" ~ 4,
      pid1 == "Plaid Cymru" ~ 5,
      pid1 == "Green" ~ 6,
      pid1 == "Don't know" ~ -1,
      TRUE ~ -999
    )
  )

prolific_merge <- bind_cols(prolific_data, weights)
                