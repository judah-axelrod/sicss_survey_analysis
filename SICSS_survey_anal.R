library(tidyverse)
library(haven)
setwd('~/Desktop/SICSS')
brit_elec <- read_dta('bes_rps_2019_1.1.1 2.dta') %>% as_tibble()

breakdown <- brit_elec %>% 
  select(gender = y09, edlevel, ethnicity = y11, region = Q19_CSES)
