
library(tidyverse)
library(readxl)
library(haven)
library(binsreg)


data = read_dta("~/Dropbox/GPW/data/ACS_microdata_full/ACS_microdata_full_55_75.dta")

data = data %>% zap_labels() %>% mutate(has_health_insurance = hcovany == 2)

data_sample = data %>% sample_n(100000)

binsreg(data_sample$has_health_insurance, data_sample$hhincome)
