
library(tidyverse)
library(readxl)
library(haven)
library(binsreg)




binsreg(data_sample$has_health_insurance, data_sample$inctot)
