

library(readr)
library(tidyverse)
library(lubridate)
library(stringr)
library(furrr)
library(caret) 
library(glmnet) 
library(doParallel)
library(rpart)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

files <- c("tbFA_QuickSearch_SMART_II.zip")
data_path <- "/Users/psg24/Dropbox/Papers/GGP_Brokers/MLSRawData/"
i = 1
fn <- paste0(data_path, files[i])

df2 <- read_csv(paste0(strsplit(fn, "\\.")[[1]][1], "_parsed", ".csv"))

reg_data = df2 %>% filter(list_year > 2009 & list_year < 2017, fa_rent_sale_ind == "S") %>%
  mutate(sold = as.numeric((fa_liststatus=="S"))) %>%
  select(-fa_rent_sale_ind) %>%
  mutate( log_price = log(closeprice)) %>%
  mutate(baths = as.factor(fa_bathstotal),
         sqft = fa_squarefeet,
         sqft_per = sqft/listprice,
         age = list_year - fa_yearbuilt,
         property_type = as.factor(fa_propertytype),
         pool = poolyn == "Y") %>%
  filter(listprice != 0 ) %>%
  select(sold, list_year, age, sqft, baths, sqft_per)  %>%
  filter(!is.na(age) & !is.na(sqft))  %>%
  mutate_if(is.character,as.numeric) %>%
  as_data_frame()
  
set.seed(123) 
(cv_model1 <- train(
  form = sold ~ sqft_per, 
  data = reg_data[complete.cases(reg_data),],
  method = "lm",
  na.action = 'na.pass',
  trControl = trainControl(method = "cv", number = 10)
))
(cv_model2 <- train(
  form = sold ~ sqft_per + age + sqft + baths, 
  data = reg_data[complete.cases(reg_data),],
  method = "lm",
  na.action = 'na.pass',
  trControl = trainControl(method = "cv", number = 10)
))
(cv_model3 <- train(
  form = sold ~ sqft_per + age + sqft + baths, 
  data = reg_data[complete.cases(reg_data),],
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10)
))
(cv_model4 <- train(
  form = sold ~ sqft_per + age + sqft + baths, 
  data = reg_data[complete.cases(reg_data),],
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10)
))


summary(resamples(list(
  model1 = cv_model1, 
  model2 = cv_model2,
  model_elastic = cv_model3,
  model_tree = cv_model4,
  model_rf = cv_model5
)))
