

library(tidyverse)
library(stats)
library(broom)
library(estimatr)
library(haven)

dblue = rgb(0, 114, 178, max = 255)
acs_data <- read_csv("~/Dropbox/Papers/GGP_HoldingLength/data/usa_00014.csv")
acs_data <- usa_00014
corelogic_data = read_dta("~/Dropbox/Papers/GGP_HoldingLength/data/holding_data_sample100k.dta")

sample = acs_data %>% filter(YEAR == 2019) %>% select(OWNERSHP, AGE, RACE, INCTOT, HHWT)
sample_10k = sample %>% sample_n(10000) %>% mutate(owner = OWNERSHP == 1,
                                                   age = AGE,
                                                   age_sq = age^2,
                                                   income = INCTOT / 10000) %>% 
  filter(age > 18) %>% filter(INCTOT > 0) 

ggplot(data = corelogic_data %>% filter(!is.na(duration))) +
  geom_histogram(aes(x = duration/12, after_stat(density)), fill=dblue,) + 
  labs(x = "Duration in years",
       y = "",
       subtitle = "Distribution of housing duration") +
  theme_minimal() +
  theme(text = element_text(size=24)) +
  xlim(0,30)
ggsave("images/housing_duration_full.png")


ggplot(data = corelogic_data %>% filter(!is.na(duration) & year >= 2010)) +
  geom_histogram(aes(x = duration/12, after_stat(density)), fill=dblue,) + 
  labs(x = "Duration in years",
       y = "",
       subtitle = "Distribution of housing duration for 2010+ cohort") +
  theme_minimal() +
  theme(text = element_text(size=24)) +
  xlim(0,30)
ggsave("images/housing_duration_2010plus.png")

ggplot(data = corelogic_data %>% filter(!is.na(duration) & year >= 2005)) +
  geom_histogram(aes(x = duration/12, after_stat(density)), fill=dblue,) + 
  labs(x = "Duration in years",
       y = "",
       subtitle = "Distribution of housing duration for 2005+ cohort") +
  theme_minimal() +
  theme(text = element_text(size=24)) +
  xlim(0,30)
ggsave("images/housing_duration_2005plus.png")

ggplot(data = corelogic_data %>% filter(!is.na(duration) & year >= 2000 & year <=2008)) +
  geom_histogram(aes(x = duration/12, after_stat(density)), fill=dblue) + 
  labs(x = "Duration in years",
       y = "",
       subtitle = "Distribution of housing duration for 2000-2008 cohorts") +
  theme_minimal() +
  theme(text = element_text(size=24)) +
  xlim(0,20) +
  facet_wrap(~year)
ggsave("images/housing_duration_20002010_byyear.png")

ggplot(data = corelogic_data %>% filter(!is.na(duration) & year >= 2000 & year <=2008)) +
  stat_ecdf(aes(x = duration/12, group=year, color=as.factor(year))) + 
  labs(x = "Duration in years",
       y = "",
       subtitle = "Distribution of housing duration for 2000-2008 cohorts",
       color = "Purchase Year") +
  theme_minimal() +
  theme(text = element_text(size=24),
        legend.position = c(0.2,.7)) +
  xlim(0,20) 
ggsave("images/housing_duration_20002010_byyear_cdf.png")


