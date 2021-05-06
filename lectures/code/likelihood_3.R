

library(tidyverse)
library(stats)
library(broom)
library(estimatr)
library(haven)

dblue = rgb(0, 114, 178, max = 255)
dred = rgb(213,94,0, max = 255)
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

ggplot(data = corelogic_data %>% filter(!is.na(duration)),
       aes(x = duration/12)) +
  geom_step(aes(y = 1 - ..y..), stat='ecdf') +
  labs(x = "Duration in years",
       y = "",
       subtitle = "Survival probability") +
  theme_minimal() +
  theme(text = element_text(size=24)) +
  xlim(0,30)

ggsave("images/housing_duration_full_naive_survival.png")

library(survminer)
library(condsurv)
library(survival)
model = survfit(Surv(time, status) ~ 1, 
                data = corelogic_data %>% filter(!is.na(duration)) %>%
                  mutate(time = duration/12) %>%
                  mutate(status = case_when(fym == 692 ~ 1,
                                            TRUE ~ 2) ))
ggplot(data = corelogic_data %>% filter(!is.na(duration)),
       aes(x = duration/12)) +
  geom_step(aes(y = 1 - ..y..), stat='ecdf', color = dred) +
  labs(x = "Duration in years",
       y = "",
       subtitle = "Survival probability") +
  theme_minimal() +
  theme(text = element_text(size=24)) +
  xlim(0,30) + 
  geom_step(data = tibble(y = model$surv, x = model$time), 
            aes(y = y, x = x), color=dblue)  +
  annotate("text", x = 25, y = 0.38, 
           hjust = 0, size = 7, color = dblue, 
           label = "Kaplan-Meier")  +
  annotate("text", x = 25, y = 0.05, 
           hjust = 0, size = 7, 
           color = dred, label = "Unadjusted")

ggsave("images/housing_duration_full_naive_survival_v_KM.png")

model_boom = survfit(Surv(time, status) ~ 1, 
                data = corelogic_data %>% filter(!is.na(duration)) %>%
                  mutate(time = duration/12) %>%
                  mutate(status = case_when(fym == 692 ~ 1,
                                            TRUE ~ 2) ) %>%
                  filter(year > 2000 & year < 2008))
model_bust = survfit(Surv(time, status) ~ 1, 
                      data = corelogic_data %>% filter(!is.na(duration)) %>%
                        mutate(time = duration/12) %>%
                        mutate(status = case_when(fym == 692 ~ 1,
                                                  TRUE ~ 2) )%>%
                       filter(year > 2007 & year < 2011))
ggplot(data = corelogic_data %>% filter(!is.na(duration)),
       aes(x = duration/12)) +
  labs(x = "Duration in years",
       y = "",
       subtitle = "Survival probability") +
  theme_minimal() +
  theme(text = element_text(size=24)) +
  xlim(0,18) + 
  geom_step(data = tibble(y = model_boom$surv, x = model_boom$time), 
            aes(y = y, x = x), color=dblue)  +
  geom_step(data = tibble(y = model_bust$surv, x = model_bust$time), 
            aes(y = y, x = x), color=dred)  +
  annotate("text", x = 15, y = 0.48, 
           hjust = 0, size = 7, color = dblue, 
           label = "Boom - 2000-2006")  +
  annotate("text", x = 10, y = 0.64, 
           hjust = 0, size = 7, 
           color = dred, label = "Bust - 2007-2010")
ggsave("images/housing_duration_survival_KM_boom_bust.png")

boom_data = tibble(y = model_boom$surv, x = model_boom$time) %>%
  mutate(f = lag(y)-y) %>%
  mutate(f = case_when(is.na(f) ~ 1-y,
                       TRUE ~ f)) %>%
  mutate(h = f/y)
bust_data = tibble(y = model_bust$surv, x = model_bust$time) %>%
  mutate(f = lag(y)-y) %>%
  mutate(f = case_when(is.na(f) ~ 1-y,
                       TRUE ~ f)) %>%
  mutate(h = f/y)
ggplot(data = corelogic_data %>% filter(!is.na(duration)),
       aes(x = duration/12)) +
  labs(x = "Duration in years",
       y = "",
       subtitle = "Hazard rate") +
  theme_minimal() +
  theme(text = element_text(size=24)) +
  xlim(0,18) + 
  geom_step(data = boom_data, 
            aes(y = h, x = x), color=dblue)  +
  geom_step(data = bust_data, 
            aes(y = h, x = x), color=dred)  +
  annotate("text", x = 15, y = 0.0, 
           hjust = 0, size = 7, color = dblue, 
           label = "Boom - 2000-2006")  +
  annotate("text", x = 10, y = 0.0, 
           hjust = 0, size = 7, 
           color = dred, label = "Bust - 2007-2010")
ggsave("images/housing_duration_hazard_KM_boom_bust.png")


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


