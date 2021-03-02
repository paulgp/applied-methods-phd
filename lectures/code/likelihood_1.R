
library(tidyverse)
library(stats)
library(broom)
library(estimatr)

acs_data <- read_csv("~/Dropbox/Papers/GGP_HoldingLength/data/usa_00014.csv")
acs_data <- usa_00014


sample = acs_data %>% filter(YEAR == 2019) %>% select(OWNERSHP, AGE, RACE, INCTOT, HHWT)
sample_10k = sample %>% sample_n(10000) %>% mutate(owner = OWNERSHP == 1,
                                                   age = AGE,
                                                   age_sq = age^2,
                                                   income = INCTOT / 10000) %>% 
  filter(age > 18) %>% filter(INCTOT > 0) 
linear_model = lm_robust(owner ~ age + age_sq + income, 
                         data = sample_10k)
skimr::skim(linear_model$fitted.values)
tidy(linear_model) %>%
  select(term, estimate, std.error) %>% mutate(estimate = round(estimate, digits = 4), std.error = round(std.error,digits=4))

logit = glm(owner ~ age + age_sq + income, 
            data = sample_10k ,
            family = "binomial")
tidy(logit) %>%
  select(term, estimate_logit = estimate) %>% 
  mutate(estimate_logit = round(estimate_logit, digits = 4)) %>%
  left_join(tidy(linear_model) %>%
              select(term, estimate_linear = estimate) %>% 
              mutate(estimate_linear = round(estimate_linear, digits = 4))
  ) %>%
  left_join(tidy(logit) %>% select(term, estimate_avg_deriv =estimate) %>%
              mutate(estimate_avg_deriv = estimate_avg_deriv * (mean(logit$fitted.values * (1-logit$fitted.values)))))

ggplot(data = tibble(linear = linear_model$fitted.values,
                     logit  = logit$fitted.values)) +
  geom_point(aes(y = linear, x = logit), alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal() +
  labs(x = "Logit Fitted Values",
       y = "",
       subtitle = "Linear Fitted Values") +
  theme(text = element_text(size = 32))
ggsave("images/linear_v_logit_scatter.png")

ggplot(data = tibble(logit = rlogis(5000), normal = rnorm(5000)) %>%
         gather(model, number)) +
  geom_density(aes(x = number, fill = model), alpha = 0.2) +
  theme_minimal() +
  labs(x = "Error Draw",
       y = "",
       fill = "Model") +
  theme(legend.position = c(.8,.8),
        text = element_text(size=32))
ggsave("images/logit_v_normal.png")

