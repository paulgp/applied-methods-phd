

library(tidyverse)
library(permute)
dblue = rgb(0,114,178,maxColorValue = 255)
dred = rgb(213,94,0,maxColorValue = 255)

n = 10
reps = 1000
Y1 = round(rnorm(10, mean = 10), digits = 1)
Y0 = round(rnorm(10, mean = 8), digits = 1)
data = tibble(D = c(rep(1,5), rep(0,5)), Y1 = Y1, Y0 = Y0, tau = Y1 - Y0)

loop = shuffleSet(n=n)
tau_est = rep(0, dim(loop)[1])
data_obs = data
data_obs = data_obs %>% mutate(Y0  = case_when(D == 1 ~ Y1,
                                    TRUE ~ Y0)) %>%
  mutate(Y1  = case_when(D == 0 ~ Y0,
                             TRUE ~ Y1))
for (i in seq(1,dim(loop)[1])) {
  D_est = data_obs$D[loop[i,]]
  tau_est[i] = 0.5*sum(D_est * data_obs$Y1) - 0.5*sum((1-D_est) * data_obs$Y0)
}
tau_true = 0.5*sum(data$D * data$Y1) - 0.5*sum((1-data$D) * data$Y0)
ggplot(data =tibble(tau_est = tau_est), aes(x = tau_est)) +
  geom_histogram(binwidth =0.1, fill = dblue, color = "black") +
  theme_minimal() +
  geom_vline(xintercept=tau_true) +
  labs(x = "Estimated Differences under Strong Null",
       y = "")+
  theme(text =element_text(size = 30))
ggsave("../lectures/images/randomized2.pdf")

share_treated = rep(0, reps)
for (i in seq(1,reps)) {
  D = as.numeric(runif(n) > 0.5)
  n_treat = sum(D == 1)
  share_treated[i] = n_treat/n
}

ggplot(data =tibble(share_treated = share_treated), aes(x = share_treated)) +
  geom_histogram(binwidth =0.1, fill = dblue, color = "black") +
  theme_minimal() +
  labs(x = "Share Population Treated",
       y = "")+
  theme(text =element_text(size = 30))
ggsave("../lectures/images/randomized1.pdf")
