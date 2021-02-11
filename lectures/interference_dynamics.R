
library(tidyverse)

# Five treatment values
Dvals = seq(1,5)

# Sample Size
n = 100

# 2 potential covariates
X <- tibble(X1=rnorm(n = n, mean = 2), X2=rnorm(n = n, mean = 5))
Y = matrix(nrow = n, ncol = length(Dvals))
for (i in Dvals) {
  Y[,i] = rnorm(n, mean = i, sd = 5) + X$X1*.2
  print(i)
}

D <- sample(rep(Dvals, n/length(Dvals)), size = n, replace = FALSE )
D == 1
Y_obs = matrix(nrow=n, ncol = 1)
for (i in Dvals) {
  Y_obs[D==i] = Y[D==i,i]
  print(i)
}
ggplot(data = tibble(Y_obs = Y_obs[,1], D= D) %>% group_by(D) %>% summarize(Y_obs = mean(Y_obs))) +
  geom_point(aes(y = Y_obs, x = D), size = 3) +
  geom_smooth(aes(y = Y_obs, x= D), method = "lm") +
  annotate("text", x = 2, y=5, label = paste0("b=",round(lm(data =  tibble(Y_obs = Y_obs[,1], D= D), 
                                            formula = Y_obs ~ D)$coef[2], digits = 3)),size = 8)+
  theme_minimal() +
  labs(x = "Treatment D",
       y = "",
       title = "Outcome") +
  theme(text = element_text(size=36))
ggsave("../lectures/images/linear_multivalued.pdf")

ggplot(data = tibble(Y_obs = Y_obs[,1], D= D) %>% group_by(D) %>% summarize(Y_obs = mean(Y_obs)) %>%
         mutate(Y_diff = Y_obs - lag(Y_obs))) +
  geom_col(aes(y = Y_diff, x = D))+
  geom_hline(yintercept = tibble(Y_obs = Y_obs[,1], D= D) %>% group_by(D) %>% summarize(Y_obs = mean(Y_obs)) %>%
               mutate(Y_diff = Y_obs - lag(Y_obs)) %>% summarize(Ydiff_mean = mean(Y_diff, na.rm=TRUE)) %>%
               pull(Ydiff_mean)) +
  theme_minimal() +
  labs(x = "Treatment D vs D-1",
       y = "",
       title = "Estimated effects",
       note = "Horizontal line is overall average" ) +
  theme(text = element_text(size=36))

ggsave("../lectures/images/linear_multivalued2.pdf")
