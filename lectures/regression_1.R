

library(tidyverse)

library(readxl)
data = read_excel("imbenskolesardata.xlsx")


ggplot(data = data %>% gather(case, value, -estimator)) +
  geom_col(aes(y = value, x = case, fill = estimator), position =  "dodge2") +
  theme_minimal() +
  geom_hline(yintercept=95) +
  scale_x_discrete(labels = c("0.5", "0.85", "1", "1.18", "2")) +
  labs(y = "Coverage", x = "Variance of Sigma^2(0)",fill = "Estimator") +
  theme(text= element_text(size=30))

ggsave("imbenskolesar_coverage.pdf")

