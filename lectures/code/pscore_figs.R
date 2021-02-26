
library(tidyverse)

p1 = pnorm(rnorm(10000, mean = 0.6, sd = 0.3) )
p0 = pnorm(rnorm(10000, mean = 0.2, sd = 0.3))
plot_data = tibble(control = p0, treatment = p1) %>%
  gather(key = "group", value = "val")

plot_data2 = tibble(control = p0, treatment = p1) 
ggplot(data = plot_data, aes(x = val, color = group)) +
  geom_density()  + theme_classic()+
  labs(x = "Propensity Score", y="",
       title = "Overlap of propensities",
       color = "Treatment Group") +
  theme(legend.position =  c(.9, .9))
ggsave("../lectures/images/overlap1.pdf")
ggplot(data = plot_data, aes(x = val, color = group)) +
  geom_density(size=2)  + theme_classic()+
  labs(x = "Propensity Score", y="",
       title = "Overlap of propensities",
       color = "Treatment Group") +
  theme(legend.position =  c(.9, .9),
        text = element_text(size=24))
ggsave("../lectures/images/overlap1.pdf")
ggplot(data = plot_data %>%
         filter(group == "control" | (group == "treatment" & val > 0.7)), aes(x = val, color = group)) +
  geom_density()  + theme_classic()+
  labs(x = "Propensity Score", y="",
       title = "Overlap of propensities",
       color = "Treatment Group") +
  theme(legend.position =  c(.9, .9),
        text = element_text(size=24))
ggsave("../lectures/images/overlap2.pdf")


ggplot(data = plot_data %>%
         filter(group == "control" ), aes(x = val)) +
  geom_density(size=2)  + theme_classic()+
  labs(x = "Propensity Score", y="",
       title = "Who benefits from the treatment?",
       subtitle = "Pr(D = 1 | X)",
       color = "Treatment Group") +
  annotate("text", x = 0.25, y = 1, label = "Low value of treatment", 
           size = 6, color = "red")+
  annotate("text", x = 0.8, y = 2, label = "High value \n of treatment", 
           size = 6, color = "blue")+
  theme(legend.position =  c(.9, .9),
        text = element_text(size=24))
ggsave("../lectures/images/overlap3.pdf")
