

dblue = rgb(0, 114, 178, max = 255)
dred = rgb(213,94,0, max = 255)
data = tibble(y = c(c(0.5,0,.75),
                    c(1.5,1,1)),
              t = c(c(-1,0,1),
                    c(-1,0,1)),
              treat = c(rep(1,3), rep(0,3)))


ggplot(data = data) +
  geom_path(aes(y = y, x = t, color = as.factor(treat)), ) +
  scale_color_manual(values = c(dred, dblue)) +
  theme_minimal() +
  theme(text = element_text(size=24),
        legend.position="none")+
  labs(x = "Time", y = "", subtitle="Outcome", color = "Treated") +
  annotate("text", x = 0.8, y = 1.1, label = "Control", size = 6, color = dred) +
  annotate("text", x = 0.8, y = 0.45, label = "Treated", size = 6, color = dblue) 
ggsave("images/rothpic1.png")
  
diff_data = data %>% group_by(t) %>% mutate(y = y*(-1*(treat==0)) + y*(treat == 1)) %>%
  summarize(y = sum(y))
  


ggplot(data = diff_data) +
  geom_path(aes(y = y, x = t)) +
  theme_minimal() +
  theme(text = element_text(size=24),
        legend.position="none")+
  labs(x = "Time", y = "", subtitle="Outcome", color = "Treated") +
  annotate("text", x = 0.55, y = -.6, hjust=0,
           label = "Treated - Control", size = 6, color = "black") 
ggsave("images/rothpic2.png")

data = tibble(y = c(c(0.25,0,.75),
                    c(1.5,1,1)),
              t = c(c(-1,0,1),
                    c(-1,0,1)),
              treat = c(rep(1,3), rep(0,3)))

ggplot(data = data) +
  geom_path(aes(y = y, x = t, color = as.factor(treat)), ) +
  scale_color_manual(values = c(dred, dblue)) +
  theme_minimal() +
  theme(text = element_text(size=24),
        legend.position="none")+
  labs(x = "Time", y = "", subtitle="Outcome", color = "Treated") +
  annotate("text", x = 0.8, y = 1.1, label = "Control", size = 6, color = dred) +
  annotate("text", x = 0.8, y = 0.45, label = "Treated", size = 6, color = dblue) 
ggsave("images/rothpic3.png")

diff_data = data %>% group_by(t) %>% mutate(y = y*(-1*(treat==0)) + y*(treat == 1)) %>%
  summarize(y = sum(y))

ggplot(data = diff_data) +
  geom_path(aes(y = y, x = t)) +
  geom_hline(yintercept = -1, color = dblue) +
  theme_minimal() +
  theme(text = element_text(size=24),
        legend.position="none")+
  labs(x = "Time", y = "", subtitle="Outcome", color = "Treated") +
  annotate("text", x = 0.55, y = -.6, hjust=0,
           label = "Treated - Control", size = 6, color = "black") +
  annotate("text", x = -1, y = -1.05, hjust=0,
           label = "Is this significantly different?", size = 6, color = dred)
ggsave("images/rothpic4.png")
