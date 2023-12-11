library(RDHonest)
library(tidyverse)
library(haven)

ggplot(data = lee08) + geom_histogram(aes(y=..density..,x = margin),alpha = 0.4) + theme_minimal()  + xlim(0,99)
ggsave("images/lee_hist.png")
ggplot(data = lee08) + geom_histogram(aes(y=..density..,x = margin),alpha = 0.4) + geom_density(aes(y=..density..,x = margin)) + theme_minimal()  + xlim(0,99)
ggsave("images/lee_density.png")
ggplot(data = lee08) +  geom_density(aes(y=..density.. ,x = margin), bw = 1)  + theme_minimal() + xlim(0,99)
ggsave("images/lee_density_bw1.png")
ggplot(data = lee08) +  geom_density(aes(y=..density.. ,x = margin), bw = 0.1)  + theme_minimal() + xlim(0,99)
ggsave("images/lee_density_bw2.png")

lee_data = read_dta("code/data/group_final.dta")

ggplot(data = lee08) + geom_point(aes(y = voteshare , x = margin)) + theme_minimal() + xlim(-25,25)  +
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t") + geom_vline(xintercept = 0) + 
    theme(text = element_text(size=24))
ggsave("images/lee_rd_scatter.png")

lee08_grouped = lee08 %>% mutate(margin_round = floor(margin/.5)*.5) %>% group_by(margin_round) %>%
  summarize(voteshare_mean = mean(voteshare))
ggplot(data = lee08_grouped) + geom_point(aes(y = voteshare_mean , x = margin_round)) + 
  theme_minimal() + 
  theme(text = element_text(size=24)) +
  xlim(-25,25) +  
  ylim(0,100)+
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t") + geom_vline(xintercept = 0) + 
  theme(text = element_text(size=24)) 
ggsave("images/lee_rd_binscatter1.png")
ggplot(data = lee08_grouped) + geom_point(aes(y = voteshare_mean , x = margin_round)) + 
  theme_minimal() + 
  xlim(-25,25) +  
  theme(text = element_text(size=24)) +
  ylim(30,70) +
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t") + geom_vline(xintercept = 0)
ggsave("images/lee_rd_binscatter1b.png")


lee08_grouped = lee08 %>% mutate(margin_round = floor(margin/4)*4) %>% group_by(margin_round) %>%
  summarize(voteshare_mean = mean(voteshare))
ggplot(data = lee08_grouped) + geom_point(aes(y = voteshare_mean , x = margin_round)) + 
  theme_minimal() + 
  xlim(-25,25) +  
  theme(text = element_text(size=24)) +
  ylim(30,70) +
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t") + geom_vline(xintercept = 0)
ggsave("images/lee_rd_binscatter1d.png")

lee08_grouped = lee08 %>% mutate(margin_round = floor(margin/.1)*.1) %>% group_by(margin_round) %>%
  summarize(voteshare_mean = mean(voteshare))
ggplot(data = lee08_grouped) + geom_point(aes(y = voteshare_mean , x = margin_round)) + 
  theme_minimal() + 
  theme(text = element_text(size=24)) +
  xlim(-25,25) +  
  ylim(30,70) +
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t") + geom_vline(xintercept = 0)
ggsave("images/lee_rd_binscatter1e.png")

library(rdrobust)
lee08 <- lee08 %>% filter(abs(margin) <= 25)
out = rdplot(lee08$voteshare, lee08$margin, nbins = c(20, 20), binselect = "es", x.lim = c(-25, 25), y.lim = c(30, 70))
out$rdplot$layers[[2]] <- NULL
out$rdplot$layers[[2]] <- NULL
out$rdplot +
  theme(text = element_text(size=24)) + theme_minimal() + 
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t", title = "")
ggsave("images/lee_rd_binscatter_es.png")
out = rdplot(lee08$voteshare, lee08$margin, nbins = c(20, 20), binselect = "qs", x.lim = c(-25, 25), y.lim = c(30, 70))
out$rdplot$layers[[2]] <- NULL
out$rdplot$layers[[2]] <- NULL
out$rdplot +
  theme(text = element_text(size=24)) + theme_minimal() + 
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t", title = "")
ggsave("images/lee_rd_binscatter_qs.png")

out = rdplot(lee08$voteshare, lee08$margin, binselect = "qs", x.lim = c(-25, 25), y.lim = c(30, 70))
out$rdplot$layers[[2]] <- NULL
out$rdplot$layers[[2]] <- NULL
out$rdplot +
  theme(text = element_text(size=24)) + theme_minimal() + 
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t", title = "")
ggsave("images/lee_rd_binscatter_qs_choice.png")
out = rdplot(lee08$voteshare, lee08$margin, binselect = "qsmv", x.lim = c(-25, 25), y.lim = c(30, 70))
out$rdplot$layers[[2]] <- NULL
out$rdplot$layers[[2]] <- NULL
out$rdplot +
  theme(text = element_text(size=24)) + theme_minimal() + 
  labs(y = "Vote Share in t+1", x = "Margin of Victory in t", title = "")
ggsave("images/lee_rd_binscatter_qsmv_choice.png")

est = rdrobust(lee08$voteshare, lee08$margin)
est_rdhonest = RDHonest(lee08$voteshare ~ lee08$margin, cutoff = 0, M = 0.1, opt.criterion="MSE")
est_rdhonest2 = RDHonest(lee08$voteshare ~ lee08$margin, cutoff = 0, M = 1, opt.criterion="MSE")
est_rdhonest3 = RDHonest(lee08$voteshare ~ lee08$margin, cutoff = 0, M = 0.01, opt.criterion="MSE")
data = tibble( mu = c(est$coef[1], est_rdhonest3$estimate, est_rdhonest$estimate, est_rdhonest2$estimate), lb = c(est$ci[2], est_rdhonest3$lower, est_rdhonest$lower, est_rdhonest2$lower), ub = c(est$ci[5], est_rdhonest3$upper, est_rdhonest$upper, est_rdhonest2$upper),  case = c("RDRobust", "RDHonest M=0.01", "RDHonest M=.1", "RDHonest M=1"), bw = c(est$bws[1], est_rdhonest3$hp, est_rdhonest$hp, est_rdhonest2$hp))
ggplot(data = data, aes(y = est, ymin = lb, ymax = ub, x = case)) + geom_pointrange()
ggplot
summary(est)
est_rdhonest
coef = est$coef[1]
label1 = paste0("Coef = ", round(coef, digits = 2))
label2 = paste0("SE = ", round(est$se[1], digits = 2))
label3 = paste(c(label1,label2), collapse = "\n")
out$rdplot + annotate("text", x = 10, y = 50, label = label3, size = 5) + theme_minimal() +
  labs(title = "Effect of Incumbency on Election in t+1",
       y = "Vote share in period t+1",
       x = "Win margin in period t") +
  theme(text = element_text(size=24))
ggsave("images/lee_rd_binscatter_output.png")


bw_vec = c()
coef_vec = c()
se_vec = c()
for (i in seq(2,10)) {
  bw_vec = c(bw_vec,i)
  coef_vec = c(coef_vec, rdrobust(lee08$voteshare, lee08$margin, h = i)$coef[1] )
  se_vec = c(se_vec, rdrobust(lee08$voteshare, lee08$margin, h = i)$se[1] )
}
ggplot(data = tibble(bw = bw_vec, coef = coef_vec, se = se_vec)) + 
  geom_pointrange(aes(y = coef, ymin = se_vec*1.96 + coef, ymax = coef - se_vec*1.96, x = bw )) +
  theme_minimal() +
  labs(x = "Bandwidth", y = "Estimated Effect") +
  theme(text = element_text(size=24))
ggsave("images/lee_rd_binscatter_output_robust.png")

oreo = data.frame(logearn = log(cghs$earnings), year14 = cghs$yearat14)
ggplot(data = oreo) + geom_point(aes(y = logearn, x = year14))

rdrobust(oreo$logearn, oreo$year14, c = 1947)
RDHonest(logearn ~ year14, data = oreo, cutoff = 1947, M = 0.01, 
         opt.criterion = "MSE")
