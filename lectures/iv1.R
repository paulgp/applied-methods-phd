
set.seed(123)
library(tidyverse)
library(lfe)
dblue = rgb(0,114,178,maxColorValue = 255)
dred = rgb(213,94,0,maxColorValue = 255)
dgreen = rgb(46,147,60,maxColorValue = 255)
max_var_d = 0.25
# want var(d) = 0.5. var(z) = 0.25. if pi = .5, then 
pi <- .5
beta <- 2
z <- rbinom(1000, 1, .5)
u <- rnorm(1000, 0, sqrt(max_var_d - var(z)*pi^2))
rho = 0
d = z*pi[1] + u 
eps =  rnorm(1000, sd = 0.5)
y = d*beta[1]  + eps + rho*u

reg_data = data.frame(y, d,z)
summary(felm(y ~ 1 | 0 | (d ~ z) , data = reg_data))
 

d_hat_0  = mean(reg_data$d[reg_data$z==0])
d_hat_1  = mean(reg_data$d[reg_data$z==1])

y_hat_0  = mean(reg_data$y[reg_data$z==0])
y_hat_1  = mean(reg_data$y[reg_data$z==1])

g =ggplot() +
  geom_point(data = tibble(y = c(y_hat_0, y_hat_1), 
                           d = c(d_hat_0, d_hat_1),
                           case = c("Z=0", "Z=1")),
             aes(y = y, x = d, label = case), 
             color = dblue)+
  geom_text(data = tibble(y = c(y_hat_0, y_hat_1), 
                           d = c(d_hat_0, d_hat_1) -.005,
                           case = c("Z=0", "Z=1")),
             aes(y = y, x = d, label = case), hjust=1, size = 6,
            color = dblue) +
  theme_minimal() +
 # xlim(d_hat_0-.3, d_hat_1 + .2) + ylim( 2*(d_hat_0-.3), 2*(d_hat_1 + .2 )) +
  geom_segment(data = tibble(y0 = y_hat_0, y1 = y_hat_1, 
                             d0 = d_hat_0, d1 = d_hat_1),
               aes(y = y0, yend = y1, x = d0, xend = d1),
               color = dblue) +
  annotate("text", x = .01+ (d_hat_0+d_hat_1)/2 , 
           y = (y_hat_0+y_hat_1)/2, hjust=0,
           label="Slope = 2SLS estimate", size = 6, color = dblue) +
  labs(y = "", 
       x = "D",
       title = "Y") +
  theme(text = element_text(size = 36),
        plot.title.position = "plot")
g +
geom_point(data = tibble(y = y, 
                         d = d),
           aes(y = y, x = d), 
           color = dred, alpha = 0.2)
ggsave("images/weakiv_example1.png")
g2=g + 
  geom_point(data = tibble(y = y, 
                           d = d),
             aes(y = y, x = d), 
             color = dred)
xrange_fix = ggplot_build(g2)$layout$panel_scales_x[[1]]$range$range
yrange_fix = ggplot_build(g2)$layout$panel_scales_y[[1]]$range$range
g + 
  annotate("text", x = Inf, 
           y = (d_hat_0+d_hat_1), hjust=1,
           label="Slope = Truth", size = 8) +
  geom_abline(intercept = 0, slope = 2) +
  xlim(xrange_fix) + ylim(yrange_fix)
ggsave("images/weakiv_example2.png")

pi <- .1
beta <- 2
u <- rnorm(1000, 0, sqrt(max_var_d - .25*pi^2))

d = z*pi[1] + u
y = d*beta[1]  + eps

reg_data = data.frame(y, d,z)
summary(felm(y ~ 1 | 0 | (d ~ z) , data = reg_data))


d_hat_0  = mean(reg_data$d[reg_data$z==0])
d_hat_1  = mean(reg_data$d[reg_data$z==1])

y_hat_0  = mean(reg_data$y[reg_data$z==0])
y_hat_1  = mean(reg_data$y[reg_data$z==1])

g =ggplot() +
  geom_point(data = tibble(y = c(y_hat_0, y_hat_1), 
                           d = c(d_hat_0, d_hat_1),
                           case = c("Z=0", "Z=1")),
             aes(y = y, x = d, label = case), 
             color = dblue)+
  geom_text(data = tibble(y = c(y_hat_0, y_hat_1), 
                          d = c(d_hat_0, d_hat_1) -.005,
                          case = c("Z=0", "Z=1")),
            aes(y = y, x = d, label = case), hjust=1, size = 6,
            color = dblue) +
  theme_minimal() +
  xlim(xrange_fix ) + ylim(yrange_fix) +
  geom_segment(data = tibble(y0 = y_hat_0, y1 = y_hat_1, 
                             d0 = d_hat_0, d1 = d_hat_1),
               aes(y = y0, yend = y1, x = d0, xend = d1),
               color = dblue) +
  labs(y = "", 
       x = "D",
       title = "Y") +
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
g+ 
  geom_point(data = tibble(y = y, 
                           d = d),
             aes(y = y, x = d), 
             color = dred, alpha = 0.2)
ggsave("images/weakiv_example3.png")



pi <- .1
beta <- 2
u <- rnorm(1000, 0, sqrt(max_var_d - .25*pi^2))

d = z*pi[1] + u
y = d*beta[1]  + eps

reg_data = data.frame(y, d,z)
summary(felm(y ~ 1 | 0 | (d ~ z) , data = reg_data))


d_hat_0  = mean(reg_data$d[reg_data$z==0])
d_hat_1  = mean(reg_data$d[reg_data$z==1])

y_hat_0  = mean(reg_data$y[reg_data$z==0])
y_hat_1  = mean(reg_data$y[reg_data$z==1])

g =ggplot() +
  geom_point(data = tibble(y = c(y_hat_0, y_hat_1), 
                           d = c(d_hat_0, d_hat_1),
                           case = c("Z=0", "Z=1")),
             aes(y = y, x = d, label = case), 
             color = dblue)+
  geom_text(data = tibble(y = c(y_hat_0, y_hat_1), 
                          d = c(d_hat_0, d_hat_1) -.005,
                          case = c("Z=0", "Z=1")),
            aes(y = y, x = d, label = case), hjust=1, size = 6,
            color = dblue) +
  theme_minimal() +
  xlim(xrange_fix ) + ylim(yrange_fix) +
  geom_segment(data = tibble(y0 = y_hat_0, y1 = y_hat_1, 
                             d0 = d_hat_0, d1 = d_hat_1),
               aes(y = y0, yend = y1, x = d0, xend = d1),
               color = dblue) +
  labs(y = "", 
       x = "D",
       title = "Y") +
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
g+ 
  geom_point(data = tibble(y = y, 
                           d = d),
             aes(y = y, x = d), 
             color = dred, alpha = 0.2)
ggsave("images/weakiv_example3.png")


pi <- .001
beta <- 2
u <- rnorm(1000, 0, sqrt(max_var_d - .25*pi^2))

d = z*pi[1] + u
y = d*beta[1]  + eps

reg_data = data.frame(y, d,z)
summary(felm(y ~ 1 | 0 | (d ~ z) , data = reg_data))


d_hat_0  = mean(reg_data$d[reg_data$z==0])
d_hat_1  = mean(reg_data$d[reg_data$z==1])

y_hat_0  = mean(reg_data$y[reg_data$z==0])
y_hat_1  = mean(reg_data$y[reg_data$z==1])

g =ggplot() +
  geom_point(data = tibble(y = c(y_hat_0, y_hat_1), 
                           d = c(d_hat_0, d_hat_1),
                           case = c("Z=0", "Z=1")),
             aes(y = y, x = d, label = case), 
             color = dblue)+
  geom_text(data = tibble(y = c(y_hat_0, y_hat_1), 
                          d = c(d_hat_0, d_hat_1) -.005,
                          case = c("Z=0", "Z=1")),
            aes(y = y, x = d, label = case), hjust=1, size = 6,
            color = dblue) +
  theme_minimal() +
  xlim(xrange_fix ) + ylim(yrange_fix) +
  geom_segment(data = tibble(y0 = y_hat_0, y1 = y_hat_1, 
                             d0 = d_hat_0, d1 = d_hat_1),
               aes(y = y0, yend = y1, x = d0, xend = d1),
               color = dblue) +
  labs(y = "", 
       x = "D",
       title = "Y") +
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
g+ 
  geom_point(data = tibble(y = y, 
                           d = d),
             aes(y = y, x = d), 
             color = dred, alpha = 0.2)
ggsave("images/weakiv_example5.png")


ggplot() +
  geom_point(data = tibble(y = c(y_hat_0, y_hat_1), 
                           d = c(d_hat_0, d_hat_1),
                           case = c("Z=0", "Z=1")),
             aes(y = y, x = d, label = case), 
             color = dblue)+
  geom_text(data = tibble(y = c(y_hat_0, y_hat_1), 
                          d = c(d_hat_0, d_hat_1) -.0005,
                          case = c("Z=0", "Z=1")),
            aes(y = y, x = d, label = case), hjust=1, size = 6,
            color = dblue) +
  theme_minimal() +
  geom_segment(data = tibble(y0 = y_hat_0, y1 = y_hat_1, 
                             d0 = d_hat_0, d1 = d_hat_1),
               aes(y = y0, yend = y1, x = d0, xend = d1),
               color = dblue) +
  labs(y = "", 
       x = "D",
       title = "Y") +
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
ggsave("images/weakiv_example6.png")



beta = 2

delta = .2
tau = .1
tau_hat = rnorm(1000, tau, sd = .05)
beta_hat = delta / tau_hat


ggplot(data = tibble(beta_hat, tau_hat)) + 
  geom_histogram(aes(x = tau_hat), bins = 100) +
  theme_minimal() +
  labs(x= "Pi Hat", y = "") +
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
ggsave("images/weak_iv_tauhat_hist.png")

ggplot(data = tibble(beta_hat, tau_hat)) + 
  geom_point(aes(y = beta_hat, x = tau_hat)) +
  theme_minimal() +
  labs(x = "Tau Hat", y = "Beta hat")+
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
ggsave("images/weak_iv_tauhat_betahat.png")


ggplot(data = tibble(beta_hat, tau_hat)) + 
  geom_histogram(aes(x = beta_hat), bins = 100) +
  theme_minimal() +
  labs(x= "Beta Hat", y = "")+
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
ggsave("images/weak_iv_betahat_hist.png")




beta = 2

delta = .2
tau = .1
tau_hat = rnorm(1000, tau, sd = .01)
beta_hat = delta / tau_hat


ggplot(data = tibble(beta_hat, tau_hat)) + 
  geom_point(aes(y = beta_hat, x = tau_hat)) +
  theme_minimal() +
  labs(x = "Pi Hat", y = "Beta hat")+
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
ggsave("images/weak_iv_tauhat_betahat_strong.png")


ggplot(data = tibble(beta_hat, tau_hat)) + 
  geom_histogram(aes(x = beta_hat), bins = 100) +
  theme_minimal() +
  labs(x= "Beta Hat", y = "")+
  theme(text = element_text(size = 24),
        plot.title.position = "plot")
ggsave("images/weak_iv_betahat_hist_strong.png")
