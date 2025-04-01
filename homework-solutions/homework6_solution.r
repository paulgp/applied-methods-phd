library(tidyverse)


### Problem 1
hrs_data <- read_csv("data/hrs_eventdata.csv")


reg_data <- hrs_data %>%
    mutate(
        treat = wave_hosp <= wave,
        event_time = wave - wave_hosp,
        treat_min4 = (event_time == -4),
        treat_min3 = (event_time == -3),
        treat_min2 = (event_time == -2),
        treat_min1 = (event_time == -1),
        treat_0 = (event_time == 0),
        treat_1 = (event_time == 1),
        treat_2 = (event_time == 2),
        treat_3 = (event_time == 3),
        treat_4 = (event_time == 4)
    )


## Part a
mean(reg_data$wave_hosp == 7)
mean(reg_data$wave_hosp == 8)
mean(reg_data$wave_hosp == 9)
mean(reg_data$wave_hosp == 10)
mean(reg_data$wave_hosp == 11)



# part b
fixest::feols(oop_spend ~ treat | wave + hhidpn, data = reg_data, cluster = ~hhidpn)

# part c
fixest::feols(oop_spend ~ treat | wave + hhidpn, data = reg_data %>% filter(wave < 11 & (wave_hosp == 8 | wave_hosp == 11)), cluster = ~hhidpn)

# part e
fixest::feols(oop_spend ~ treat_min3 + treat_min2 + treat_0 + treat_1 + treat_2 + treat_3 | wave + hhidpn,
    data = reg_data, cluster = ~hhidpn
)

# part f
fixest::feols(oop_spend ~ treat_0 + treat_1 + treat_2 | wave + hhidpn,
    data = reg_data %>% filter(wave < 11 & (wave_hosp == 8 | wave_hosp == 11)), cluster = ~hhidpn
)

fixest::feols(oop_spend ~ treat_min2 + treat_0 + treat_1 | wave + hhidpn,
    data = reg_data %>% filter(wave < 11 & (wave_hosp == 9 | wave_hosp == 11)), cluster = ~hhidpn
)
fixest::feols(oop_spend ~ treat_min3 + treat_min2 + treat_0 | wave + hhidpn,
    data = reg_data %>% filter(wave < 11 & (wave_hosp == 10 | wave_hosp == 11)), cluster = ~hhidpn
)

# part g
make_weights <- function(panel_data, varname) {
    var <- enquo(varname)
    outcome_formula <- as.formula(paste("test", "~ treat_min3 + treat_min2 + treat_0 + treat_1 + treat_2 + treat_3 | wave + hhidpn"))
    panel_data$out0 <- panel_data[[varname]] * (panel_data$wave_hosp == 8)
    if (mean(panel_data$out0) == 0) {
        wave_8_w <- tibble(estimate = 0, wave = 8)
    } else {
        wave_8_w <- fixest::feols(outcome_formula,
            data = panel_data %>% mutate(test = out0),
            cluster = ~hhidpn
        ) %>%
            broom::tidy() %>%
            filter(term == "treat_min2TRUE") %>%
            mutate(wave = 8) %>%
            select(estimate, wave)
    }
    panel_data$out1 <- panel_data[[varname]] * (panel_data$wave_hosp == 9)
    if (mean(panel_data$out1) == 0) {
        wave_9_w <- tibble(estimate = 0, wave = 9)
    } else {
        wave_9_w <- fixest::feols(outcome_formula,
            data = panel_data %>% mutate(test = out1),
            cluster = ~hhidpn
        ) %>%
            broom::tidy() %>%
            filter(term == "treat_min2TRUE") %>%
            mutate(wave = 9) %>%
            select(estimate, wave)
    }

    panel_data$out2 <- panel_data[[varname]] * (panel_data$wave_hosp == 10)
    if (mean(panel_data$out2) == 0) {
        wave_10_w <- tibble(estimate = 0, wave = 10)
    } else {
        wave_10_w <- fixest::feols(outcome_formula,
            data = panel_data %>% mutate(test = out2),
            cluster = ~hhidpn
        ) %>%
            broom::tidy() %>%
            filter(term == "treat_min2TRUE") %>%
            mutate(wave = 10) %>%
            select(estimate, wave)
    }
    panel_data$out3 <- panel_data[[varname]] * (panel_data$wave_hosp == 11)
    if (mean(panel_data$out3) == 0) {
        wave_11_w <- tibble(estimate = 0, wave = 11)
    } else {
        wave_11_w <- fixest::feols(outcome_formula,
            data = panel_data %>% mutate(test = out3),
            cluster = ~hhidpn
        ) %>%
            broom::tidy() %>%
            filter(term == "treat_min2TRUE") %>%
            mutate(wave = 11) %>%
            select(estimate, wave)
    }
    return(bind_rows(wave_8_w, wave_9_w, wave_10_w, wave_11_w))
}

## weights for own effect
make_weights(reg_data, "treat_min2")
# Check sum to 1
make_weights(reg_data, "treat_min2") %>% summarize(sum(estimate))

# check sum to 0
bind_rows(
    make_weights(reg_data, "treat_min3"),
    make_weights(reg_data, "treat_0"),
    make_weights(reg_data, "treat_1"),
    make_weights(reg_data, "treat_2"),
    make_weights(reg_data, "treat_3")
) %>% summarize(sum(estimate))

# Check sum to -1
bind_rows(
    make_weights(reg_data, "treat_min4"),
    make_weights(reg_data, "treat_min1")
) %>% summarize(sum(estimate))

# part h
fixest::feols(oop_spend ~ (treat_min3 + treat_min2 + treat_0 + treat_1 + treat_2):i(wave_hosp, ref = 11) | wave + hhidpn,
    data = reg_data %>% filter(wave < 11), cluster = ~hhidpn
) %>% fixest::etable()

fixest::feols(oop_spend ~ (treat_min3 + treat_min2 + treat_0 + treat_1 + treat_2):i(wave_hosp, ref = 11) | wave + hhidpn,
    data = reg_data %>% filter(wave < 11), cluster = ~hhidpn
) %>%
    broom::tidy() %>%
    filter(stringr::str_detect(term, "treat_0")) %>%
    tidyr::separate(term, into = c("treat", NA, "wave_hosp"), sep = ":") %>%
    mutate(wave_hosp = as.numeric(wave_hosp)) %>%
    right_join(reg_data %>% filter(event_time == 0) %>% group_by(wave_hosp) %>% filter(wave_hosp != 11) %>% tally() %>% mutate(share = n / sum(n))) %>%
    mutate(agg_0 = sum(share * estimate))

# agg_0 is 2960, matches paper

### Problem 2

library(synthdid)
data("california_prop99")

setup <- panel.matrices(california_prop99)
tau.hat.sc <- sc_estimate(setup$Y, setup$N0, setup$T0)
tau.hat.sdid <- synthdid_estimate(setup$Y, setup$N0, setup$T0)

summary(tau.hat.sc)
data <- california_prop99
### Set options for data preparation
id.var <- "State" # ID variable
time.var <- "Year" # Time variable
period.pre <- seq(from = 1970, to = 1988, by = 1) # Pre-treatment period
period.post <- (1989:2000) # Post-treatment period
unit.tr <- "California" # Treated unit (in terms of id.var)
unit.co <- setdiff(unique(california_prop99$State), unit.tr) # Donors pool
outcome.var <- "PacksPerCapita" # Outcome variable
cov.adj <- NULL # Covariates for adjustment
features <- NULL # No features other than outcome
constant <- FALSE # No constant term
report.missing <- FALSE # To check where missing values are
cointegrated.data <- TRUE # Belief that the data are cointegrated

library(scpi)
####################################
### Data preparation
df <- scdata(
    df = data, id.var = id.var,
    time.var = time.var, outcome.var = outcome.var,
    period.pre = period.pre, period.post = period.post,
    unit.tr = unit.tr, unit.co = unit.co, cov.adj = cov.adj, features = features,
    constant = constant, cointegrated.data = cointegrated.data
)

####################################
### SC - point estimation with simplex
est.si <- scest(data = df, w.constr = list(name = "simplex"))
# Use print or summary methods to check results
print(est.si)
summary(est.si)
### SC - plot results
scplot(result = est.si, fig.path = ".")


## Set options for inference
u.alpha <- 0.05 # Confidence level (in-sample uncertainty)
e.alpha <- 0.05 # Confidence level (out-of-sample uncertainty)
rho <- NULL # Regularization parameter (if NULL it is estimated)
rho.max <- 1 # Maximum value attainable by rho
sims <- 200 # Number of simulations
u.order <- 1 # Degree of polynomial in B and C when modelling u
u.lags <- 0 # Lags of B to be used when modelling u
u.sigma <- "HC1" # Estimator for the variance-covariance of u
u.missp <- T # If TRUE then the model is treated as misspecified
e.lags <- 0 # Degree of polynomial in B and C when modelling e
e.order <- 1 # Lags of B to be used when modelling e
e.method <- "gaussian" # Estimation method for out-of-sample uncertainty
cores <- 1 # Number of cores to be used by scpi
w.constr <- list(name = "simplex") # Simplex-type constraint set

set.seed(8894)
pi.si <- scpi(
    data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma,
    u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
    e.method = e.method, cores = cores, w.constr = w.constr, u.alpha = u.alpha,
    e.alpha = e.alpha, rho = rho, rho.max = rho.max
)

scplot(result = pi.si)
# Use print or summary methods to check results
print(pi.si)
summary(pi.si)



### Placebo


data <- california_prop99
### Set options for data preparation
id.var <- "State" # ID variable
time.var <- "Year" # Time variable
period.pre <- seq(from = 1970, to = 1985, by = 1) # Pre-treatment period
period.post <- (1986:2000) # Post-treatment period
unit.tr <- "California" # Treated unit (in terms of id.var)
unit.co <- setdiff(unique(california_prop99$State), unit.tr) # Donors pool
outcome.var <- "PacksPerCapita" # Outcome variable
cov.adj <- NULL # Covariates for adjustment
features <- NULL # No features other than outcome
constant <- FALSE # No constant term
report.missing <- FALSE # To check where missing values are
cointegrated.data <- TRUE # Belief that the data are cointegrated


####################################
### Data preparation
df <- scdata(
    df = data, id.var = id.var,
    time.var = time.var, outcome.var = outcome.var,
    period.pre = period.pre, period.post = period.post,
    unit.tr = unit.tr, unit.co = unit.co, cov.adj = cov.adj, features = features,
    constant = constant, cointegrated.data = cointegrated.data
)

####################################
### SC - point estimation with simplex
est.si <- scest(data = df, w.constr = list(name = "simplex"))
# Use print or summary methods to check results
print(est.si)
summary(est.si)
### SC - plot results
scplot(result = est.si, fig.path = ".")


## Set options for inference
u.alpha <- 0.05 # Confidence level (in-sample uncertainty)
e.alpha <- 0.05 # Confidence level (out-of-sample uncertainty)
rho <- NULL # Regularization parameter (if NULL it is estimated)
rho.max <- 1 # Maximum value attainable by rho
sims <- 200 # Number of simulations
u.order <- 1 # Degree of polynomial in B and C when modelling u
u.lags <- 0 # Lags of B to be used when modelling u
u.sigma <- "HC1" # Estimator for the variance-covariance of u
u.missp <- T # If TRUE then the model is treated as misspecified
e.lags <- 0 # Degree of polynomial in B and C when modelling e
e.order <- 1 # Lags of B to be used when modelling e
e.method <- "gaussian" # Estimation method for out-of-sample uncertainty
cores <- 1 # Number of cores to be used by scpi
w.constr <- list(name = "simplex") # Simplex-type constraint set

set.seed(8894)
pi.si <- scpi(
    data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma,
    u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
    e.method = e.method, cores = cores, w.constr = w.constr, u.alpha = u.alpha,
    e.alpha = e.alpha, rho = rho, rho.max = rho.max
)

scplot(result = pi.si)
# Use print or summary methods to check results
print(pi.si)
summary(pi.si)



library(tidyverse)
library(synthdid)
# library(panelView)
dat <- read_csv("data/wolfers_aer.csv") %>%
    filter(!is.na(div_rate)) %>%
    group_by(st) %>%
    filter(year > 1958 & year < 1991) %>%
    filter(st != "LA" & st != "NM" & st != "IN")

# panelview(div_rate ~ unilateral, data = dat, index = c("st", "year"))

dat %>% write_csv("data/wolfers_aer_clean.csv")

dat %>%
    group_by(lfdivlaw) %>%
    tally()

dat %>%
    group_by(year) %>%
    tally()

dat2 <- dat %>%
    filter(lfdivlaw == 1971 | lfdivlaw == 2000) %>%
    mutate(unilateral = as.logical(unilateral))


results <- list()
results_se <- list()
for (year in seq(1971, 1971 + 15, 2)) {
    if (year <= 1985) {
        dat_year <- dat2 %>% filter(year < 1971 | (year >= !!year & year < !!(year + 2)))
    } else {
        dat_year <- dat2 %>% filter(year < 1971 | year >= 1985)
    }

    setup <- panel.matrices(
        as.data.frame(dat_year %>% select(st, year, div_rate, unilateral))
    )

    tau.hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se <- sqrt(vcov(tau.hat, method = "bootstrap"))
    results[[paste0(year)]] <- tau.hat
    results_se[[paste0(year)]] <- se
}


tau.hat_estimates <- sapply(results, function(x) as.numeric(x))
tau.hat_estimates
se.hat_estimates <- sapply(results_se, function(x) as.numeric(x))
library(ggplot2)

tau.hat_df <- data.frame(
    Year = as.numeric(names(tau.hat_estimates)),
    TauHat = tau.hat_estimates,
    se = se.hat_estimates
)

ggplot(tau.hat_df, aes(x = Year, y = TauHat)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(
        x = "Year",
        y = "Tau Hat Estimates",
        title = "Yearly Tau Hat Estimates"
    ) +
    theme_minimal()

setup <- panel.matrices(
    as.data.frame(dat2 %>% select(st, year, div_rate, unilateral))
)

tau.hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
plot(tau.hat, overlay = 1)

results <- list()
for (year in seq(1973, 1973 + 15, 2)) {
    if (year <= 1988) {
        dat_year <- dat2 %>% filter(year < 1973 | (year >= !!year & year < !!(year + 2)))
    } else {
        dat_year <- dat2 %>% filter(year < 1973 | year >= 1973 + 15)
    }

    setup <- panel.matrices(
        as.data.frame(dat_year %>% select(st, year, div_rate, unilateral))
    )

    tau.hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
    results[[paste0(year)]] <- tau.hat
}

tau.hat_estimates <- sapply(results, function(x) as.numeric(x))
tau.hat_estimates

tau.hat_df <- data.frame(
    Year = as.numeric(names(tau.hat_estimates)),
    TauHat = tau.hat_estimates
)

ggplot(tau.hat_df, aes(x = Year, y = TauHat)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(
        x = "Year",
        y = "Tau Hat Estimates",
        title = "Yearly Tau Hat Estimates"
    ) +
    theme_minimal()
