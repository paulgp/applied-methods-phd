

library(tidyverse)
library(haven)

bkdata <- read_dta("~/Dropbox/Papers/GPH_ExaminerDesign/Applications/DGPY/Data/paulData_prepped.dta")

overall_leniency = bkdata %>% group_by(judge, office) %>% summarize(discharge = mean(discharge),
                                                                    discharge_res = mean(z_ij_pooled),
                                                                    findex_0_4_std = mean(findex_0_4_std, na.rm=TRUE),
                                                                    findex_0_4_std_res = mean(findex_0_4_std_res, na.rm=TRUE),
)

ggplot(data = overall_leniency, aes(x = discharge)) + 
  geom_histogram(binwidth=0.025) + 
  theme_minimal() + labs(x = "Avg. Discharge Rate", y="")
ggsave("images/leniency_overall.png")
ggplot(data = overall_leniency, aes(x = discharge_res)) + 
  geom_histogram(binwidth=0.01) + 
  theme_minimal() + labs(x = "Avg. Discharge Rate (residualized)", y="")
ggsave("images/leniency_res.png")
ggplot(data = overall_leniency %>% ungroup(), aes(x = findex_0_4_std)) +
  geom_histogram() +
  theme_minimal() + labs(x = "Avg. Index Measure", y="")
ggsave("images/outcome.png")

ggplot(data = overall_leniency %>% ungroup(), aes(x = findex_0_4_std_res)) +
  geom_histogram() +
  theme_minimal() + labs(x = "Avg. Index Measure", y="")
ggsave("images/outcome_res.png")
