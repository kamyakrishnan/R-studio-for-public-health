#Part 4 Week 2 R-studio guide: Simple cox model
install.packages("survival")

library(survival)
library(survminer)

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = mort_data) # take variables straight from g
summary(cox)