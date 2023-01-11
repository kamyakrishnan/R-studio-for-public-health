#Part 4 Week 1 R-studio guide: Life Tables

#load csv
mort_data <- read.csv(file = 'simulated HF mort data.csv')

install.packages("survival")
install.packages("ggplot")

library(survival) # this is the cornerstone command for survival analysis in R
library(ggplot2) # newer package that does nice plots

gender <- as.factor(mort_data[,"gender"]) # R calls categorical variables factors
fu_time <- mort_data[,"fu_time"] # continuous variable (numeric) 
death <- mort_data[,"death"] # binary variable (numeric) 

#KM plot
km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

summary(km_fit, times = c(1:7,30,60,90*(1:10))) 
#The “times” argument gives us control over what 
#time periods we want to see. The above code asks 
#for output every day for the first week, then at 30, 
#60 and 90 days, and then every 90 days thereafter.

#Example 1: Km plot splitting the curve by gender
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_gender_fit)

#test chi squared on groups
survdiff(Surv(fu_time, death) ~ gender, rho=0) 

#Example 2: Km plot splitting by patients 65 and over with those under 65

age_65plus <- ifelse(mort_data[,"age"]>=65,1,0) # dichotomise age
#checks to see if data is split properly
table(age_65plus, exclude = NULL)
table(age,age_65plus, exclude = NULL)

#run survival analysis
survdiff(Surv(fu_time, death) ~ age_65plus, rho=0)

#can conclude that survival times do differ by whether you’ve turned 65 b/c of low p-value