#Part 3 Week 2 R-studio guide: Simple Logistic Regression

#simplest model we can fit is one with no predictors
#simplest logistic regression model
m <- glm(dm ~ 1, family=binomial (link=logit)) #The “1” is just R’s way of saying that there’s only an intercept term in the model.

summary(m)
table(m$y)
#60 1s and 330 0s, which is good because there were 60 yesses and 330 noes in the “dm” vector. It’s important to know that R is modelling the log odds of dm=1 and not the log odds of dm=0!  

#for categorical variables, need to use the as.factor() command 
m <- glm(dm ~ gender, family=binomial (link=logit))
#saying that log odds of having diabetes differs by gender alone
summary(m)

# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) 

# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 


#How to rededine the reference group
#Ex: want to make male the reference variable instead of female
gender <- relevel(gender, ref = "male") 

#check the levels

levels(gender) 
## [1] "male"   "female" 

m <- glm(dm ~  gender, family=binomial (link=logit)) 
summary(m) 
#male will now be the automatic reference group