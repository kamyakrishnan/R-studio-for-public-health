#Part 3 Week 4 R-studio guide: Model fitting using predictive power and goodness of fit techniques

##Step 1: design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 

# check your model 
summary(full_model) 

##Step 2: run a null model 
null_model <- glm(dm ~ 1, family=binomial (link=logit)) 

# check 
summary(null_model) 

##Step 3: test predictive power of model

#McFadden's R-squared
R2 <- 1-logLik(full_model)/logLik(null_model) 
# print it 
R2 

#c-statistic
# install a package 
install.packages("DescTools") 
# load package 
require(DescTools) 
# generate the c-statistic 
Cstat(full_model)

##Step 3: test goodness of fit of model

#Hosmer-Lemeshow statistic and test:
# install package "ResourceSelection" 
install.packages("ResourceSelection") 
# load package 
require(ResourceSelection) 
full_model$y
#full_model$y  is the outcome variable we specified (dm); fitted(full_model) generates fitted values from the model. 
# run Hosmer-Lemeshow test 
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL 

# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 

# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"]) 

# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"])) 


#Hosmer and Lemeshow goodness of fit (GOF) test (another package that can be used) 
# install package("generalhoslem") 
install.packages("generalhoslem") 
# load package 
require(generalhoslem) 
# run Hosmer-Lemeshow test 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10) 


#Looking @ deviance of models

# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit)) 

# analyse table of deviance 
anova(full_model, test = "Chisq") 

#Using backwards selection to determine if systolic and diastolic BP are significantly correlated as found in literature

#Step 1: create vectors for all predictors being tested

dm <- as.factor(DIABETES[,"dm"]) 
insurance <- as.factor(DIABETES[,"insurance"])# let's say 0=none, 1=gov, 2=private 
fh <- as.factor(DIABETES[,"fh"]) # 1=FH, 0=no FH 
smoking <- as.factor(DIABETES[,"smoking"]) # 1,2,3 
chol <- DIABETES[,'chol'] 
hdl <- DIABETES[,'hdl'] 
ratio <- DIABETES[,'ratio'] 
location <- as.factor(DIABETES[,'location']) 
age <- DIABETES[,'age'] 
gender <- as.factor(DIABETES[,'gender']) 
frame <- as.factor(DIABETES[,'frame']) 
systolic <- DIABETES[,'bp.1s'] 
diastolic <- DIABETES[,'bp.1d'] 

#bmi calculation
height <- DIABETES[,'height']
weight <- DIABETES[,'weight']
height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2

#Step 2: create the model
model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit)) 

summary(model)

#Step 3: test deviance
anova(model, test = "Chisq") 

#Step 4: drop variables with poor correlation
#In this scenario, both BP variables had boor correlation and will be dropped from the model

model <- glm(dm ~ age + bmi + chol + hdl, family = binomial(link = logit)) 

summary(model)

#Step 5: test the correlation of the dropped predictor with other predictors

#Ex: hdl
cor.test(systolic, hdl)

