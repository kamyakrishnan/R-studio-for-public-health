#Part 3 Week 3 R-studio guide: Multiple Logistic Regression

##Step 1: shape distribution of the variables of interest by plotting the variables 

#Ex: cholesterol

#make a column vector
chol <- DIABETES['chol']

#list summary
summary(chol)

#had to exlude null values to run density finction
chol.no.na <- chol[is.na(chol)==0]

#density
d <- density(chol.no.na)

#plot
plot(d,main = "") 


##Step 2: Assessing crude relations between the predictors and the outcomes

###GENDER###

# define the gender variable 
gender <- as.factor(DIABETES[,"gender"]) 

# cross tabulation 
dm_by_gender <- table(gender, dm) # not including NA values because there aren't that many 

# proportion of diabetes status by gender 
dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1) 

# calculate the odds of having diabetes by gender 
odds_gender <- dm_by_gender_prop[, "yes"]/dm_by_gender_prop[, "no"] 

# calculate the log odds 
logodds_gender <- log(odds_gender) 

# plot the log odds of having diabetes by gender 
dotchart(logodds_gender)

###AGE###

##Plotting age as a continuous variable##
# define the age variable (continuous) 
age <- age <- DIABETES[,"age"] 

# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by age 
dm_by_age_prop <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds_age <- dm_by_age_prop[, "yes"]/dm_by_age_prop[, "no"] 

# calculate the log odds 
logodds_age <- log(odds_age) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_age_prop), logodds_age)

##Plotting age as a categorical variable##
# age grouping converting continuous variable to a categorical (ordinal) one  
age_grouped <- ifelse(age < 45, "under 45", 
                      ifelse(age >= 45 & age < 65, "45 - 64",  
                             ifelse(age >= 65 & age < 75, "65 - 74",  
                                    ifelse(age >= 75, "75 or over", NA)))) 

age_grouped <- factor(age_grouped, levels = c("under 45", "45 - 64", "65 - 74", "75 or over")) 

# create a cross tabulation of age and diabetes status  
dm_by_age_grouped <- table(age_grouped, dm) 

# output the frequencies of diabetes status by age 
age_grouped_prop <- prop.table(dm_by_age_grouped, margin = 1) 

# calculate the odds of having diabetes 
odds_age_grouped <- age_grouped_prop[, "yes"]/age_grouped_prop[, "no"] 

# calculate the log odds 
logodds_age_grouped <- log(odds_age_grouped) 

# plot the age groups found in the sample against the log odds of having diabetes 
dotchart(logodds_age_grouped) 

###CHOLESTEROL###

##plotting cholesterol as a continuous variable##
# define chol as a continuous variable 
chol <- DIABETES[,"chol"] 

# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol <- table(chol, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by cholesterol 
dm_by_chol_prop <- prop.table(dm_by_chol, margin = 1) 

# calculate the odds of having diabetes 
odds_chol <- dm_by_chol_prop[, "yes"]/dm_by_chol_prop[, "no"] 

# calculate the log odds 
logodds_chol <- log(odds_chol) 

# plot the cholesterol found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_chol_prop), logodds_chol, xlim=c(150, 300)) 

##Plotting cholesterol as an ordinal variable##
# categorising chol into an ordinal variable 

# https://www.medicalnewstoday.com/articles/315900.php 
chol_categorised <- ifelse(chol < 200, "healthy",  
                           ifelse(chol < 240, "borderline high", 
                                  ifelse(chol >= 240, "high", NA))) 

# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
chol_categorised <- factor(chol_categorised, levels = c("healthy", "borderline high", "high")) 

# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol_categorised <- table(chol_categorised, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by cholesterol 
dm_by_chol_categorised_prop <- prop.table(dm_by_chol_categorised, margin = 1) 

# calculate the odds of having diabetes 
odds_chol_categorised <- dm_by_chol_categorised_prop[, "yes"]/dm_by_chol_categorised_prop[, "no"] 

# calculate the log odds 
logodds_chol_categorised <- log(odds_chol_categorised) 

# plot the cholesterol categories found in the sample against the log odds of having diabetes 
dotchart(logodds_chol_categorised)

###HDL###
#same code and process as cholesterol

### Multiple Regression Model ###

m <- glm(dm ~ age + gender + bmi, family=binomial (link=logit)) 
summary(m) 

