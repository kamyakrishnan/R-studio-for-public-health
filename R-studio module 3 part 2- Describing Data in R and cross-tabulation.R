#Part 3 Week 2 R-studio guide: Describing Data in R and cross-tabulation

#Step 1: read the csv file
DIABETES <- read.csv(file = 'Diabetesfilepart3week2Rstudio.csv', header = TRUE, sep = ',')

#Step 2: look at the column names
dimnames(DIABETES)[[2]]

#Step 3: look at what the data in each column is encoding

#Examples
#insurance: 0=none, 1=government, 2=private
#fh = family history of diabetes (yes/no, where 1=yes, 0=no)
#smoking: 1=current, 2=never and 3=ex

#Step 4: assign each variable as continuous or categorical
#r assumes all variables are continuous by default

chol <- DIABETES['chol'] # cholesterol is continuous, so it’s easy
gender <- as.factor(DIABETES[,'gender']) # but gender isn’t.
dm <- as.factor(DIABETES[,'dm']) # neither is dm

#Step 5: Do a variable analysis

###In this case, we will first analyze gender(categorical)
t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results

#to get proportions and round
round(prop.table(t),digits=3) # get proportions rounded to 3dp
round(100*prop.table(t),digits=1) # get %s rounded to 1dp

###Now we will analyze dm(categorical)
#we want to include all missing values, so we make a new factor where we don't exlude all dm null values
dm2 <- factor(dm, exclude=NULL) # make new factor from the old one
table(dm2) # display the counts including the missings (NAs)

###Now we will analyze cholesterol(continuous)
#we will analyze continuous variables using the summary tool
summary(chol)

#how to make categorical variables from a continuous one
#Below is an example of categorizing bmi based off calculations from known height an d weight
> height <- DIABETES[,'height']
> weight <- DIABETES[,'weight']
> height.si <- height*0.0254
> weight.si <- weight*0.453592
> bmi <- weight.si/height.si^2

bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 
# frequencies of diabetes by BMI category
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL) 

# check 
dm_by_bmi_category

# with the row percentages 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 


#Next steps:
#You could make this a hypothesis test to see whether obesity is 
#statistically associated with diabetes – in which case, you’d use 
#a chi-squared test to do this. But, the aim here is merely to describe 
#the data in a way that will help you with the modelling later on. Just because 
#you can do a statistical test, doesn’t mean that you should. You’ll go through 
#the chi-squared test later on.

#PRACTICING ON AGE + GENDER VARIABLE

age <- DIABETES[,"age"] 
age_categorised <- ifelse(age < 45, "<45", 
                          ifelse(age >= 45 & age < 65, "45<age<64", 
                                 ifelse(age >= 65 & age < 75, "65<age<74", 
                                        ifelse(age >= 75, ">75", NA))))
table(age_categorised, exclude = NULL) 

#combined variable crosstable
age_group_by_gender <- table(age_categorised, gender, exclude = NULL)
round(100 * prop.table(age_group_by_gender, margin = 2), digits = 1)


# optional extra check for the extra cautious! 
head(cbind(age_grouped, age))
