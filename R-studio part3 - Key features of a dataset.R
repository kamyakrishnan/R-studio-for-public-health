#Week 3 R-studio guide: Assessing key data features of data set

# # of rows and columns
dim(COPD) 
## [1] 101  24

#looks at beginning of data frame
head(COPD) 

#type of value in column
class(COPD$CAT) 

## [1] "integer" 

#summary
summary(COPD$CAT) 

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
##    3.00   12.00   18.00   19.34   24.00  188.00 

#histogram
hist(COPD$CAT)

#convert an integer into a categorical variable that can be analyzed

class(COPD$gender) 

## [1] "integer" 

COPD$gender <- as.factor(COPD$gender) 
class(COPD$gender) 

## [1] "factor"

#-----------------------------------------#

#1 Inspect the dataset for missing values and outliers

#download Hmisc package to use describe() function
describe(COPD)

#categorical variables, you can tabulate the data  
#download the ‘gmodels’ package, then use the CrossTable() function
#the sum(is.na()) functions to check for missing values.

#2 Examine the relationship between your candidate predictor variables 

#Ex: Pearson’s pairwise correlation coefficient for the variables AGE, PackHistory, FEV1, FEV1PRED, FVC, CAT, HAD, and SGRQ
#Step 1) create correlation matrix

my_data <- COPD[,c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")] #create a new vector
cor_matrix <- cor(my_data) #create a correlation matrix with the vector
round(cor_matrix, 2) #round the matrix to 2 decimal places

#visually assess correlation
pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ, data = COPD) 

#use cross tabulations to examine associations between categorical variables
CrossTable(COPD$hypertension, COPD$IHD)
#8 patients had IHD only, and 11 patients had hypertension only, one person had 
#both hypertension and IHD and 81 had neither

# Step 3) Fit a simple linear regression model 

model_name <- lm(outcome ~ predictor, data =dataframe) 

summary(model_name) 

confint(model_name)  

#-----------------------------------------#

#Running and interpreting a multiple regression
#variables need to be changed to integers
COPD$Diabetes <- c(0,1)[as.integer(COPD$Diabetes)]
COPD$AtrialFib <- c(0,1)[as.integer(COPD$AtrialFib)]

#create the new variable using the R code: 
DAF <- COPD$Diabetes * COPD$AtrialFib 

#code for the regression, using the command: 
r1 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(DAF), data=COPD) 

#view the regression using the function summary(r1) and the confidence intervals using the command confint(r1). 

#As a shortcut, you can also code this in the following manner: 
r2 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(Diabetes*AtrialFib), data=COPD)

#looking at predictions
#not essential but another helpful tool to determine variable relevance
install.packages("prediction")
library(prediction)
list("Diabetes" = prediction(r2, at = list(Diabetes = c(0,1))),
     "AtrialFib" = prediction(r2, at = list(AtrialFib = c(0,1))),
     "Diabetes*AtrialFib" = prediction(r2, at = list(Diabetes = c(0,1), AtrialFib = c(0,1))))
