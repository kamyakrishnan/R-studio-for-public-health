#clear all previous variables from the console
rm(list=ls())

#load csv
COPD <- read.csv(file = 'COPD_student_dataset.csv')

#where you put the path file in quotation marks inside the parentheses.  
setwd()

#look at column names
colnames(COPD)

#histogram
hist(variable)

#Example below
hist(COPD$MWT1Best)

#detailed histogram
#main = “New figure title”
#breaks = and specifying the number of bins you want displayed. 
hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab="MWT1Best", breaks=12)

#There are no obvious impossible values, but there is quite a high value which is above 650. 
#Forthis specific value, you can look at it using the subset() function.
subset(dataframe, variable > x)
#so here: 
subset(COPD, MWT1Best > 650)

#if you now want to look at more than one value, let’s say samples which have values of MWT1Best 
#over 650 and under 150, you can use the following code: 
subset(COPD, MWT1Best > 600 | MWT1Best < 150) # where ‘|’ stands for ‘and’.

#same hist as MWT1Best except for FEV1
hist(COPD$FEV1, main="Histogram of FEV1", xlab="FEV1")   

#summary
summary()

#SD
sd()

#range
range()

#IQR
IQR()

#look at statistics together by assing 
list()

list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(COPD$MWT1Best, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE))

#calculate the correlation coefficient and have a look at the scatterplot of the two variables
#plot(x,y)
plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best") 

#calculate the Pearson’s correlation coefficient to assess a linear association between the two variables
cor.test(x,y) 

#need to remove missing values, otherwise you will have an error message.
#use = “complete.obs”

#example for using person correlation as the method
cor.test(COPD$FEV1, COPD$MWT1Best, use = "complete.obs", method = "pearson")

#Pearson’s correlation coefficient is 0.47 and the 95% confidence interval 
#suggests the population coefficient is likely to lie somewhere between 0.3 to 0.6. 
#The p value is less than 0.001 so there is very strong evidence against the null hypothesis of the coefficient being zero.   

#To run a linear regression in R, the function you need to use is lm()
lm(outcome ~ predictor, data =dataframe)

#manipulate and investigate this model, store it as an R object, i.e., assign it to a vector. 
#new vector name<-linear regression model

MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)

#view 95% confidence intervals
#use the command confint(linear reg vector):  
confint(MWT1Best_FEV1)

#view all 4 plots in one output by setting a viewing format of 2 by 2 plots
par(mfrow=c(2,2))

#view plots
#plot(MWT1Best_FEV1)

#multiple linear regression
#Model name <- lm(outcome ~ predictor1 + predictor2, data =dataframe) 
#Ex:
MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = COPD) 