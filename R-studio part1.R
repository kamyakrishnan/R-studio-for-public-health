# Script for importing cancer data for Statistical Thinking for Public Health MDDC

#Week 1 R-studio guide: using ggplot2 and Chi-squared

#arrow means we are assigning something
g <- read.csv(file='cancer-data-for-MOOC-1-_1_.csv', header = TRUE, sep = ',')
# datatables are seperated by columns, but it is good to be explicit about the commas

as.factor()
#converts a vector object g to a factor

g[,'fruit']
#this selects data from the column fruit
g$fruit
#This also selects data from the column fruit

hist(age)
hist(g$fruit_veg, xlab = "Portions of fruit and vegetables", main = "Daily consumption of fruit and vegetables combined")
# histogram
# maodifying x-label on histogram 

axis(side = 1, at = seq(0, 11, 1))
axis(side = 2, at = seq(0, 16, 2))
#The “at” tells R where you want the tick marks to be drawn.

summary(bmi)
#summary stats

#ggplot instructions


install.packages("ggplot2")
#download ggplot2 packages

require(ggplot2)
#load your required package


ggplot() + geom_histogram(data = g, aes(x = fruit_veg), bins = 10, fill = "darkgreen", col = "black") +
labs(x = "Portions of fruit and vegetables", y = "Frequency") + scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) + theme_bw()
#data is where you state your dataset 
#aes() is where you describe the variables
#bins establish set intervals for your data
#fill sets the color
#col creates a border
# + labs() adds the label feature for both x and y simultaneously
#scale_x_continuous creates a scaling grid for  your data to be m apped agains

g$healthy_BMI <- ifelse(g$bmi > 18.5 & g$bmi < 25, 1, 0)
#if g$bmi is greater than 18.5 AND less than 25, assign 1 to g$healthy_BMI; otherwise assign 0. 

table(g$healthy_BMI)
#create a table to show # of 1's and 0's


#create a regular histogram for fruit
hist(g$fruit, xlab = "Portions of fruit",
main = "Daily consumption of fruit", axes = F)

axis(side = 1, at = seq(0, 4, 1))
axis(side = 2, at = seq(0, 24, 4))
#label the axis sides
#axis side1 from 0->4 with intervals of 1
#axis side2 from 0->24 with intervals of 4

#create a histogram with ggplot that is same as above
ggplot() + geom_histogram(data = g, aes(x = fruit), bins = 5, fill = "darkgreen", col = "black") +
theme_bw() + labs(x = "Portions of fruit", y = "Frequency") + scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1))

#Chi-squared
chisq.test(x=five_a_day,y=cancer) 

#independent samples T-test in R
t.test(bmi~cancer)

#test whether mean= value of interest in t-test
t.test(bmi,mu=25) # the null hypothesis here is that the mean BMI is 25

#formal intro code from module
# To check which directory you are working in: 

getwd() 



# To import the data set 

# you need to change the “file” location to where you’ve stored the data set  

g <- read.csv(file = "Q:/MPHMOOC/R/cancer data for MOOC 1.csv", 
              
              header = TRUE, sep = ',')  





# To have a look at the first few rows of our data set: 

head(g) 



# To inspect the `age` variable: 

g$age 



# To display a summary of the ages of our patients: 

summary(g$age) 



# To display a summary of the genders of our patients: 

table(g$gender) 



# To display a summary of the BMI of our patients: 

summary(g$bmi) 



# To display a summary of the smoking status of our patients: 

table(g$smoking) 



# To display a summary of the exercise status of our patients: 

table(g$exercise) 



# To display a summary of the daily fruit consumption of our patients: 

table(g$fruit) 



# To display a summary of the daily vegetable consumption of our patients: 

table(g$veg) 



# To display a summary of the cancer status of our patients: 

table(g$cancer) 



# To create a new variable `fruitveg`, which sums the daily consumption of fruit and veg of each patient: 

g$fruitveg <- g$fruit + g$veg 



# To display a summary of the combined fruit and veg consumption of our patients: 

table(g$fruitveg) 



# To display a histogram of the ages of our patients: 

hist(g$age) 



# To create a new binary variable `five_a_day`, whether the patient eats at least 5 fruit or veg a day: 

g$five_a_day <- ifelse(g$fruitveg >= 5, 1, 0) 



# To summarise the `five_a_day` variable: 

table(g$five_a_day) 





# To display a histogram of the daily fruit and veg consumption of our patients, including a title and proper axes: 

hist(g$fruitveg, xlab = "Portions of fruit and vegetables", 
     
     main = "Daily consumption of fruit and vegetables combined", axes = F) 

axis(side = 1, at = seq(0, 11, 1)) 

axis(side = 2, at = seq(0, 16, 2)) 





# To create a new binary variable `healthy_BMI`, whether the patient has a healthy BMI or not: 

g$healthy_BMI <- ifelse(g$bmi > 18.5 & g$bmi < 25, 1, 0) 



# To summarise `healthy_BMI`: 

table(g$healthy_BMI) 





# To run a chi-squared test to look for an association between eating five or more fruit and veg a day and cancer: 

chisq.test(x = g$five_a_day, y = g$cancer) 



# To run a (two-tailed) t-test to see whether the mean BMI of those with cancer is different from the mean BMI of those without cancer: 

t.test(g$bmi ~ g$cancer) 



# To run a (two-tailed) t-test to see whether the mean BMI of those with cancer is different from the mean BMI of those without cancer, where the variances are equal: 

t.test(g$bmi ~ g$cancer, var.equal = T) 





# To run a t-test to see whether the mean BMI of all patients is different from 25: 

t.test(g$bmi, mu = 25) 



# To run a chi-squared test to see whether there is an association between eating five or more fruit a day and having cancer: 

chisq.test(x = g$five_a_day, y = g$cancer) 





# To create a new binary variable, whether overweight or not according to their BMI: 

g$overweight <- ifelse(g$bmi >= 25, 1, 0) 



# To summarise the `overweight` variable: 

table(g$overweight) 



# To run a chi-squared test to see whether there is an association between being overweight and cancer: 

chisq.test(x = g$overweight, y = g$cancer) 

