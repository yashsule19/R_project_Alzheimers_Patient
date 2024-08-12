# My working directory:
setwd("C://Users/Yash Shule/Desktop/Statistical Data Analysis and Visualization Coursework")

#Removing all the variables,values, datasets, models from the environment:
rm(list = ls())

#Clearing the plot area or any previous graphs:
graphics.off()

# Part 1:
# a) How many variables are in the dataset?
  
AlzheimersData <- read.csv("AlzheimersData.csv", header = TRUE)
AlzheimersData
attach(AlzheimersData)

# b) What types of variables are they?

typeof(Subject)
typeof(ColourShapeCorrect)
typeof(GreyShapeCorrect)
typeof(Gender)
typeof(Age)
typeof(AlzheimersDisease)

# c) Report the summary statistics of the variables.
# Giving min, 1st quartile,median, mean, 3rd quartile, max for each variable 
# in dataset. 
summary(AlzheimersData)

# Number of Male and Females in dataset:
counts <- table(Gender)
counts

# Standard Deviation and Variance of each variable other than Gender 
# and AlzheimersDisease since they are categorical variables in dataset.
sd(Subject)
sd(ColourShapeCorrect)
sd(GreyShapeCorrect)
sd(Age)
var(Subject)
var(ColourShapeCorrect)
var(GreyShapeCorrect)
var(Age)

# d) What is the range of ages in the Alzheimer's Disease patients and healthy controls? 
aggregate(AlzheimersData$Age, by = list(AlzheimersData$AlzheimersDisease), FUN = range)


# e) Is the dataset balanced? Explain.
# Plotting Stacked Plot:
counts_MF <- table(Gender, AlzheimersDisease)
counts_MF
barplot(counts_MF,xlab = "Types of Patients",ylab = "Number of Participants",main = "Alzheimer Patients Vs Healthy Control Patients",col=c("orangered1","orchid1"), legend.text = c("Female","Male"), names = c("Alzheimer Patient","Healthy Patient"),cex.main = 1.2,cex.lab = 1.2, col.axis = "dodgerblue2", col.lab = "firebrick1", col.main = "firebrick1")
axis(2, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')

# f) Compute the summary statistics of the variables for Alzheimer's Disease and healthy controls separately. Discuss.
AlzheimersData_TRUE <- AlzheimersData[AlzheimersData$AlzheimersDisease == "TRUE",] 
AlzheimersData_TRUE
summary(AlzheimersData_TRUE)
countsgender_TRUE <- table(AlzheimersData_TRUE$Gender)
countsgender_TRUE

AlzheimersData_FALSE <- AlzheimersData[AlzheimersData$AlzheimersDisease == "FALSE",] 
AlzheimersData_FALSE
summary(AlzheimersData_FALSE)
countsgender_FALSE <- table(AlzheimersData_FALSE$Gender)
countsgender_FALSE

sd(AlzheimersData_TRUE$Subject)
sd(AlzheimersData_TRUE$ColourShapeCorrect)
sd(AlzheimersData_TRUE$GreyShapeCorrect)
sd(AlzheimersData_TRUE$Age)
var(AlzheimersData_TRUE$Subject)
var(AlzheimersData_TRUE$ColourShapeCorrect)
var(AlzheimersData_TRUE$GreyShapeCorrect)
var(AlzheimersData_TRUE$Age)
sd(AlzheimersData_FALSE$Subject)
sd(AlzheimersData_FALSE$ColourShapeCorrect)
sd(AlzheimersData_FALSE$GreyShapeCorrect)
sd(AlzheimersData_FALSE$Age)
var(AlzheimersData_FALSE$Subject)
var(AlzheimersData_FALSE$ColourShapeCorrect)
var(AlzheimersData_FALSE$GreyShapeCorrect)
var(AlzheimersData_FALSE$Age)


#Part - 2:
# Analysis - 1:
# Testing the difference in Age:
# 1) Creating Datasets of Ages for Healthy Control Patients and Alzheimers Disease Patients:
Age_FALSE <- AlzheimersData_FALSE$Age
Age_FALSE

Age_TRUE <- AlzheimersData_TRUE$Age
Age_TRUE

# Creating data frame:
Age_Data <- data.frame(Age_FALSE,Age_TRUE)
Age_Data

# Checking for normality separately:
# 2) Plotting of histogram and QQ - plots:
# Healthy Control Patients:
graphics.off()
par(mfrow = c(1,2))
hist(Age_FALSE, freq = FALSE, xlab = "Age of Healthy Control Patients", ylab = "Density", main = "Histogram of Healthy Control Patients",col="olivedrab1",cex.main = 1.2,cex.lab = 1.2, col.axis = "dodgerblue2", col.lab = "firebrick1", col.main = "firebrick1")
axis(1, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')
axis(2, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')
lines(density(Age_FALSE),lwd = 2, col = "darkorchid2", lty = 1)
qqnorm(Age_FALSE, col = "olivedrab1",pch = 16,cex.main = 1.2,cex.lab = 1.2,col.axis = "dodgerblue2", col.lab = "firebrick1", col.main = "firebrick1")
qqline(Age_FALSE, col = "darkorchid2", lwd = 3)
axis(1, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')
axis(2, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')

# Alzheimers' Disease Patients:
graphics.off()
par(mfrow = c(1,2))
hist(Age_TRUE, freq = FALSE, xlab = "Age of Alzheimers Disease Patients", ylab = "Density", main = "Histogram of Alzheimers Disease Patients",col="olivedrab1",cex.main = 1.2,cex.lab = 1.2, col.axis = "dodgerblue2", col.lab = "firebrick1", col.main = "firebrick1")
axis(1, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')
axis(2, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')
lines(density(Age_TRUE),lwd = 2, col = "darkorchid2", lty = 1)
qqnorm(Age_TRUE, col = "olivedrab1",pch = 16,cex.main = 1.2,cex.lab = 1.2,col.axis = "dodgerblue2", col.lab = "firebrick1", col.main = "firebrick1")
qqline(Age_TRUE, col = "darkorchid2", lwd = 3)
axis(1, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')
axis(2, col = 'red', col.axis = "dodgerblue2", col.ticks = 'red')

# 3) Apply the Kolmogorov - Smirnov Test
ks.test(Age_FALSE,"pnorm", mean = mean(Age_FALSE), sd = sd(Age_FALSE))
ks.test(Age_TRUE,"pnorm", mean = mean(Age_TRUE), sd = sd(Age_TRUE))

# 4) Perform parametric test: 
# Welch two sample t-test (Unpaired data : Since subjects are different in both groups)
t.test(Age_FALSE,Age_TRUE,paired = FALSE)

# 1) Boxplot - 1:
graphics.off()
boxplot(Age ~ Gender:AlzheimersDisease, data = AlzheimersData , notch = TRUE, col = c("mediumorchid1","palevioletred2"),main = "Ages of Healthy and Alzheimer Patients", xlab = "Patient Types with Gender", ylab = "Ages",names = c("HealthyP:F","HealthyP:M","AlzheimerP:F","AlzheimerP:M"),cex.main = 1.2,cex.lab = 1.2, col.axis = "dodgerblue2", col.lab = "firebrick1", col.main = "firebrick1")

# Testing the difference in Gender:
# Applying the chi - square distribution since it is a frequency 
Chisq_data <- matrix(c(22,23,20,25),nrow = 2, byrow = TRUE)
chisq.test(Chisq_data)

# Analysis - 2:
# Correlation Analysis of Age and Gender:
# Case 1: Correlation between Age and ColourShapeCorrect
data1_AgeColour <- cor(Age, ColourShapeCorrect)
data1_AgeColour
# Testing of the hypothesis:
cor.test(Age, ColourShapeCorrect)

# Case 2: Correlation between Age and GreyShapeCorrect
data2_AgeGrey <- cor(Age, GreyShapeCorrect)
data2_AgeGrey
# Testing of the hypothesis:
cor.test(Age, GreyShapeCorrect)

# Case 3: Correlation between Age, ColourShapeCorrect and GreyShapeCorrect
data3_AgeColourGrey <- data.frame(Age, ColourShapeCorrect, GreyShapeCorrect)
data3_AgeColourGrey
cor(data3_AgeColourGrey)

# Visual representation of correlations:
library(psych)
pairs.panels(data3_AgeColourGrey,hist.col="coral1",main = "Correlation visual statistics")
describe(data3_AgeColourGrey)

# Biserial Correlation:
# Case 4: Correlation between Gender and ColourShapeCorrect
install.packages("ltm")
library(ltm)
biserial.cor(ColourShapeCorrect,Gender,use = c("all.obs"),level = 2)

# Case 5: Correlation between Gender and GreyShapeCorrect
biserial.cor(GreyShapeCorrect,Gender,use = c("all.obs"),level = 2)


# Analysis - 3:
# Regression Analysis of Age:
# Case - 1 and 2:
Model1 <- lm(ColourShapeCorrect ~ Age)
summary(Model1)
ks.test(Model1$residuals,"pnorm",mean = mean(Model1$residuals),sd = sd(Model1$residuals))

Model2 <- lm(GreyShapeCorrect ~ Age)
summary(Model2)
ks.test(Model2$residuals,"pnorm",mean = mean(Model2$residuals),sd = sd(Model2$residuals))

graphics.off()
par(mfrow = c(1,2))
plot(Age,ColourShapeCorrect, col = "red", pch = 16, cex.lab = 1.2, col.axis = "dodgerblue2", col.lab = "firebrick1")
abline(lm(ColourShapeCorrect ~ Age),col = "blue", lwd = 4)
plot(Age,GreyShapeCorrect, col = "green", pch = 16, cex.lab = 1.2, col.axis = "dodgerblue2", col.lab = "firebrick1")
abline(lm(GreyShapeCorrect ~ Age), col = "purple", lwd = 4)
mtext("Linear Regression plots", side = 3, line = -2, outer = TRUE, cex = 1.5, col = "firebrick1")

#Case - 3: Multiple Linear Regression
Model3 <- lm(Age ~ ColourShapeCorrect + GreyShapeCorrect)
summary(Model3)

#Analysis - 4:

wilcox.test(ColourShapeCorrect[AlzheimersDisease == TRUE], ColourShapeCorrect[AlzheimersDisease == FALSE])
# 2) Boxplot - 2:
graphics.off()
boxplot(ColourShapeCorrect ~ Gender:AlzheimersDisease, data = AlzheimersData , notch = TRUE, col = c("springgreen2","violetred1"),main = "Colour Shapes Correct Trials", xlab = "Patient Types with Gender", ylab = "No. of Correct Trials",names = c("HealthyP:F","HealthyP:M","AlzheimerP:F","AlzheimerP:M"),cex.main = 1.2,cex.lab = 1.2, col.axis = "dodgerblue2", col.lab = "firebrick1", col.main = "firebrick1")

wilcox.test(GreyShapeCorrect[AlzheimersDisease == TRUE], GreyShapeCorrect[AlzheimersDisease == FALSE])
# 3) Boxplot - 3:
boxplot(GreyShapeCorrect ~ Gender:AlzheimersDisease, data = AlzheimersData , notch = TRUE, col = c("slateblue2","turquoise1"),main = "Grey Shapes Correct Trials", xlab = "Patient Types with Gender", ylab = "No. of Correct Trials",names = c("HealthyP:F","HealthyP:M","AlzheimerP:F","AlzheimerP:M"),cex.main = 1.2,cex.lab = 1.2, col.axis = "dodgerblue2", col.lab = "firebrick1", col.main = "firebrick1")
