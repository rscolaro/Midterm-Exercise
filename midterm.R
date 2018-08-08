#Midterm Exam Summer 2018
#Your Name: Ryan Scolaro
#
#Directions: provide R code and/or answers/comments on answers as directed
#in this R script file. Steps are actions to perform,
#questions require answers in the form of R code and/or answers/comments.
#Step 1:  Download and extract the folder for the exam named midtermsummer2018.
#
#Step 2:  place all files in this folder and zip the
#folder to turn in your work.  Set the working directory to this folder
#and save your R script file.
#
#Step 3:  Many packages and libraries have been developed
#for R.  When using a package, you want to be sure you can trust
#the package.  Generally packages are better when downloaded
#from CRAN, a user community, rather than GIT, a developer
#community.  Your first question requires that you download a new
#package and follow a tutorial to explore the package.
#
#Question 1:  Complete the tutorial on DataExplorer:
#add code from the tutorial to this script file
# DataExplorer tutorial:
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619
# note the chocolate folder is with your exam folder
# the chocolate file has been edited - you will
# get some missing values.
# html file will be generated and placed in your folder
# code goes here.
#(20)
install.packages('DataExplorer') 
library(DataExplorer)
choco = read.csv('flavors_of_cacao.csv', header = T, stringsAsFactors = F)
choco$Cocoa.Percent = as.numeric(gsub('%','',choco$Cocoa.Percent))
choco$Review.Date = as.character(choco$Review.Date)
plot_str(choco)
plot_missing(choco)
plot_histogram(choco)
plot_density(choco)
plot_correlation(choco, type = 'continuous','Review.Date')
plot_bar(choco)
create_report(choco)
#
#Question 2:  Rating is a numeric continuous variable
#from 1 to 5. We will use this to create a target variable.
#create a new variable target based on rating - target
#indicates whether or not the chocolate is desirable
#set target equal to 1 if the rating is 3.75 or higher
#set target equal to 0 if the rating is less than 3.75
#(5)
#
choco$target <- 0
choco$target[choco$Rating >= 3.75] <- 1
#
#Question 3:  Is the cocoa percent distribution skewed?
#calculate skewness and a normal probability
#plot of the cocoa percent distribution and comment.
#(5)
#
#install.packages("e1071") #already installed
library(e1071)
(skew = skewness(choco$Cocoa.Percent))
qqnorm(choco$Cocoa.Percent, datax = TRUE)
qqline(choco$Cocoa.Percent, datax = TRUE, col = "blue")
#Yes, the cocoa percent distribution is skewed, though not by a huge amount.
#
#Question 4: Create a new variable logcocoapercent and 
#and set the variable equal to the ln transformation
#in order to reduce skewness with the new logcocoapercent
#(5)
#
choco$logcocoapercent <- log(choco$Cocoa.Percent)
#
#Question 5:  were you successful in reducing skewness
#and achieving a more symmetrical distribution?  Show
#this.
#(10)
#
(skew = skewness(choco$logcocoapercent))
qqnorm(choco$logcocoapercent, datax = TRUE)
qqline(choco$logcocoapercent, datax = TRUE, col = "blue")
#The transformation did reduce the skewness, but did not eliminate it entirely.
#
#Question 6:  test for outliers in the cocoa percent.  If
#outliers are present do you recommend removing them?
#why or why not?
#(10)
#
zscore.Cocoa.Percent <- (choco$Cocoa.Percent - mean(choco$Cocoa.Percent))/sd(choco$Cocoa.Percent)
# show the lower range outliers
zscore.Cocoa.Percent[zscore.Cocoa.Percent < -3]
# show the upper range outliers
zscore.Cocoa.Percent[zscore.Cocoa.Percent > 3]
#Since outliers are present I would reccomend reomving them since they contribute to the skewness and
#nonnormal distribution we have already observed.
#
#Question 7:  Add an index variable.  Why do we do this?
#(3)
#
choco$index <- seq(nrow(choco))
#We do this to help keep the original order of the records, especially if we partiton the data later.
#
#Question 8:  Comment on the correlated variables, if any, as
#show from the DataExplorer output.
#(5 points)
#
#The REF and Review.Date columns are positively correlated because the REF (reference number) is lower for older
#dates and higher for newer ones.
#
#Question 9:  Conduct a one sample T test and
# construct a 95% confidence interval
# for the mean for the cocoa percent. Describe
# the confidence interval.
#(5 points)
#
(mean.Cocoa.Percent <- mean(choco$Cocoa.Percent))
mean.test <- t.test(x = choco$Cocoa.Percent, mu = mean.Cocoa.Percent, conf.level = 0.95)
mean.test$conf.int
#The confidence interval states that with 95% confidence the mean of Cocoa.Percent is between 71.40562 and
#71.99104. 
#
# 
#Question 10:  In advertising, a statement has been made
#that on average chocolate bars have 85% cocoa percent.
#What is the null hypothesis?
#what is the alternative hypothesis?
#Do we reject the null with alpha of .05? 
#Use R to show your work and explain the results.
#(10 points)
#
#Ho M = 0.85
#Ha M != 0.85
mean2.test <- t.test(x = choco$Cocoa.Percent, mu = 0.85)
mean2.test$p.value
#We reject the null hypothesis as the p value is 0.
#The null hypothesis also falls outside of the confidence interval we created previously so it can be rejected
#based off of that as well.
#
#
#Question 11:  Using the new attribute target 
#(a) find out the proportion of the chocolates that are desirable.
#(b)Conduct a one sample proportion test and calculate the confidence
#interval at 95% confidence - discuss the confidence interval.Suppose
#(c) we would like to test using level of significance a = .10 whether
#the population proportion differs from 19%. Calculate the p
#value and discuss the null, alternate hypothesis and result.
#(12 points)
#
#(a)
(table(choco$target))
(p <- 310 / nrow(choco))
#17% of the chocolates are desireable.
#(b)
(error <- qnorm(0.975, mean=0, sd=1) * sqrt((p * (1 - p)) / nrow(choco)))
(lower.bound <- p-error)
(upper.bound <- p+error)
#We can state that the percentage of desirable chocolates is between 15.5% and 19% with 95% confidence.
#9(c)
#Ho P = 0.19
#Ha P != 0.19
(Z_data <-(p-0.19)/sqrt((0.19*(1-0.19))/nrow(choco)))
(p.value <-2*pnorm(Z_data,mean=0,sd=1))
#The p value is less than 0.10 so we will reject the null hypothesis, which means the population proportion
#does differ from 19% at 90% confidence.
#
#
#Question 12:  Partition data into 75% training data
#and 25% test data (test).
#(5 points)
v <- runif(1795)
choco_train <- choco[v < 0.75, ]
choco_test <- choco[v >= 0.75, ]
#
#Question 13:  Is the data set balanced between
#good chocolate and not so good chocolate?
#why or why not?  show your code.
#(5 points)
(table(choco$target))
#No, there is slightly less than 5 times as much bad chocolate as there is good.
#