#Statistical Inference ToothGrowth Data

#The project consists of two parts:
#1. Simulation Exercise to explore inferenced
#2. Basic inferential analysis using the ToothGrowth data in the R datasets package


#Part 2 Statistical Inference using ToothGrowth Dataset Part 2

#Overview

#Analyze the ToothGrowth data in the R datasets package. 

#What We have to do 

#1. Load the ToothGrowth data and perform some basic exploratory data analysis
#2. Provide a basic summary of the data
#3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose
#4. State your conclusions and the assumptions needed for your conclusions

#Load Libraries 

library(data.table)
library(ggplot2)
library(datasets)

#Load the ToothGrowth dataset

toothGrowth <- data.table(ToothGrowth)

##Description of Dataset

#The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, (orange juice or ascorbic acid (a form of vitamin C and coded as VC).

##Format Of dataset

#A data frame with 60 observations on 3 variables.

# Exploratory Data Analysis

head(toothGrowth); 

tail(toothGrowth)

#The three variables are length, supplement, and dose.

summary(ToothGrowth)

str(ToothGrowth)

unique(ToothGrowth$dose)

#There are three discrete levels for dose: 0.5, 1.0, and 2.

#Visually examine the data by looking at the tooth length compared to dose by supplement.

ggplot(aes(x = supp, y = len), data = ToothGrowth) + 
  geom_boxplot(aes(fill = supp)) +
  labs(title = "Box_Plot of Tooth Length across Supplement Types", 
       x = "Supplement Type", y = "Tooth Length")

ggplot(aes(x=dose, y = len), data = ToothGrowth) + 
  labs(x = "Tooth Length", y = "Dosage Frequency", title = "Tooth Lengths by Dosage and Supplement") +
  geom_point(aes(color = supp))


ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ supp) +
  xlab("Dose") +
  ylab("Tooth Length") +
  guides(fill=guide_legend(title="Supplement"))

#There appears to be an impact on tooth growth by increasing the dosage but it is unclear which of the two supplements contributes to the growth.

#Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose

#Use t.test to determine if there is a difference in the performance of the treatments. 
#First, we will run the test based on supplement. 
#Looking to see if the p-value > 0.05 and if the confidence interval crosses 0.

t.test(ToothGrowth$len[ToothGrowth$supp=="OJ"], ToothGrowth$len[ToothGrowth$supp=="VC"], paired = FALSE, var.equal = FALSE)


#The p-value is 0.06063 and the confidence interval is [-0.1710156 7.5710156], thus containing 0.

#Since the p-value is 0.06063, there is not enough evidence to reject the null hypothesis.
#We cannot assume the delivery type has a significant effect on tooth growth.

#Test the tooth length comparing the dosage of 1mg to 2mg to determine the effects of an increased dosage.

t.test(ToothGrowth$len[ToothGrowth$dose==2], ToothGrowth$len[ToothGrowth$dose==1], paired = FALSE, var.equal = TRUE)

#We see the p-value is very small, and is significant. 
#Therefore, we can reject the null hypothesis and assume the dosage increase from 1mg to 2mg creates an positive effect on tooth growth.

#Next, perform the test comparing the dosage of 0.5mg to 1mg.

t.test(ToothGrowth$len[ToothGrowth$dose==1], ToothGrowth$len[ToothGrowth$dose==0.5], paired = FALSE, var.equal = TRUE)

#Again, we see the p-value is still small although slighty larger than the previous test, therefore, it is significant.

#We can again reject the null hypothesis and assume the dosage increase from .5mg to 1mg creates an positive effect on tooth growth.

#There is no need for futher testing of dosages given the previous tests.

# State conclusions and the assumptions needed for conclusions

#In this experiment, we assume there is a common variance in the population and that the guinea pigs were chosen at random.

#The delivery type does not show a significant increase in tooth growth even though it does have a confidence level that crosses 0 at the 95% confidence.

#However, there does appear to be a difference with an increase in tooth growth when the dosage is increased. 
#The tests comparing the dosage show confidence intervals of differences never crossing zero.




#Conclusion
#Increasing the dosage leads to an increase in tooth growth in guinea pigs.