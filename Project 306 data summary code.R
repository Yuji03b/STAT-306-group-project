# Install Libraries
install.packages("tidyverse")
install.packages("leaps")
install.packages("GGally")
install.packages("dplyr")
library(tidyverse)
library(leaps)
library(GGally)
library(dplyr)

# load the data set
insurance <- read.table("insurance.csv", header = TRUE, sep = ",")
head(insurance)

## no NA values present in the datatset
sum(is.na(insurance))

## change data type to factor
insurance$sex=as_factor(insurance$sex)
insurance$smoker=as_factor(insurance$smoker)
insurance$region=as_factor(insurance$region)
insurance$children=as_factor(insurance$children)

# remove index column
insur = subset(insurance, select = -index)
head(insur)
view(insur)

# histogram of charges
# Visualization of charges - Histogram
hist(insur$charges, main = "Histogram of Insurance Charges", xlab = "Charges in $")
hist(log(insur$charges), main = "Histogram of Log Insurance Charges", xlab = "Log Charges in $")

## log transformed y
ins = insur
ins$charges = log(ins$charges)
ins$log.charges = ins$charges
ins = subset(ins, select = -charges) 
head(ins)

## correlation matrix
ins.num = subset(ins, select = c(-sex,-children,-smoker,-region))
corr_mat=round(cor(ins.num),4)
view(corr_mat)


## Age and Charges
plot(ins$age, ins$log.charges, main = "Scatterplot of Log Charges against Age", ylab = " Log Charges", xlab = "Age")
cor(ins$age, ins$log.charges)
# low correlation between age and charges
# scatter plot shows an overall increasing trend -- an increase in age is correlated to an increase in charges

## BMI and Charges
plot(ins$bmi, ins$log.charges, main = "Scatterplot of Log Charges against BMI", ylab = " Log Charges", xlab = "BMI")
cor(ins$bmi, ins$log.charges)

## Sex and Charges
boxplot(ins$log.charges ~ ins$sex, main = "Boxplot of Log Charges grouped by Sex", ylab = "Log Charges", xlab = "Sex")
# Charges show a similar distribution by sex, with females being more clustered and dense as compared to the male conterparts.  

## Children and Charges
boxplot(ins$log.charges ~ ins$children, main = "Boxplot of Log Charges grouped by Num of Children", ylab = "Log Charges", xlab = "Num of Children")
# Distribution is not uniform across the number of children

## Smoker and Charges
boxplot(ins$log.charges ~ ins$smoker, main = "Boxplot of Log Charges grouped by Smoker", ylab = "Log Charges", xlab = "Smoker")
# Distribution of charges shows great variation and differences when grouped by whether or not the individual smokes

## Region and Charges
boxplot(ins$log.charges ~ ins$region, main = "Boxplot of Log Charges grouped by Region", ylab = "Log Charges", xlab = "Region")
# Distribution of charges looks relatively consistent across the 4 regions. 

# data summary of continuous
inss = ins
inss$charges = insur$charges
insur_summary <-
  inss %>%
  select(c(-sex,-smoker,-region,-children)) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>% 
  summarise(
    mean = mean(value,na.rm = T),
    sd = sd(value,na.rm = T),
    median = median(value, na.rm = T),
    variance = var(value, na.rm = T),
    max = max(value, na.rm = T),
    min = min(value, na.rm = T))
view(insur_summary)

# data summary of categorical variable
sex=table(ins$sex)
sexprop=prop.table(sex)
view(round(rbind(sex,sexprop),3))

children=table(ins$children)
childprop=prop.table(children)
view(round(rbind(children,childprop),3))

smoker=table(ins$smoker)
smokeprop=prop.table(smoker)
view(round(rbind(smoker,smokeprop),3))

region=table(ins$region)
regionprop=prop.table(region)
view(round(rbind(region,regionprop),3))
