#This script shows few most of interesting methods we can use in R for A/B testing

# The sanity check is a assamptions of the correct test, where we need pool proportion withing the CI
#sanity check for A/B testing
true_p = 0.5
t_cont = 15348
t_exp = 15312
sd = sqrt((0.5*0.5)/(t_cont+t_exp))
sd
m = sd*1.96 ; m
#CI = (true_p-m,true_p+m)
CI <-  c(true_p-m, true_p+m)
print(CI )
pool_p = (t_cont)/(t_exp+t_cont)
pool_p
# Since our pool proportion is still within the interval, this does pass sanity check. 
#Then we don't have to worry about checking further number of cookies by day data.

# import numpy as np   ## in python
#   true_p = 0.5
# t_cont = 15348
# t_exp = 15312
# sd = np.sqrt((0.5*0.5)/(t_cont+t_exp))
# m = sd*1.96
# CI = (true_p-m,true_p+m)
# print CI
# pool_p = float(t_cont)/(t_exp+t_cont)
# pool_p

#THERE ARE FEW DIFFERENT METHODS HOW WE CAN CALCULATE AB TEST 
#1
#Using R calculate the 95% Confidence Interval of the variant 𝑉1 which has 150 Clicks and 900 Impressions
CI<-prop.test(x=150,n=900, correct = FALSE, conf.level = 0.95)
CI$conf.int
# Using R test if the actual 𝑝 of the variant 𝑉1 could be considered as 0.17 and then test again for 0.20
#Hypothesis Testing for p=0.17
t1<-prop.test(x=150,n=900, p=0.17, alternative = c("two.sided"), conf.level = 0.95, correct = FALSE)
t1
#Hypothesis Testing for p=0.20
t2<-prop.test(x=150,n=900, p=0.20, correct = FALSE)
t2

#2
#Hypothesis Testing of Two Variants (AB Testing)
# Using R calculate compare the variant 𝑉1 which has 120 Clicks and 800 Impressions with variant 𝑉2 which has 100 Clicks and 700 Impressions
# define a vector of the responses
x<-c(120,100)
# define a vector of the impressions
n<-c(800,700)
test1<-prop.test(x,n, correct = FALSE)
test1

#3
# Hypothesis Testing of k Variants (ABn Testing) Chi-Square Test
#Assume that we have 8 variants with the following clicks (80,85,90,95,100,105,110,115) 
#respectively and all of them have 1000 impressions. Using R determine if all these variants can be considered equivalent.
x<-seq(from=80, by=5, length.out=8)
x  # [1]  80  85  90  95 100 105 110 115
n<-rep(1000,8)
n  # [1] 1000 1000 1000 1000 1000 1000 1000 1000
chisqtest<-prop.test(x,n, conf.level = 0.95)
chisqtest

#Multiple Pairwise Comparisons Without P-Value Adjustments
x<-seq(from=80, by=5, length.out=8)
n<-rep(1000,8)
ppt<-pairwise.prop.test(x, n, p.adjust.method = "none")
ppt

#Multiple Pairwise Comparisons With P-Value Adjustments  
#{“holm”, “hochberg”, “hommel”, “bonferroni”, “BH”, “BY”, “fdr”, “none”}
#False Discovery Rate as method of adjustment
x<-seq(from=80, by=5, length.out=8)
n<-rep(1000,8)
ppt<-pairwise.prop.test(x, n, p.adjust.method = "fdr")
ppt

#Multiple Pairwise Comparisons of Control Variant With P-Value Adjustments
x<-seq(from=80, by=5, length.out=8)
n<-rep(1000,8)
ppt<-pairwise.prop.test(x, n, p.adjust.method = "none")
# this vector is the p-values of variant 1 versus the rest 7 variants without adjustments
pvalue_vector<-ppt$p.value[,1]
pvalue_vector

# now apply the pvalue adjustment to the vector of pvalues
p.adjust(pvalue_vector, method = "fdr")

#Multiple Comparisons applying TukeyHSD Test - Logistic Regression applying the Tukey Test.
library(multcomp)

dataset<-data.frame(x=seq(from=80, by=5, length.out=8), n=rep(1000,8), ID=factor(c(1:8)))
dataset
model1<- glm(formula = cbind(x, n-x) ~ ID, family = binomial(link = "logit"), data=dataset)
model1
# Tukey multiple comparisons
summary(glht(model1, mcp(ID="Tukey")))
