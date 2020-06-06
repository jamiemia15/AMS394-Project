### Jamie MacKay 
### AMS394 Project

## Question 1:
## Using the “ToothGrowth” dataset in R. Test by using α= .01. The response is the length of
## odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of
## three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, orange
## juice or ascorbic acid (a form of vitamin C and coded as VC). Test by using α= .01.
## There are three variables in this dataset
## Column  name  type     description
## [,1]    len   numeric  Tooth length
## [,2]    supp  factor   Supplement type (VC or OJ).
## [,3]    dose  numeric  Dose in milligrams/day
## 1) Choose appropriate test to test if the mean tooth length of each level of dose are the same.
## 2) Conduct a pairwise t test with Bonferroni method for each level of dose. And conclude
## which ones appear to be different?
## 3) Choose appropriate test to test if the mean tooth length of VC and OJ group are the same.
## 4) Conduct two-way ANOVA test with interaction to test if supplement type and dose have
## significant effect on the tooth length.

##(1)
library(ISwR)
data(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose, levels=c(0.5, 1, 2))
t <- aov(ToothGrowth$len~ToothGrowth$dose, data = ToothGrowth)
summary(t)
## the p-value is 9.53e-16 so we reject the null hypothesis and think the mean tooth length is different for each dose
##(2)
pairwise.t.test(ToothGrowth$len, ToothGrowth$dose, p.adj="bonferroni")
##   0.5     1      
## 1 2.0e-08 -      
## 2 4.4e-16 4.3e-05
## The p-values are extremely small for all pairs. So for every pair, we reject the null hypothesis and think they are all different
##(3)
ToothGrowth$supp = factor(ToothGrowth$supp, levels = c("VC", "OJ"))
t2 <- aov(ToothGrowth$len~ToothGrowth$supp, data = ToothGrowth)
summary(t2)
## The p-value is 0.0604 which is greater than 0.01 so we cannot reject the null hypothesis and think the mean tooth length is the same for OJ and 
##(4)
fit1 = lm(ToothGrowth$len~ToothGrowth$supp+ToothGrowth$dose)
anova(fit1)
## The p-value is <2e-16 for dose so we reject the null hypothesis and think dose does have a significant effect on the tooth length. The p-value for supp is 0.0004293 so we reject the null hypothesis and think supp does have a significant effect on the tooth length.





##Question 2
## Analyze and interpret the effect of explanatory variables on the milk intake (dl.milk) in the kfm
## data set (ISwR) using a multiple regression model. Test by using α= .05.
## 1) Run regression for dl.milk on all other variables. Do you find any significance that milk
## intake can be explained by other variables?
## 2) Find regression models in which fewer explanation variables should be used. i.e., select a
## subset of variables so that a better fit can be achieved. 

##(1)
data(kfm)
y = dl.milk
x1 = kfm$no
x2 = kfm$sex
x3 = kfm$weight
x4 = kfm$ml.suppl
x5 = kfm$mat.weight
x6 = kfm$mat.height
mr = lm(y~x1+x2+x3+x4+x5+x6)
summary(mr)
## Coefficients:
##          Estimate Std. Error t value Pr(>|t|)    
## x1      -0.005522   0.005286  -1.045 0.302010    
## x2boy  -11.652909   4.357128  -2.674 0.010542 *  
## x2girl -12.141666   4.318292  -2.812 0.007395 ** 
## x3       1.311822   0.324088   4.048 0.000212 ***
## x4      -0.002432   0.001254  -1.939 0.059077 .  
## x5       0.002453   0.023956   0.102 0.918925    
## x6       0.076445   0.030401   2.515 0.015739 *  
## milk intake can be explained by the variables no, sex$boy, ml.suppl, and mat.weight because the p-values for those variables are larger than 0.05.
##(2)
fit2 = lm(y~x2+x3+x6+0)
summary(fit2)
## Coefficients:
##         Estimate Std. Error t value Pr(>|t|)    
## x2boy  -10.97682    4.07253  -2.695 0.009788 ** 
## x2girl -11.44764    4.03507  -2.837 0.006750 ** 
## x3       1.33965    0.31525   4.249 0.000103 ***
## x6       0.06923    0.02605   2.658 0.010781 *  
## This is a better fit because we selected a smaller subset of variables with higher significance and our result shows higher t-values and lower p-values.

