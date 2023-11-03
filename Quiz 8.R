library(readxl)
Data <- read_excel("G:/8th semester/EM 524/23 11 03 LAB 08/Data1.xlsx")

# data structure
str(Data)

# Convert to factor variables
Data$`% of Hardwood Concentration`<-as.factor(Data$`% of Hardwood Concentration`)
Data$`Cooking Time`<-as.factor(Data$`Cooking Time`)
Data$`Freeness`<-as.factor(Data$`Freeness`)


# making anova table
attach(Data)
three_factor=lm(`Response`~`% of Hardwood Concentration`*`Cooking Time`*`Freeness`, data=Data)
anova(three_factor)


# interaction plot
interaction.plot(`Drying Time`, Patient, `Surface Finish`, data=Data)
# Since the two lines are parallel, there is no interaction between two factors


# Pairwise comparison
pairwise.t.test(`Surface Finish`, `Drying Time` : Patient, p.adjust.method = "bonferroni")
# Value < 0.05
# There is no significant difference between Low:1 & Low:2 and High:2 & Low:2 at 5% significance level.

# Test for normality
library(nortest) 

# 1. Anderson Darling Normality Test
ad.test(three_factor$residuals)
# P-value = 0.1311 > 0.05
# There fore, don't reject H0 at 5% significance level
# Residuals are normally distributed


# 2. Shapiro Wilk Normality Test
shapiro.test(three_factor$residuals)
# P-value = 0.4485 > 0.05
# There fore, don't reject H0 at 5% significance level
# Residuals are normally distributed



# 3. Normal Q-Q plot
qqnorm(three_factor$residuals)
qqline(three_factor$residuals, col="red")
# Most of the points are lie on the line.
# Therefore, residuals are normally distributed



# Test for constant variance (Bartlett Test)

bartlett.test(three_factor$residuals, `% of Hardwood Concentration`:`Cooking Time`:`Freeness`, data= Data)
# p value = 0.9259 > 0.05
# Therefore do not reject H0 at 5% significance level.
# Residuals have a constant variance.



# Durbin-Watson test
library(lmtest)
dwtest(`Response` ~ `% of Hardwood Concentration`*`Cooking Time`*`Freeness`, data=Data, alternative = "two.sided")
# P - vale = 0.3558 > 0.05
# Therefore don't reject H0 at 5% significance level
# Residuals are uncorrelated
