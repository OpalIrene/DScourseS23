library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(readxl)

#Borrower Race or National Origin
#1=American Indian or Alaskan Native; 
#2=Asian; 
#3=Black or African American; 
#4=Native Hawaiian or Other Pacific Islander; 
#5=White; 
#7=Information not provided by applicant in mail or telephone application
# Gender
#1=Male; 
#2=Female; 
#3=Information not provided by applicant in mail or telephone application; 
#4=No Co-Borrower
# Credit Scores are separated into ranges:  1 = <620, 2 = 620 to < 660, 3 = 660 < 700, 4 = 700 < 760, 5 = 760 or greater

df1 <- read_excel("/Users/home/Desktop/ECON 5253/DScourseS23/FinalProject/2019FHL.xlsx")
df1 <- data.frame(df1)
names(df1) <- c("totmonthlyincome","LTV",
                  "bo1race","bo1gender","bo1age",
                  "noteratepercent","noteamt",
                  "hsexpenseratio","debtexpenseratio")

df1 <- df1 %>% mutate(year = 2019)

df2 <- read_excel("/Users/home/Desktop/ECON 5253/DScourseS23/FinalProject/2021FHL.xlsx")
df2 <- data.frame(df2)
names(df2) <- c("totmonthlyincome","LTV",
               "bo1race","bo1gender","bo1age",
               "noteratepercent","noteamt",
               "hsexpenseratio","debtexpenseratio")

df2 <- df2 %>% mutate(year = 2021)

# Make sure continuous variables are formatted as numeric
df1 %<>% mutate(across(c("totmonthlyincome","LTV","bo1age","noteratepercent","noteamt",
                        "debtexpenseratio"), as.numeric))
# Make sure discrete variables are formatted as factors
df1 %<>% mutate(across(c("bo1gender","bo1race"), as.factor))

df1 %<>% mutate(lognoteamt = log(noteamt))
df1 %<>% mutate(logtotmonthincome = log(totmonthlyincome))
df1 %<>% select(lognoteamt, logtotmonthincome, noteratepercent, LTV, bo1race, bo1age, bo1gender, hsexpenseratio, debtexpenseratio, year)

# df 2 mutate code 

# format as numeric
df2 %<>% mutate(across(c("totmonthlyincome","LTV","bo1age","noteratepercent","noteamt",
                         "debtexpenseratio"), as.numeric))
# Make sure discrete variables are formatted as factors
df2 %<>% mutate(across(c("bo1gender","bo1race"), as.factor))

df2 %<>% mutate(lognoteamt = log(noteamt))
df2 %<>% mutate(logtotmonthincome = log(totmonthlyincome))
df2 %<>% select(lognoteamt, year, logtotmonthincome, noteratepercent, LTV, bo1race, bo1age, bo1gender, hsexpenseratio, debtexpenseratio)

datasummary_skim(df1,histogram=TRUE) # check to see 
datasummary_skim(df2,histogram=TRUE) # check to see with histogram
datasummary_skim(df1,histogram=FALSE)
datasummary_skim(df2,histogram=FALSE) # check to see without histogram

# mutate both years together after cbind
df <- rbind(df1, df2)
df <- df %>% mutate(year= as.factor(year))

datasummary_skim(df,histogram=TRUE) # check to see 

# ltv
est1 <- lm(LTV ~ lognoteamt + noteratepercent + bo1race*year + bo1gender*year + bo1age + debtexpenseratio, data=df)
est1
coef(lm(LTV ~ lognoteamt + noteratepercent + bo1race*year + bo1gender*year + bo1age + debtexpenseratio, data=df))

library(modelsummary)
tidy(est1)
summary(est1)

# Estimate the regression model with an intercept at zero using lm()

# Call:
#  lm(formula = LTV ~ lognoteamt + noteratepercent + bo1race * year + 
#       bo1gender * year + bo1age + debtexpenseratio, data = df)

# Residuals:
#  Min      1Q  Median      3Q     Max 
# -66.639  -7.178   0.954   9.303  37.741 

# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -10.62301    7.54128  -1.409  0.15908    
# lognoteamt            6.63556    0.55365  11.985  < 2e-16 ***
# noteratepercent       4.85497    0.73952   6.565 6.47e-11 ***
# bo1race2             -7.54544    3.05848  -2.467  0.01370 *  
# bo1race3             -1.25218    3.20649  -0.391  0.69620    
# bo1race4             10.52857   13.97287   0.754  0.45123    
# bo1race5             -4.76355    1.43934  -3.310  0.00095 ***
# year2021             -0.06996    2.25285  -0.031  0.97523    
# bo1gender2            1.77288    0.88610   2.001  0.04554 *  
# bo1age               -0.31515    0.02127 -14.817  < 2e-16 ***
# debtexpenseratio      0.29758    0.03314   8.979  < 2e-16 ***
# bo1race2:year2021     9.80301    4.65489   2.106  0.03532 *  
# bo1race3:year2021     0.88545    4.48463   0.197  0.84350    
# bo1race4:year2021          NA         NA      NA       NA    
# bo1race5:year2021     1.98127    2.10128   0.943  0.34584    
# year2021:bo1gender2  -0.69886    1.28266  -0.545  0.58591    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 13.89 on 2194 degrees of freedom
# Multiple R-squared:  0.226,	Adjusted R-squared:  0.221 
# F-statistic: 45.75 on 14 and 2194 DF,  p-value: < 2.2e-16

modelsummary(est1, stars = T)
summary(est1)

# lognoteamount
est2 <- lm(lognoteamt ~ noteratepercent + LTV + bo1race*year + bo1gender*year + bo1age + debtexpenseratio + hsexpenseratio, data=df)
summary(est2)
coef(lm(lognoteamt ~ noteratepercent + LTV + bo1race*year + bo1gender*year + bo1age + debtexpenseratio + hsexpenseratio, data = df)) 
Call:
  lm(formula = lognoteamt ~ noteratepercent + LTV + bo1race * year + 
       bo1gender * year + bo1age + debtexpenseratio + hsexpenseratio, 
     data = df)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.65680 -0.35408 -0.00591  0.37239  1.65926 

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         11.4138188  0.1399465  81.558  < 2e-16 ***
#  noteratepercent     -0.0784339  0.0277452  -2.827  0.00474 ** 
#  LTV                  0.0093461  0.0007699  12.139  < 2e-16 ***
#  bo1race2             0.3137623  0.1138377   2.756  0.00590 ** 
#  bo1race3             0.0484186  0.1193184   0.406  0.68493    
#bo1race4             0.0370061  0.5200609   0.071  0.94328    
#bo1race5             0.1467065  0.0536095   2.737  0.00626 ** 
#  year2021             0.0312175  0.0838709   0.372  0.70977    
#bo1gender2          -0.2154022  0.0327097  -6.585 5.67e-11 ***
#  bo1age              -0.0008992  0.0008304  -1.083  0.27896    
#debtexpenseratio     0.0010746  0.0014860   0.723  0.46966    
#hsexpenseratio       0.0071392  0.0016733   4.267 2.07e-05 ***
#  bo1race2:year2021   -0.2196751  0.1733272  -1.267  0.20515    
#bo1race3:year2021   -0.0122286  0.1669414  -0.073  0.94161    
#bo1race4:year2021           NA         NA      NA       NA    
#bo1race5:year2021   -0.0161812  0.0782071  -0.207  0.83611    
#year2021:bo1gender2  0.0717689  0.0477083   1.504  0.13264    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.5167 on 2193 degrees of freedom
#Multiple R-squared:  0.1249,	Adjusted R-squared:  0.1189 
#F-statistic: 20.87 on 15 and 2193 DF,  p-value: < 2.2e-16
modelsummary(est2, stars = T)
summary(est2)

# noteratepercent
est3 <- lm(noteratepercent ~ debtexpenseratio + lognoteamt*year + LTV + bo1race*year + bo1gender*year + bo1age, data=df)
est3
summary(est3)
(coef(lm(noteratepercent ~ debtexpenseratio + lognoteamt*year + LTV + bo1race*year + bo1gender*year + bo1age, data=df)))

modelsummary(list(est1, est2, est3), stars = T)


# summary statistics
# t test
# coefficients 

# best prediction subgroup mean
df1 %>% 
  group_by(bo1race) %>%
  summarise(mean = mean(noteamt))

# A tibble: 5 × 2
#bo1race    mean
#<fct>     <dbl>
#1 1       167257.
#2 2       246547.
#3 3       177500.
#4 4       190495 
#5 5       196257.

df2 %>% 
  group_by(bo1race) %>%
  summarise(mean = mean(noteamt))

# df2 A tibble: 4 × 2
#bo1race    mean
#<fct>     <dbl>
# 1 1       186050.
# 2 2       232612.
# 3 3       191333.
# 4 5       215323.

df1 %>% 
  group_by(bo1gender) %>%
  summarise(mean = mean(noteamt))

#df1 A tibble: 2 × 2
#bo1gender    mean
#<fct>       <dbl>
#1 1         207322.
#2 2         167020.

df1 %>% 
  group_by(bo1gender) %>%
  summarise(mean = mean(noteratepercent))
## A tibble: 2 × 2
#bo1gender  mean
#<fct>     <dbl>
#1 1          3.97
#2 2          3.95

df2 %>% 
  group_by(bo1gender) %>%
  summarise(mean = mean(noteamt))
# A tibble: 2 × 2
# bo1gender    mean
# <fct>       <dbl>
# 1 1         222959.
# 2 2         189714.
df2 %>% 
  group_by(bo1gender) %>%
  summarise(mean = mean(noteratepercent))
# A tibble: 2 × 2
#bo1gender  mean
#<fct>     <dbl>
#1 1          2.80
#2 2          2.86

df %>% 
  group_by(bo1gender) %>%
  summarise(mean = mean(noteratepercent))

## A tibble: 2 × 2
#bo1gender  mean
#<fct>     <dbl>
#1 1          3.41
#2 2          3.44

