library(readr)
library(dplyr)
library(modelsummary)
library(magrittr)


# 4.
# Load the data
wages12 <- read_csv("~/Desktop/ECON 5253/DScourseS23/ProblemSets/PS12/wages12.csv")
wages12

#5.
# Format the variables
wages12 <- wages12 %<>% 
  mutate(across(c("college","married","union"), as.factor))

# 6.
wages12 <- as.data.frame(wages12)
# Generate the summary table for the data frame
datasummary_skim(wages12,histogram=FALSE, output = "table.tex")

# Calculate the rate of missing log wages
missing_logwage_rate <- sum(is.na(wages12$logwage)) / nrow(wages12)

# Print the rate
print(missing_logwage_rate)
# [1] 0.3068641

# examine missingness
library(ggplot2)

ggplot(data = wages12, aes(x = exper, y = logwage)) +
  geom_miss_point() +
  theme_minimal()

ggplot(data = wages12, aes(x = married, y = logwage)) +
  geom_miss_point() +
  theme_minimal()

ggplot(data = wages12, aes(x = hgc, y = logwage)) +
  geom_miss_point() +
  theme_minimal()

# 7.
library(tidyverse)
library(sampleSelection)
library(broom)
#a.
complete_cases <- wages12 %>% drop_na(logwage)
complete_cases <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = complete_cases)
summary(complete_cases)
#b.
mean_logwage <- mean(wages12$logwage, na.rm = TRUE)

wages12_mean_imputed <- wages12 %>%
  mutate(logwage = if_else(is.na(logwage), mean_logwage, logwage))

lm_mean_imputed <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = wages12_mean_imputed)
summary(lm_mean_imputed)

#c.
wages12 <- wages12 %>%
  mutate(valid = !is.na(logwage),
         logwage = if_else(is.na(logwage), 0, logwage))

heckit_model <- selection(selection = valid ~ hgc + union + college + exper + married + kids,
                          outcome = logwage ~ hgc + union + college + exper + I(exper^2),
                          data = wages12, method = "2step")
summary(heckit_model)
# regression table with the estimates of the first two regression models:
table <- modelsummary(list("Complete Cases" = complete_cases,
                           "Mean Imputation" = lm_mean_imputed),
                      stars = TRUE, output = "tables.tex")

table


models <-modelsummary(list("Complete Cases" = complete_cases,
                    "Mean Imputation" = lm_mean_imputed,
                    "Heckman Selection" = heckit_model), stars= TRUE, output = "tables.tex")
models
table <- modelsummary(models_list, stars = TRUE)

# first two 
modelsummary(list(lm_complete_cases, lm_mean_imputed), stars = T, output = "tables.tex")

# model
modelsummary(heckoutcomeequation, output = "tabless.tex")


# 8. 

probit_model <- glm(union ~ hgc + college + exper + married + kids, data = wages12, family = binomial(link = "probit"))
summary(probit_model)

# Call:  glm(formula = union ~ hgc + college + exper + married + kids, 
# family = binomial(link = "probit"), data = wages12)

# Coefficients:
#  (Intercept)          hgc     college1        exper     married1  
#-6.7426      -1.0090       0.3972       1.8490       0.5878  
#kids  
#0.7993  

#Degrees of Freedom: 2228 Total (i.e. Null);  2223 Residual
#Null Deviance:	    2443 
#Residual Deviance: 228.6 	AIC: 240.6

library(car)
vif(probit_model)
#hgc  college    exper  married     kids 
#5.671536 1.348302 4.963587 1.062918 1.135585 

#### Binary choice model
library(mlogit)

# load data on residential heating choice in CA
data(Heating) 
levels(Heating$depvar) <- c("gas","gas","elec","elec","elec")

# estimate logit and get predicted probabilities
estim <- glm(depvar ~ income+agehed+rooms+region,family=binomial(link='logit'),data=Heating)
print(summary(estim))
Heating$predLogit <- predict(estim, newdata = Heating, type = "response")
print(summary(Heating$predLogit))

# estimate probit and get predicted probabilities
estim2 <- glm(depvar ~ income+agehed+rooms+region,family=binomial(link='probit'),data=wages12)
print(summary(estim2))
Heating$predProbit <- predict(estim2, newdata = Heating, type = "response")
print(summary(Heating$predProbit))


# counterfactual policy: electric heating subsidy to higher-income folks
estim$coefficients["income"] <- 4*estim$coefficients["income"]
Heating$predLogitCfl <- predict(estim, newdata = Heating, type = "response")
print(summary(Heating$predLogitCfl))




