library(mice)
library(modelsummary)
library(magrittr)
library(readr)
library(dplyr)
library(broom)
library(knitr)


# 4. load wages
wages <- read_csv("ProblemSets/PS7/wages.csv")

spec(wages)
df <- as_tibble(wages)
view(df)
summary(df)
datasummary_skim(df,histogram=FALSE)

df <- df %>% 
  mutate(college_char = as.character(college),
         college_num = ifelse(college_char == "college grad", 1, 0))
df <- df %>% 
  mutate(married_char = as.character(married),
         married_num = ifelse(married_char == "married", 1, 0))

df2 <- select(df, logwage, hgc, college_num, tenure, age, married_num)
df2 <- as.data.frame(df2)

# 5. drop obvs
df3 <- na.omit(df2)
str(df3)

# 6. 
datasummary_skim(df,histogram=FALSE)
datasummary_skim(df3,histogram=FALSE)

est <- lm(logwage ~ hgc + college_num + tenure + age + married_num, data = df3)
modelsummary(est)
summary(est)
modelsummary(est, output = "table.tex")
modelsummary(est, output = "Fraser_PS7.docx")
# 25% of log wages are missing. The missingness is not related to any other varibles
# in the data so it may be MCAR.

# 7. 
# a. completed cases
# create linear regression model with complete cases only
df4 <- complete.cases(df3)
str(df4)

est1 <- lm(logwage ~ hgc + college_num + tenure + tenure^2 + age + married_num, data = na.omit(df4))
modelsummary(est1)

# b. mean imputation to fill in missing log wages

# Calculate mean of non-missing logwage values
mean_logwage <- mean(df$logwage, na.rm = TRUE)
table(df$logwage)
table(df$logwage_imputed)

# Replace missing logwage values with mean
df$logwage_imputed <- ifelse(is.na(df$logwage), mean_logwage, df$logwage)

# Print number of missing values in logwage variable after imputation
sum(is.na(df$logwage_imputed))

est2 <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data = na.omit(df))
summary(est2)
modelsummary(est2)

# c. impute 
df$logwage_imputed <- ifelse(is.na(df$logwage), predict(est2, newdata = df), df$logwage)

# Fit linear regression model
est3 <- lm(logwage ~ hgc + college + tenure + age + married, data = df)
modelsummary(est3)

# d.perform multiple imputation on the dataset
df_mice <- mice(df, m = 5, printFlag = TRUE)

# completed data sets
completed_data <- complete(df_mice)

# linear regression model to each imputed dataset
models <- with(df_mice, lm(logwage ~ hgc + college + tenure + age + married))

# combine results using pool()
p_model <- pool(models)

# summary table of pooled model
summary_table <- summary(p_model)
summary_table


# Combine results into a table using tidy() function from broom package
final <- modelsummary(list(est, est1, est2, est3), stars = T)
final


# The true value of ˆb1 = 0.093. If the true value of ˆb1 is 0.093 it is close to b1 = 0.062. 
# We would need to have infinite number of observations in the dataset to produce the exact ˆb1 = 0.093. 
# Comment on the differences of ˆb1 across the models. My b1 was the same 
# across the four models. Not sure if I did them correctly or not.
# What patterns do you see? hgc, college, and tenure are statistically significant across the four models.
# They are positive in direction so they have positive relationship with logwage.
# What can you conclude about the veracity of the various imputation methods? Also discuss what the estimates of ˆb1 are for the last two
# methods. The estimate for b1 in the last two models is 0.062. This suggests that the last two methods are reliable for the data.
