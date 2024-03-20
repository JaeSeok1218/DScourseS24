#install.packages("mice")
library(mice)
library(modelsummary)

setwd("/Users/jaeseokoh/Oklahoma_University/Spring2024/Data_Science/DScourseS24/ProblemSets/PS7")

# Load .csv data
df <- read.csv("wages.csv")

# Drop missing observations in variables of interest
df <- df[complete.cases(df[c('hgc', 'tenure')]), ]

# The number of missing data
n_missing <- sum(is.na(df$logwage))
# The percentage of missing data
p_missing <- n_missing / nrow(df)

# Compare missing-case and complete-case
df_test <- df[is.na(df$logwage), ]
datasummary_skim(df_test)
df_test$inter <- interaction(df_test$college, df_test$married)
table(df_test$college)
table(df_test$married)
table(df_test$inter)
#---------------------------
df_comp <- df[complete.cases(df),]
datasummary_skim(df_comp)
df_comp$inter <- interaction(df_comp$college, df_comp$married)
table(df_comp$college)
table(df_comp$married)
table(df_comp$inter)

# Compare hgc with missing logwage and complete logwage
boxplot(hgc ~ is.na(logwage), data = df, main = "Boxplot of hgc by logwage missingness", xlab = "Missing logwage")
# Export the boxplot as a .png file into the working directory
png("hgc_boxplot.png")


# Summary table
datasummary(All(df) ~ mean + SD + Min + Max + Median + P0 + P25 + P50 + P75 + P100, data = df)

#----------------------------------------------
#Linear Regression
#----------------------------------------------
#drop missing values in logwage
model_1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = df)
summary(model_1)


#----------------------------------------------
# Mean Imputation
#----------------------------------------------
data1 <- df
Mean <- function(x) {
  return(mean(x, na.rm = TRUE))
}
#df$logwage1[is.na(df$logwage)] <- Mean(df$logwage): check how it works
data1$logwage[is.na(data1$logwage)] <- Mean(df$logwage)

model_2 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = data1)
summary(model_2)

#----------------------------------------------
# Predicted Values Imputation
#----------------------------------------------
data2 <- df
#df1$logwage1[is.na(df1$logwage)] <- predict(model_1, newdata = df1[is.na(df1$logwage), ]) : check how it works
data2$logwage[is.na(data2$logwage)] <- predict(model_1, newdata = data2[is.na(data2$logwage), ])

model_3 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = data2)
summary(model_3)


#----------------------------------------------
# Multiple Imputation
#----------------------------------------------
df_imp <- mice(df, m = 5, maxit = 50, seed = 500)
model_4 <- with(df_imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
summary(pool(model_4)) # pool results following Rubin's rule

#----------------------------------------------
# Pool Results
#----------------------------------------------
mod <- list()
mod[['Raw']] <- model_1
mod[['Mean Imp']] <- model_2
mod[['Predicted Imp']] <- model_3
mod[['Multiple Imp']] <- model_4

mod[['Multiple Imp']] <- mice::pool(mod[['Multiple Imp']])

# Gerenate Summary Table
modelsummary(mod, stars = TRUE, gof_map = c("nobs", "r.squared") , output = 'latex')

#----------------------------------------------
# Detail Mean Imputation
#----------------------------------------------
data_r <- df
m_wage_m_grad <- Mean(df$logwage[df$college == "college grad" & df$married == "married"])
m_wage_m_nograd <- Mean(df$logwage[df$college == "not college grad" & df$married == "married"])
m_wage_s_grad <- Mean(df$logwage[df$college == "college grad" & df$married == "single"])
m_wage_s_nograd <- Mean(df$logwage[df$college == "not college grad" & df$married == "single"])

data_r$logwage[is.na(data_r$logwage) & data_r$college == "college grad" & data_r$married == "married"] <- m_wage_m_grad
data_r$logwage[is.na(data_r$logwage) & data_r$college == "not college grad" & data_r$married == "married"] <- m_wage_m_nograd
data_r$logwage[is.na(data_r$logwage) & data_r$college == "college grad" & data_r$married == "single"] <- m_wage_s_grad
data_r$logwage[is.na(data_r$logwage) & data_r$college == "not college grad" & data_r$married == "single"] <- m_wage_s_nograd

model_r <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = data_r)
summary(model_r)