# Read Data
setwd("/Users/jaeseokoh/Oklahoma_University/Spring2024/Data_Science/DScourseS24/ProblemSets/PS6")
sn_data <- read.csv("StockX-Data-Contest-2019-3.csv")

####################### Data Cleaning
# Type Check
variable.names <- colnames(sn_data)
for (variable.name in variable.names){
  print(typeof(sn_data[[variable.name]]))
}
# Numeric Variables
numeric.variable.names <- c("Sale.Price", "Retail.Price")
for (variable.name in numeric.variable.names){
  sn_data[[variable.name]] <- as.numeric(gsub("[\\$,]", "", sn_data[[variable.name]]) )
}
# Data Variables
date.variables <- c("Order.Date", "Release.Date")
for (variable.name in date.variables){
  sn_data[[variable.name]] <- as.Date(sn_data[[variable.name]], format = "%m/%d/%y")
}
# Generate id
sn_data$id <- as.numeric(factor(sn_data$Sneaker.Name, levels = unique(sn_data$Sneaker.Name)))

# Generate order_of_transaction(time-dimension)
library(dplyr)
sorted_data <- sn_data %>%
  arrange(id, Order.Date ) %>%
  group_by(id) %>%
  mutate(order_of_transaction = row_number()) %>%
  ungroup()

####################### Data Visualization
library(tidyverse)
library(modelsummary)
library(kableExtra)
library(ggplot2)

####################### Graph 1. Relationship btw Shoesize and Price
## Generate Main Variable: Price ratio(%)
sorted_data$PPratio <- 100*(sn_data$Sale.Price / sn_data$Retail.Price)

## Quadratic Model
sn_data$shoe_sq <- sn_data$Shoe.Size^2
model <- lm(PPratio ~ Shoe.Size + shoe_sq -1, data = sorted_data)
summary(model)
coefficients <- coef(model)
linear_function <- function(x) {
  coefficients[1] * x + coefficients[2] * x^2
}

## Generate data for plotting
predicted_data <- data.frame(Shoe.Size = sorted_data$Shoe.Size,
                             PPratio = predict(model, newdata = sorted_data))

## Plot observed vs. fitted values
ggplot(data = sorted_data, aes(x = Shoe.Size, y = PPratio)) +
  geom_point() +  # Plot observed values
  geom_line(data = predicted_data, aes(y = PPratio), color = "red") +  # Plot fitted values
  labs(title = "Observed vs. Fitted Values",
       x = "Shoe Size",
       y = "Sale Price") +
  theme_minimal()

ggsave("PS6a_Oh.png", width = 8, height = 6, units = "in")
####################### Graph 2. Time Trend of Sale Price for id=1, shoesize=9,10,13
## Filter data
filtered_data <- sorted_data %>%
  filter(id %in% c(1), Shoe.Size %in% c(9,10,13))

## Plot
ggplot(data = filtered_data, aes(x = order_of_transaction, y = Sale.Price, color = factor(Shoe.Size))) +
  geom_line() +
  labs(title = "Time Trend of Sale Price(Adidas Yeezy Boost 350)",
       x = "Order of Transaction",
       y = "Sale Price") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "Shoe Size") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

ggsave("PS6b_Oh.png", width = 8, height = 6, units = "in")
####################### Graph 3. Histogram of Sale Price
## Plot
ggplot(data = sorted_data, aes(x = Sale.Price)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Histogram of Sale Price",
       x = "Sale Price",
       y = "Frequency") +
  theme_minimal()

ggsave("PS6c_Oh.png", width = 8, height = 6, units = "in")