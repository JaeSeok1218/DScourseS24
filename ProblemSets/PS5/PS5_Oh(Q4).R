library(httr)
library(jsonlite)
library(ggplot2)

setwd("/Users/jaeseokoh/Oklahoma_University/Spring2024/Data_Science/DScourseS24/ProblemSets/PS5") # working directory

endpoint = "series/observations"
params1 = list(
  api_key = "9479ebdfd89daefc64b2dc7c9eeb9c45",
  file_type = "json",
  series_id = "DFTWRX1A020NBEA"
)

params2 = list(
  api_key = "9479ebdfd89daefc64b2dc7c9eeb9c45",
  file_type = "json",
  series_id = "MRTSSM4482USS"
)

shoeretail =
  httr::GET(
    url = "https://api.stlouisfed.org/",
    path = paste0("fred/", endpoint),
    query = params1
  ) %>%
  httr::content("text") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("observations") %>%
  as_tibble()%>%
  mutate(across(realtime_start:date, ymd)) %>%
  mutate(value = as.numeric(value)) 

shoestores =
  httr::GET(
    url = "https://api.stlouisfed.org/",
    path = paste0("fred/", endpoint),
    query = params2
  ) %>%
  httr::content("text") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("observations") %>%
  as_tibble()%>%
  mutate(across(realtime_start:date, ymd)) %>%
  mutate(value = as.numeric(value)) 


shoeretail <- subset(shoeretail, select = -c(realtime_start, realtime_end))
shoestores <- subset(shoestores, select = -c(realtime_start, realtime_end))


write.csv(shoeretail, file = "fred_data1.csv", row.names = FALSE)
write.csv(shoestores, file = "fred_data2.csv", row.names = FALSE)

# Convert 'date' column to date format if it's not already in date format
shoeretail$date <- as.Date(shoeretail$date)
shoestores$date <- as.Date(shoestores$date)
shoeretail$value <- shoeretail$value * 100
shoeretail$log_value <- log(shoeretail$value)
shoestores$log_value <- log(shoestores$value)
start_date <- as.Date("2007-01-01") 
end_date <- as.Date("2022-01-01")

# Filter the data for the specified time period
filtered_data_st <- shoestores %>%
  filter(date >= start_date & date <= end_date)

filtered_data_re <- shoeretail %>%
  filter(date >= start_date & date <= end_date)

# Plot shoeretail data
p <- ggplot(filtered_data_re, aes(x = date, y = log_value)) +
  geom_line(color = "blue") +
  labs(x = "Date", y = "Value") +
  theme_minimal()+
  scale_color_manual(name = "Line Name", values = c("Shoe Retail" = "blue", "Shoe Stores" = "red"))

# Add shoestores data to the plot
p <- p + geom_line(data = filtered_data_st, aes(x = date, y = log_value), color = "red") 

# Show the plot
print(p)

ggsave("plot.png", plot = p, width = 8, height = 6, dpi = 300)
