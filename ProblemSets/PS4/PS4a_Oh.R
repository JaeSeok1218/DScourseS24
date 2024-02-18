url <- "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"
destination_file <- "downloaded_file.json"
download.file(url, destfile = destination_file, method = "auto")


#wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"

library(tidyverse)
library(jsonlite)

setwd("/Users/jaeseokoh/Oklahoma_University/Spring2024/DScourseS24/ProblemSets/PS4")

mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])
class(mydf$date)
head(mydf, 10)