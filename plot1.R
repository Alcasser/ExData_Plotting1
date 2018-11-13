dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dataPath <- "./household_power_consumption.txt"

startDate <- as.Date("2007-02-01")
endDate <- as.Date("2007-02-02")

fetchData <- function(dataUrl) {
  temp <- tempfile()
  download.file(dataUrl, temp)
  unzip(temp)
  unlink(temp)
}

generatePlots <- function() {
  library(dplyr)
  
  if(!file.exists(dataPath))
    fetchData(dataUrl)
  
  data <- read.csv(dataPath, sep = ";", stringsAsFactors = F, na.strings = "?")
  data <- data[complete.cases(data), ]
  data <- data %>% mutate(Global_active_power = as.numeric(Global_active_power)) %>% 
    mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
    filter(between(Date, startDate, endDate))
  
  print(data$Date)
}

generatePlots()