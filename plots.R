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

generatePlotAsPng <- function(outputFileName, plotCreation, mfrow = c(1, 1)) {
  png(outputFileName, width = 480, height = 480, units = "px", bg = "white")
  par(mar= c(4, 4, 2, 1), mfrow = mfrow)
  plotCreation()
  dev.off()
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
  
  plot1 <- function() {
    hist(data$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
  }
  generatePlotAsPng("plot1.png", plot1)
  
  plot2 <- function() {
    dataWithDateAndtimeCol <- data %>% mutate(datetime = paste(Date, Time))
    datetime <- strptime(dataWithDateAndtimeCol$datetime, "%Y-%m-%d %H:%M:%S") 
    plot(datetime, data$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")
  }
  generatePlotAsPng("plot2.png", plot2)
  
  plot3 <- function() {
    plot(datetime, data$Sub_metering_1, type = "l", ylab = "Energy sub metering", xlab = "")
    lines(datetime, data$Sub_metering_2, type = "l", col = "red")
    lines(datetime, data$Sub_metering_3, type = "l", col = "blue")
    legend("topright", pch = "-", col = c("black", "red", "blue"),
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  }
  generatePlotAsPng("plot3.png", plot3)
  
  generatePlotAsPng("plot4.png", function() {
    plot2()
    plot(datetime, data$Voltage, type = "l", ylab = "Voltage", xlab = "")
    plot3()
    plot(datetime, data$Global_reactive_power, type = "l", ylab = "Global Reactive Power", xlab = "")
  }, mfrow = c(2, 2))
  
  
}

data <- generatePlots()