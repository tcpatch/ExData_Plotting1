load_power_data <- function() {
  library(lubridate)
  power_consumption <- "household_power_consumption.txt" 
  #download data if hasn't been downloaded yet
  if (!file.exists(power_consumption)){
    dataset <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    datafile <- "dataset.zip"
    download.file(dataset, destfile=datafile, method="auto")
    unzip(datafile)
  }
  #read the data
  df <- read.csv(power_consumption, sep=';', header=TRUE, na.strings="?")
  #set type of each column
  df$Date <- dmy(df$Date)
  df$Time <- hms(df$Time)
  df$Global_active_power <- as.numeric(df$Global_active_power)
  df$Global_reactive_power <- as.numeric(df$Global_reactive_power)
  df$Voltage <- as.numeric(df$Voltage)
  df$Global_intensity <- as.numeric(df$Global_intensity)
  df$Sub_metering_1 <- as.numeric(df$Sub_metering_1)
  df$Sub_metering_2 <- as.numeric(df$Sub_metering_2)
  df$Sub_metering_3 <- as.numeric(df$Sub_metering_3)
  #subset the two days of interest
  doi1 <- df$Date == '2007-02-01'
  doi2 <- df$Date == '2007-02-02'
  df1 <- df[doi1,]
  df2 <- df[doi2,]
  #combind the two days of interest and return that dataframe
  df_doi <- rbind(df1, df2)
  df_doi
}

plot1 <- function() {
  #load the dataset using the helper function load_power_data()
  df <- load_power_data()
  #plot data and save to .png file
  hist(df$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")
  dev.copy(png, file="plot1.png", height=480, width=480)
  dev.off()
}