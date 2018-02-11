load_power_data <- function() {
  power_consumption <- "household_power_consumption.txt" 
  #download data if hasn't been downloaded yet
  if (!file.exists(power_consumption)){
    dataset <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    datafile <- "dataset.zip"
    download.file(dataset, destfile=datafile, method="auto")
    unzip(datafile)
  }
  #read in the data into a dataframe
  df1 <- read.table("household_power_consumption.txt", sep=";", header=TRUE, na.strings="?")
  #select the days of interest and combine the dates/times into one column
  doi1 <- df1$Date == '1/2/2007'
  doi2 <- df1$Date == '2/2/2007'
  df2 <- df1[doi1,]
  df3 <- df1[doi2,]
  df <- rbind(df2, df3)
  df$Date <- as.Date(df$Date, format="%d/%m/%Y")
  dateTime <- paste(df$Date, df$Time)
  df$DateTime <- as.POSIXct(dateTime)
  #return the new dataframe with new column DateTime
  df
}

plot3 <- function() {
  #load the dataset using the helper function load_power_data()
  df <- load_power_data()
  #plot data and save to .png file
  with(df, {
    plot(Sub_metering_1~DateTime, type="l",
         ylab="Energy sub metering", xlab="")
    lines(Sub_metering_2~DateTime, col='Red')
    lines(Sub_metering_3~DateTime, col='Blue')
  })
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=1, 
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.copy(png, file="plot3.png", height=480, width=480)
  dev.off()
}