## Damien Edwards
## Course Project 1 plot 3

## Read data frame and subset the data frame based on dates
df <-read.table('household_power_consumption.txt',header=TRUE, sep=';',na.strings = "?")
df.sub1 <- subset(df,as.Date(df$Date,format="%d/%m/%Y") >="2007-02-01"& as.Date(df$Date,format="%d/%m/%Y") <="2007-02-02" )
## Convert date
df.sub1$Date <- as.POSIXlt(paste(as.Date(df.sub1$Date, format="%d/%m/%Y"), df.sub1$Time, sep=" "))
## Create plot
library(datasets)
png("plot3.png", width=480, height=480)
par(mar=c(4.7, 4.7, 0.7, 0.7))
with(df.sub1,plot(df.sub1$Date,df.sub1$Sub_metering_1,type="l",xlab="",ylab="Energy sub Meeting"))
     lines(df.sub1$Date, df.sub1$Sub_metering_1, col="black")
     lines(df.sub1$Date, df.sub1$Sub_metering_2, col="red")
     lines(df.sub1$Date, df.sub1$Sub_metering_3, col="blue")
     legend("topright", lwd = 1, col = c("black", "red","blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))

dev.off()