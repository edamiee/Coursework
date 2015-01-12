## Damien Edwards
## Course Project 1 plot 1


## Read data frame and subset the data frame based on dates
df <-read.table('household_power_consumption.txt',header=TRUE, sep=';',na.strings = "?")
df.sub1 <- subset(df,as.Date(df$Date,format="%d/%m/%Y") >="2007-02-01"& as.Date(df$Date,format="%d/%m/%Y") <="2007-02-02" )

## Histogram function and print
png("plot1.png", width=480, height=480)
par(mar=c(4.7, 4.7, 0.7, 0.7))
hist(df.sub1$Global_active_power,col = "red", main="Global Active Power",xlab="Global Active Power (kilowatts)",ylab="Frequency", breaks=12) 
dev.off()