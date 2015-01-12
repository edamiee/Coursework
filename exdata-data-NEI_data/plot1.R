##Damien Edwards
## Course Project 2 plot 1


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

Emission_agg <- aggregate(Emissions ~ year,NEI, sum)
##Emissions$PM <- round(Emissions[, 2] / 1000, 2)
library(datasets)
png("plo1.png")
par(mar=c(4.7, 4.7, 0.7, 0.7))
barplot(
  (Emission_agg$Emissions)/10^6,
  names.arg=Emission_agg$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons",
  main="Total PM2.5 Emissions",
  col="red")
dev.off()