##Damien Edwards
## Course Project 2 plot 2


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Subset and aggregate data
NEI_sub <- NEI[NEI$fips=="24510",]
Emission_agg <- aggregate(Emissions ~ year,NEI_sub, sum)

library(datasets)
png("plo2.png", width=480, height=480)
par(mar=c(4.7, 4.7, 0.7, 0.7))
barplot(
  (Emission_agg$Emissions)/10^6,
  names.arg=Emission_agg$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons",
  main="Total PM2.5 Emissions for Baltimore City, Maryland",
  col="green")
dev.off()