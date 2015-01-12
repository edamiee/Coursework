##Damien Edwards
## Course Project 2 plot 5


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
## Create subset of Baltimore vehicle data
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
SCC_vehicle <- SCC[vehicles,]$SCC
vehicle_sub <- NEI[NEI$SCC %in% SCC_vehicle,]
NEI_sub<- vehicle_sub[vehicle_sub$fips=="24510",]
## Create plot and print to png
library(ggplot2)
png("plot5.png")
grplot <- ggplot(NEI_sub,aes(factor(year),Emissions)) +
  geom_bar(stat="identity", fill="green", width=0.75) +
  theme_bw() + guides(fill=FALSE)+
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions for Baltimore City,Maryland 1999-2008 Vehicle Source"))

print(grplot)
dev.off()