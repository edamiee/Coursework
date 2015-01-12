##Damien Edwards
## Course Project 2 plot 6


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
## Create subset of Baltimore vehicle data
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
SCC_vehicle <- SCC[vehicles,]$SCC
vehicle_sub <- NEI[NEI$SCC %in% SCC_vehicle,]
NEI_sub_baltimore<- vehicle_sub[vehicle_sub$fips=="24510",]
NEI_sub_baltimore$city<- "Baltimore City"
NEI_sub_LAC<- vehicle_sub[vehicle_sub$fips=="06037",]
NEI_sub_LAC$city<- "Los Angeles County"
NEI_sub<-rbind(NEI_sub_baltimore,NEI_sub_LAC)
## Create plot and print to png
library(ggplot2)
png("plot6.png")
grplot <- ggplot(NEI_sub,aes(x=factor(year),y=Emissions)) +
  geom_bar(aes(fill=year), stat="identity") +
  facet_grid(scales="free", space="free", .~city) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions for Baltimore & LA 1999-2008 Vehicle Source"))

print(grplot)
dev.off()