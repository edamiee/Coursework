##Damien Edwards
## Course Project 2 plot 3


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Subset and aggregate data
NEI_sub <- NEI[NEI$fips=="24510",]
Emission_agg <- aggregate(Emissions ~ year,NEI_sub, sum)

library(ggplot2)
png("plo3.png")
grplot <- ggplot(NEI_sub,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions for Baltimore City,Maryland 1999-2008 Type"))

print(grplot)
dev.off()