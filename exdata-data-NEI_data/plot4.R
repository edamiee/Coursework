##Damien Edwards
## Course Project 2 plot 4


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Subset and aggregate data
combustion <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion <- (combustion & coal)
SCC_Combustion <- SCC[coalCombustion,]$SCC
NEI_sub <- NEI[NEI$SCC %in% SCC_Combustion,]
## Create the plot and print to png
library(ggplot2)
png("plot4.png")
grplot <- ggplot(NEI_sub,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Coal Combustion in America from 1999-2008"))

print(grplot)
dev.off()