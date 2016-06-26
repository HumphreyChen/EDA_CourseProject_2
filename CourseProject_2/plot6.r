## Question 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

# Answer: 

plot6 <- function()
{
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  emissionVehicleBalt <- subset(NEI, NEI$fips == "24510" & type == "ON-ROAD")
  yearVehicleSumBalt <- aggregate(emissionVehicleBalt[,4], list(emissionVehicleBalt[,]$year), sum)            # aggregate function of sum in year
  emissionVehicleLAC <- subset(NEI, NEI$fips == "06037" & type == "ON-ROAD")
  yearVehicleSumLAC <- aggregate(emissionVehicleLAC[,4], list(emissionVehicleLAC[,]$year), sum)            # aggregate function of sum in year
  
  yearVehicleSumBalt$city <- c("BL","BL","BL","BL")
  yearVehicleSumLAC$city <- c("LA","LA","LA","LA")
  
  cnames <- c("year", "sumEmissions", "city")
  colnames(yearVehicleSumBalt) <- cnames
  colnames(yearVehicleSumLAC) <- cnames
  
  mergedData <- rbind(yearVehicleSumBalt, yearVehicleSumLAC)
  
  library(ggplot2)
  
  gp <- ggplot(data=mergedData, aes(x=year, y=sumEmissions, colour = city)) +
    geom_line() +
    geom_point( size=4, shape=21, fill="white")
  
  print(gp)
  ggsave("plot6.png")
}