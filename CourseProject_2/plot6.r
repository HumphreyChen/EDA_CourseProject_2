## Question 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

# Answer: The emission trends from different areas are placed in the same plot for facilitating comparison. 
#         It indicates that the emissions in Baltimore city decrease with years, while that in LA county is increasing globally.
#         In addition, the mean values (dashed lines) from the two areas show a big difference, indicating the area could be a significant factor on air pollution evaluation

plot6 <- function()
{
  # if the data has been loadaded in the workspace, it has no need to reload again
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  # subset data content of ON-ROAD source in Baltimore from the entire data frame
  emissionVehicleBalt <- subset(NEI, NEI$fips == "24510" & type == "ON-ROAD")
  
  # aggregate the summation function on the emission with respect to the year
  yearVehicleSumBalt <- aggregate(emissionVehicleBalt[,4], list(emissionVehicleBalt[,]$year), sum)           
  
  # subset data content of ON-ROAD source in Los Angeles County from the entire data frame
  emissionVehicleLAC <- subset(NEI, NEI$fips == "06037" & type == "ON-ROAD")
  
  # aggregate the summation function on the emission with respect to the year
  yearVehicleSumLAC <- aggregate(emissionVehicleLAC[,4], list(emissionVehicleLAC[,]$year), sum)           
  
  # place area variables into the data frames
  yearVehicleSumBalt$area <- c("BL","BL","BL","BL") # Baltimore city
  yearVehicleSumLAC$area <- c("LA","LA","LA","LA")  # Los Angeles County
  
  # place column names
  cnames <- c("year", "sumEmissions", "area")
  colnames(yearVehicleSumBalt) <- cnames
  colnames(yearVehicleSumLAC) <- cnames
  
  # bind the two data frames
  mergedData <- rbind(yearVehicleSumBalt, yearVehicleSumLAC)
  
  library(plyr)
  m_sd <- aggregate(mergedData$sumEmissions, by=list(mergedData$area), mean)
  
  # use point_line plot to help interpret the data
  library(ggplot2)
  gp <- ggplot(data=mergedData, aes(x=year, y=sumEmissions, shape = area, colour = area)) +
    geom_line() + geom_point(size=3) + 
    geom_hline(aes(yintercept = x, colour = Group.1), m_sd, linetype = 2) + 
    labs(x = "Year", y = "Total Emissions", title = "Comparison of Emission Year Trend In Baltimore City and Los Angeles County From Vehicles")
  print(gp)
  ggsave("plot6.png")
}