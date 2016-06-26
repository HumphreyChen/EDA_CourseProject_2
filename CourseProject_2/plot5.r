## Question 5
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# Answer: The emissions from motor vehicle sources decrease from 1999-2008 stably.


plot5 <- function()
{
  # if the data has been loadaded in the workspace, it has no need to reload again
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  # subset data frame in Maryland with type of ON-ROAD (from roughly looking the ON-ROAD, it shows the vehicle source)
  emissionVehicleBalt <- subset(NEI, fips == "24510" & type == "ON-ROAD")  
  
  # aggregate function of sum in year
  yearVehicleSum <- aggregate(emissionVehicleBalt[,4], list(emissionVehicleBalt[,]$year), sum)            
  
  # place column names
  cnames <- c("year", "sumEmissions")
  colnames(yearVehicleSum) <- cnames
  
  # use point_line plot to help interpret the data
  library(ggplot2)
  ggp <- ggplot(yearVehicleSum, aes(factor(year), sumEmissions, group = 1)) + geom_point(size = 3) + geom_line() + 
  labs(x = "Year", y = "Total Emissions", title = "Emission Year Trend In Baltimore From Vehicles")
  print(ggp)
  ggsave("plot5.png")
  
}