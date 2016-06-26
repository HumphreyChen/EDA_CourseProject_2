## Question 5
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# Answer: 

plot5 <- function()
{
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  emissionVehicleBalt <- subset(NEI, fips == "24510" & type == "ON-ROAD")  # subset data frame in Marylan
  yearVehicleSum <- aggregate(emissionVehicleBalt[,4], list(emissionVehicleBalt[,]$year), sum)            # aggregate function of sum in year
  cnames <- c("year", "sumEmissions")
  colnames(yearVehicleSum) <- cnames
  
  library(ggplot2)
  ggp <- ggplot(yearVehicleSum, aes(factor(year), sumEmissions, group = 1)) + geom_point() + geom_line() + labs(title = "Emission year Trend in Baltimore in various types")
  print(ggp);
  ggsave("plot5.png")
  
}