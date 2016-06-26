## Question 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# Answer: Yes

plot1 <- function()
{
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")

  yearSum <- aggregate(NEI[, 4], list(NEI$year), sum)                          # aggregate function of sum in year
  cnames <- c("year", "sumEmissions")
  colnames(yearSum) <- cnames
  png("plot1.png", res=72)
  plot(yearSum$year, yearSum$sumEmissions, xlab = "Year", ylab = "Totl Emissions in US", type="o", pch = 22, col="blue", main="Question #1 Plot")
  # title(main="Question #1 Plot", col.main="black", font.main=4)
  dev.off()
}