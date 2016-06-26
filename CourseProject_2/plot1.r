## Question 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# Answer: Yes. Based on the data analysis, it showed the values of total emission decreased from adjacent years in 1999, 2002, 2005 and 2008. 

plot1 <- function()
{
  # if the data has been loadaded in the workspace, it has no need to reload again
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")

  # aggregate the summation function on the emission with respect to the year
  yearSum <- aggregate(NEI[, 4], list(NEI$year), sum)       
  
  # place column names
  cnames <- c("year", "sumEmissions")
  colnames(yearSum) <- cnames
  
  # output an image
  png("plot1.png", 512, 512)
  
  # set graph parameters (mar: margiin; cex.axis: axis font size; cex.main: main font size)
  par(mar=c(4,4,2,1), cex.axis=1.0, cex.main=1.15)
  
  # plot total emission with respect to years
  plot(yearSum$year, yearSum$sumEmissions, xlab = "Year", ylab = "Total Emissions", type="o", pch = 19, cex = 2.0, main="Total Emissions From PM2.5 In US From 1999 To 2008.")
  
  # turn off graphical device
  dev.off()
}