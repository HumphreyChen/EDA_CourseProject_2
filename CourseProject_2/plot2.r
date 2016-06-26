## Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Answer: No, it increases from 2002 to 2005

plot2 <- function()
{
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  emissionBalt <- subset(NEI, NEI$fips == "24510")                             # subset data frame in Maryland
  yearSumBalt <- aggregate(emissionBalt[,4], list(emissionBalt$year), sum)     # aggregate function of sum in year
  cnames <- c("year", "sumEmissions")
  colnames(yearSumBalt) <- cnames
  png("plot2.png", res=72)
  plot(yearSumBalt$year, yearSumBalt$sumEmissions, xlab = "Year", ylab = "Totl Emissions in Baltimore", type="o", pch = 22, col="blue", main="Question #2 Plot")
  # title(main="Question #2 Plot", col.main="black", font.main=4)
  dev.off()
}