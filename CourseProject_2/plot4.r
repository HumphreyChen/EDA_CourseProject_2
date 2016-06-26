## Question 4
# Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

# Answer: It overall decreases from 1999 to 2008, while the emissions in 2002 and 2005 show a slight fluctuation.

plot4 <- function()
{
  # if the data has been loadaded in the workspace, it has no need to reload again
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  # subset data content in Maryland with coal-related source from the entire data frame
  withCoalSCC <- grep("Coal+", SCC$EI.Sector, perl=TRUE, value=FALSE)
  withCoalNEI <- which(NEI$SCC %in% SCC$SCC[withCoalSCC])

  # aggregate the summation function on the subset emission with respect to the year
  yearCoalSum <- aggregate(NEI[withCoalNEI,4], list(NEI[withCoalNEI,]$year), sum) 
  
  # place column names
  cnames <- c("year", "sumEmissions")
  colnames(yearCoalSum) <- cnames
  
  # use point_line plot to help interpret the data
  library(ggplot2)
  ggp <- ggplot(yearCoalSum, aes(factor(year), sumEmissions, group = 1)) + geom_point(size = 3) + geom_line() + labs(x = "Year", y = "Total Emissions", title = "Emission Year Trend In Maryland In Various Types")
  print(ggp)
  ggsave("plot4.png")
}