## Question 4
# Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

# Answer: It decreases per year

plot4 <- function()
{
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  withCoalSCC <- grep("Coal+", SCC$EI.Sector, perl=TRUE, value=FALSE)
  withCoalNEI <- which(NEI$SCC %in% SCC$SCC[withCoalSCC])

  yearCoalSum <- aggregate(NEI[withCoalNEI,4], list(NEI[withCoalNEI,]$year), sum)                          # aggregate function of sum in year
  cnames <- c("year", "sumEmissions")
  colnames(yearCoalSum) <- cnames
  
  library(ggplot2)
  ggp <- ggplot(yearCoalSum, aes(factor(year), sumEmissions, group = 1)) + geom_point() + geom_line() + labs(title = "Emission year Trend in Baltimore in various types")
  print(ggp);
  ggsave("plot4.png")
}