## Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

# Answer: 

plot3 <- function()
{
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  emissionBalt <- subset(NEI, NEI$fips == "24510")                             # subset data frame in Maryland
  totalEmissionByYearAndType <- aggregate(Emissions ~ year + type, emissionBalt, sum)
  
  library(ggplot2)
  rhg_cols <- c("#FF0000", "#00FF00", "#0000FF", "#FF00FF")
  Year<-factor(totalEmissionByYearAndType$year)
  gp <- ggplot(data=totalEmissionByYearAndType, aes(x=factor(year), y=Emissions, fill = Year)) + geom_bar(position='dodge', stat='identity') + 
    scale_fill_manual(values = rhg_cols) + facet_grid(.~ type) + xlab("Year") + ylab("Total Emissions")
  print(gp)
  ggsave("plot3.png")
}