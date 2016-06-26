## Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

# Answer: It can be observed from the barplots that the types of NON-ROAD, NON-POINT, and ON-ROAD showed 
#         decreasing total emissions, while the type of POINT showed an increasing total emission. 
#         In other words, it is good to see most of source types had a decreasing trend of total emissions.

plot3 <- function()
{
  # if the data has been loadaded in the workspace, it has no need to reload again
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  # subset data content in Maryland from the entire data frame
  emissionBalt <- subset(NEI, NEI$fips == "24510")   
  
  # aggregate the summation function on the emission with respect to both the year and type
  totalEmissionByYearAndType <- aggregate(Emissions ~ year + type, emissionBalt, sum)
  
  # use barplot to help interpret the data
  library(ggplot2)
  rhg_cols <- c("#FF0000", "#00FF00", "#0000FF", "#FF00FF")
  Year<-factor(totalEmissionByYearAndType$year)
  gp <- ggplot(data = totalEmissionByYearAndType, aes(x=factor(year), y=Emissions, fill = Year)) + 
    geom_bar(position ='dodge', stat ='identity') + 
    scale_fill_manual(values = rhg_cols) + 
    facet_grid(.~ type) + xlab("Year") + ylab("Total Emissions") +theme_bw()
  print(gp)
  ggsave("plot3.png")
}