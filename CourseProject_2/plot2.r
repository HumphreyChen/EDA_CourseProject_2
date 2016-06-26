## Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Answer: It decreases in an overall manner, which can be described by the linear regression with a negative slope. However, the data plotting also showed an increasing 
#         locally from from 2002 to 2005. Note that, this analysis should be applied to longer historical data in order to get a more representative result.

plot2 <- function()
{
  # if the data has been loadaded in the workspace, it has no need to reload again
  if (exists("NEI") != TRUE)
    NEI <- readRDS("../data/summarySCC_PM25.rds")
  if (exists("SCC") != TRUE)
    SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  # subset data content in Maryland from the entire data frame
  emissionBalt <- subset(NEI, NEI$fips == "24510")                             
  
  # aggregate the summation function on the emission with respect to the year
  yearSumBalt <- aggregate(emissionBalt[,4], list(emissionBalt$year), sum)    
  
  # place column names
  cnames <- c("year", "sumEmissions")
  colnames(yearSumBalt) <- cnames
  
  # output an image
  png("plot2.png", 512, 512)
  
  # set graph parameters (mar: margiin; cex.axis: axis font size; cex.main: main font size)
  par(mar=c(4,4,2,1), cex.axis=1.0, cex.main=1.15)
  
  # plot total emission in Maryland with respect to years
  plot(yearSumBalt$year, yearSumBalt$sumEmissions, xlab = "Year", ylab = "Total Emissions", type="o", pch = 19, cex = 2.0, main="Total Emissions From PM2.5 In Maryland From 1999 To 2008.")
  
  # linear fitting the data
  fit <- lm(yearSumBalt$sumEmissions ~ yearSumBalt$year)
  
  # draw the linear regression line
  abline(coef(fit)[1:2], lty=2, col="blue")
  
  # round the line coefficients
  cf <- round(coef(fit), 2)
  
  # prepare the line equation
  eq <- paste0("totalEmission = ", cf[1],
               ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " year ")
  
  # print the line equation
  mtext(eq, 3, line=-2, col="blue")
  
  # turn off graphical device
  dev.off()
}