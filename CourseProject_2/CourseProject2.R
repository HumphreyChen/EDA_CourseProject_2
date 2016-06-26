## This first line will likely take a few seconds. Be patient!
if (exists("NEI") != TRUE)
{
  print("hello")
  NEI <- readRDS("../data/summarySCC_PM25.rds")
  SCC <- readRDS("../data/Source_Classification_Code.rds")
}

## Question 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# Answer: Yes
yearSum <- aggregate(NEI[, 4], list(NEI$year), sum)                          # aggregate function of sum in year
cnames <- c("year", "sumEmissions")
colnames(yearSum) <- cnames
png("plot1.png", 480, 480)
plot(yearSum$year, yearSum$sumEmissions, xlab = "Year", ylab = "Totl Emissions in US", type="o", pch = 22, col="blue")
title(main="Question #1 Plot", col.main="black", font.main=4)
dev.off()

## Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Answer: No, it increases from 2002 to 2005
emissionBalt <- subset(NEI, NEI$fips == "24510")                             # subset data frame in Maryland
yearSumBalt <- aggregate(emissionBalt[,4], list(emissionBalt$year), sum)     # aggregate function of sum in year
cnames <- c("year", "sumEmissions")
colnames(yearSumBalt) <- cnames
png("plot2.png", 480, 480)
plot(yearSumBalt$year, yearSumBalt$sumEmissions, xlab = "Year", ylab = "Totl Emissions in Baltimore", type="o", pch = 22, col="blue")
title(main="Question #2 Plot", col.main="black", font.main=4)
dev.off()

## Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
# a plot answer this question.
emissionPointBalt <- subset(emissionBalt, emissionBalt$type == "POINT")
yearSumPointBalt <- aggregate(emissionPointBalt[,4], list(emissionPointBalt$year), sum)
emissionNonpointBalt <- subset(emissionBalt, emissionBalt$type == "NONPOINT")
yearSumNonpointBalt <- aggregate(emissionNonpointBalt[,4], list(emissionNonpointBalt$year), sum)
emissionOnroadBalt <- subset(emissionBalt, emissionBalt$type == "ON-ROAD")
yearSumOnroadBalt <- aggregate(emissionOnroadBalt[,4], list(emissionOnroadBalt$year), sum)
emissionNonroadBalt <- subset(emissionBalt, emissionBalt$type == "NON-ROAD")
yearSumNonroadBalt <- aggregate(emissionNonroadBalt[,4], list(emissionNonroadBalt$year), sum)

cnames <- c("year", "sumEmissions")
colnames(yearSumPointBalt) <- cnames
colnames(yearSumNonpointBalt) <- cnames
colnames(yearSumOnroadBalt) <- cnames
colnames(yearSumNonroadBalt) <- cnames

rng <- range(yearSumPointBalt$sumEmissions, yearSumNonpointBalt$sumEmission, 
             yearSumOnroadBalt$sumEmission, yearSumNonroadBalt$sumEmission)

png("plot3.png", 480, 480)
plot(yearSumPointBalt$year, yearSumPointBalt$sumEmissions, ylim = rng, xlab = "Year", ylab = "Totl Emissions in Baltimore", type="o", lwd=2)
lines(yearSumPointBalt$year, yearSumPointBalt$sumEmission, col = "red", pch = 15)
lines(yearSumNonpointBalt$year, yearSumNonpointBalt$sumEmission, col = "green", pch = 16)
lines(yearSumOnroadBalt$year, yearSumOnroadBalt$sumEmission, col = "blue", pch = 17)
lines(yearSumNonroadBalt$year, yearSumNonroadBalt$sumEmission, col = "yellow", pch = 18)
legend("topright", c("Point", "Nonpoint", "Onroad", "Nonroad"), col=c("red", "green", "blue", "yellow"), lty=1, lwd=1)
title(main="Question #3 Plot", col.main="black", font.main=4)
dev.off()

