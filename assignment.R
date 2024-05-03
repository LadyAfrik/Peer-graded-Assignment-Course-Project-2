#Reading the data

NEI <- readRDS("../exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("../exdata_data_NEI_data/Source_Classification_Code.rds")

#QUESTION ONE

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


total_year <- tapply(NEI$Emissions, NEI$year, sum, na.rm=TRUE)

# Open PNG device and specify file path
png("plot1.png", width=640, height=640)

# Set plot layout and margins
par(mfrow=c(1,1), mar=c(5,5,4,2))

# Create bar plot
barplot(total_year/1000, names.arg = names(total_year), col="dodgerblue2", main="Emissions of PM2.5 by year", 
        xlab = "Year", ylab="Amount of emissions (kilotons)", ylim = range(0,total_year/1000) * 1.1)


# Add line plot
lines(names(total_year), total_year/1000, lwd=2, col="blue")

# Close the PNG device
dev.off()


#QUESTION 2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.

NEI_Baltimore<-subset(NEI, fips == "24510")
Baltimore_year<-tapply(NEI_Baltimore$Emissions, NEI_Baltimore$year, sum, na.rm=TRUE)
png("plot2.png", width=640, height=640)
par(mfrow=c(1,1),mar=c(5,5,4,2))
barplot(Baltimore_year, names.arg = names(Baltimore_year), col="dodgerblue3", 
        main="Emissions of PM2.5 by year in Baltimore City", xlab = "Year", 
        ylab="Amount of emissions (tons)", ylim = range(0,Baltimore_year)*1.1) 
dev.off()


#QUESTION 3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

install.packages("reshape2")

library(ggplot2)
library(reshape2)

Bmelt<-melt(NEI_Baltimore,id=c("year","type"), measure.vars = "Emissions")
Bcast<-dcast(Bmelt,year~type,sum)
Baltimore_type<-melt(Bcast,id=c("year"),measure.vars=names(Bcast)[-c(1)])

png("plot3.png", width=640, height=480)
gt<-ggplot(Baltimore_type, aes(x=year,y=value)) + geom_col(width = 1,mapping=aes(fill=variable)) + 
  facet_grid(.~variable) + theme_bw() + geom_smooth(method = "lm") +
  labs(title="Emissions of PM2.5 in Baltimore City by type") +
  xlab("Year") + ylab("Amount of PM2.5 emissions (tons)")+
  theme(text= element_text(size = 10), plot.title = element_text(size=13, hjust = 0.5, face="bold"), 
        plot.margin = margin(1, 1, 1, 1, "cm"))
gt 
dev.off()


#QUESTION 4
#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

gl<-grepl("(.*)(Comb)(.*)(Coal)(.*)",SCC$Short.Name)
subscc<-SCC[gl,]
NEI_Coal<-subset(NEI, SCC %in% subscc$SCC)
NEI_Coal_year<-tapply(NEI_Coal$Emissions, NEI_Coal$year, sum, na.rm=TRUE)

png("plot4.png", width=640, height=640)
par(mfrow=c(1,1),mar=c(5,5,4,2))
barplot(NEI_Coal_year, names.arg = names(NEI_Coal_year), col="darkorchid3", 
        main="Emissions of PM2.5 from coal combustion-related sources by year", 
        xlab = "Year", ylab="Amount of emissions (tons)", ylim = range(0,NEI_Coal_year)*1.1) 
dev.off()  

#QUESTION 5
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

gl<-grepl("(.*)(Highway Veh)(.*)",SCC$Short.Name)
SCC_MotorVeh<-SCC[gl,]
NEI_Baltimore_MotorVeh<-subset(NEI_Baltimore, SCC %in% SCC_MotorVeh$SCC)
NEI_Baltimore_MotorVeh_year<-tapply(NEI_Baltimore_MotorVeh$Emissions, 
                                    NEI_Baltimore_MotorVeh$year, sum, na.rm=TRUE)

png("plot5.png", width=640, height=480)
par(mfrow=c(1,1),mar=c(5,5,4,2))
barplot(NEI_Baltimore_MotorVeh_year, names.arg = names(NEI_Baltimore_MotorVeh_year), col="darkorchid2", 
        main="Emissions of PM2.5 from motor vehicles by year in Baltimore City", 
        xlab = "Year", ylab="Amount of emissions (tons)", 
        ylim = range(0,NEI_Baltimore_MotorVeh_year)*1.1) 
dev.off()

#QUESTION 6  
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (  fips == "06037"
 #   fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

NEI_LA <-subset(NEI, fips=="06037")
NEI_LA_MotorVeh<-subset(NEI_LA,SCC %in% SCC_MotorVeh$SCC)
NEI_LA_MotorVeh_year<-tapply(NEI_LA_MotorVeh$Emissions,
                             NEI_LA_MotorVeh$year, sum, na.rm=TRUE)
Balt_LA<-data.frame(year=as.numeric(names(NEI_LA_MotorVeh_year)),
                    Baltimore.Emissions = NEI_Baltimore_MotorVeh_year,
                    LA.Emissions = NEI_LA_MotorVeh_year)

rng<-c(0,max(Balt_LA$Baltimore.Emissions,Balt_LA$LA.Emissions)+1000)
png("plot6.png", width=480, height=720)
par(mfrow=c(1,1),mar=c(5,5,4,2))
plot(Balt_LA$year, Balt_LA$LA.Emissions, type="l", lwd=2, pch=19,  
     col="darkorchid3", main="Emissions of PM2.5 from motor vehicles by year", 
     xlab = "Year", ylab="Amount of emissions (tons)", ylim=rng)
lines(Balt_LA$year,Balt_LA$Baltimore.Emissions, lwd=2, col="darkorchid3")
points(Balt_LA$year,Balt_LA$Baltimore.Emissions, lwd=2, pch=19, col="darkorchid3")
points(Balt_LA$year,Balt_LA$LA.Emissions, lwd=2, pch=19, col="chartreuse3")
lines(Balt_LA$year,Balt_LA$LA.Emissions, lwd=2, col="chartreuse3")
legend("topright", lwd=2, col=c("darkorchid3", "chartreuse3"), 
       legend=c("Baltimore City","Los Angeles County"), bty="n") 
dev.off()