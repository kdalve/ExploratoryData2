#Download the files into your working directory & Read
NEI <-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

#Plot 1
#Calculate total Emissions from all sources for each year.
#Subset year
year1<-NEI[(NEI$year == "1999"), ]
year2<-NEI[(NEI$year=="2002"), ]
year3<-NEI[(NEI$year=="2005"), ]
year4<-NEI[(NEI$year=="2008"), ]

#Find total emissions from each year.
sum(year1$Emissions)
sum(year2$Emissions)
sum(year3$Emissions)
sum(year4$Emissions)

#Create data frame of the variables year and sum of the emissions.
year=c(1999,2002,2005,2008)
emiss=c(7332967,5635780,5454703,3464206)
emiss<-as.numeric(emiss)
total<-data.frame(year, emiss)

#Plot this dataframe using the Base Plotting System to explore the data.
with(total,plot(year,emiss,xaxt="n",xlab="Year", ylab="Total Emissions from all sources"))
lines(lowess(year,emiss),col="red")
axis(1,at=year)
title(main="Total Emissions in the United States")

#Save graph to png file
dev.copy(png, file = "plot1.png", width = 480, height = 480) 
dev.off()


#Plot 2
#Subset Baltimore data from the NEI dataset.
#Subset year.
baltimore<-NEI[(NEI$fips == "24510"), ]
year1<-baltimore[(baltimore$year == "1999"), ]
year2<-baltimore[(baltimore$year == "2002"), ]
year3<-baltimore[(baltimore$year == "2005"), ]
year4<-baltimore[(baltimore$year == "2008"), ]

#Calculate total emissions from each year.
sum(year1$Emissions)
sum(year2$Emissions)
sum(year3$Emissions)
sum(year4$Emissions)

#Create dataframe
year=c(1999,2002,2005,2008)
emiss=c(3274.18,2453.916,3091.354,1862.282)
emiss<-as.numeric(emiss)
total<-data.frame(year, emiss)

#Plot this dataframe using the base plotting system to explore the data
with(total,plot(year,emiss,xaxt="n",xlab="Year", ylab="Total Emissions from all sources"))
lines(lowess(year,emiss),col="red")
axis(1,at=year)
title(main="Total Emissions in Baltimore City, Maryland")

#Save graph as png file
dev.copy(png, file = "plot2.png", width = 480, height = 480) 
dev.off()

#Plot 3
#Install ggplot2
install.packages("ggplot2")
library(ggplot2)

#Using the Baltimore data subset, plot the data by source type using facets.
g<-qplot(year, Emissions, data=baltimore, facets=.~type,geom=c("point","smooth"),method="lm")
g+ggtitle("Total Emissions for Source Type in Baltimore City, Maryland") +
scale_x_continuous(breaks=c(1999,2002,2005,2008))+
geom_point(aes(color=type),size=4,alpha=1/2)+
theme(strip.background = element_blank(), strip.text = element_blank())

#Save graph as png file
dev.copy(png, file = "plot3.png", width = 480, height = 480) 
dev.off()

#Plot 4
#Subset the data for coal combustion related sources using the SCC data. 
grep("coal", SCC$EI.Sector, ignore.case=TRUE, value =TRUE)
combcoal<-grep("coal", SCC$EI.Sector, ignore.case=TRUE)
ccoaldata<-SCC[combcoal, ]
#Merge this subset with the NEI data
coaldata<-merge(NEI, ccoaldata, by="SCC")

#Plot the merged data set to explore the data
c<-qplot(year, Emissions, data=coaldata,geom=c("point","smooth"),method="lm")
c+ggtitle("Emissions from Coal Combustion Sources") +
scale_x_continuous(breaks=c(1999,2002,2005,2008))+
geom_point(aes(color=EI.Sector),size=4,alpha=1/2)+
theme(strip.background = element_blank(), strip.text = element_blank())

#Save the graph as png file
dev.copy(png, file = "plot4.png", width = 480, height = 480) 
dev.off()

#Plot 5
#Subset the data for Motor Vehicle sources (considered to be on-road mobile vehicles) using the SCC data.
grep("Mobile - On-Road", SCC$EI.Sector, ignore.case=TRUE, value =TRUE)
motveh<-grep("Mobile - On-Road", SCC$EI.Sector, ignore.case=TRUE)
motvehdata<-SCC[motveh, ]
#Merge this subset with the Baltimore dataset (a subset of the NEI data;created in plot2)
motordata<-merge(baltimore, motvehdata, by="SCC")

#Plot this merged dataset using ggplot2 to explore the data
m<-qplot(year, Emissions, data=motordata,geom=c("point","smooth"),method="lm")
m+ggtitle(expression(atop("Emissions from Motor Vehicle Sources", atop(italic("Baltimore, Maryland"), ""))))+
scale_x_continuous(breaks=c(1999,2002,2005,2008))+
geom_point(aes(color=EI.Sector),size=4,alpha=1/2)+
theme(strip.background = element_blank(), strip.text = element_blank())

#Save graph as png file
dev.copy(png, file = "plot5.png", width = 480, height = 480) 
dev.off()

#Plot 6
#Subset Los Angeles County data from NEI 
losangeles<-NEI[(NEI$fips == "06037"), ]
#Merge the LA data with the Motor Vehicle Data (motvehdata; determined in plot5)
motordata2<-merge(losangeles, motvehdata, by="SCC")
#Plot this data to explore LA motor vehicle emissions.
l<-qplot(year, Emissions, data=motordata2,geom=c("point","smooth"),method="lm")
l+ggtitle("Emissions from Motor Vehicle Sources in Los Angeles County, California") +scale_x_continuous(breaks=c(1999,2002,2005,2008))+geom_point(aes(color=EI.Sector),size=4,alpha=1/2)+theme(strip.background = element_blank(), strip.text = element_blank())

#Combine Baltimore data (motordata) and LA data (motordata2)
motorvehicles<-rbind(motordata, motordata2)

#Plot this data using ggplot2 to compare the two cities using facets
v<-qplot(year, Emissions, data=motorvehicles, facets=.~fips, geom=c("point","smooth"),method="lm")
v+ggtitle(expression(atop("Emissions from Motor Vehicle Sources", atop(italic("Los Angeles (fips=06037) & Baltimore (fips=24510)"), "")))) +
scale_x_continuous(breaks=c(1999,2002,2005,2008))+
geom_point(aes(color=fips),size=4,alpha=1/2)+
theme(strip.background = element_blank(), strip.text = element_blank())

#Save graph as png file
dev.copy(png, file = "plot6.png", width = 480, height = 480) 
dev.off()