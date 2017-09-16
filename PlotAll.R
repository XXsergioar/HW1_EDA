## HW1_ExploratoryDataAnalysi
## Dataset: Electric power consumption [20Mb]
## url: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
##Description: Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years. Different electrical quantities and some sub-metering values are available.
## 9 Variables in data set:
## Date: Date in format dd/mm/yyyy 
## Time: time in format hh:mm:ss 
## Global_active_power: household global minute-averaged active power (in kilowatt) 
## Global_reactive_power: household global minute-averaged reactive power (in kilowatt) 
## Voltage: minute-averaged voltage (in volt) 
## Global_intensity: household global minute-averaged current intensity (in ampere) 
## Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered). 
## Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light. 
## Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.
## NOTES:
## We will only be using data from the dates 2007-02-01 and 2007-02-02. 
## One alternative: read the data for just those dates rather than the entire dataset and subsetting.
## useful to convert the Date and Time variables to Date/Time classes in R using the strptime() and as.Date() functions.
## Note that in this dataset missing values are coded as ?.
##      TASK: reconstruct the plots below, using the base plotting system.
## First: fork and clone the GitHub repository: https://github.com/rdpeng/ExData_Plotting1
## For each plot you should
## 1. Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
## 2. Name each of the plot files as plot1.png, plot2.png, etc.
## 3. Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. Your code file should include code for reading the data so that the plot can be fully reproduced. You should also include the code that creates the PNG file.
## 4. Add the PNG file and R code file to your git repository
## to finish: push your git repository to GitHub 
##      There should be four PNG files and four R code files.
## Plot1: Histogram of Global Active Power, in red blocks
## Plot2: GAP Vs Time
## plot3: Three Enegry submettering (in red, yellow, ornage) VS. time in the same plot, 
##        with top right box giving colors for each one 
## plot4: four plots: i)plot 1, ii) voltage VS Date time, 
## iii) plot 3; iv) Global_reactive_Power VS. DateTime 
##
## First: Read the Data:
library(data.table)
library(stringr)
library(dplyr)
library(reshape2)
url1<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
setwd("C:/Users/Sergio/courseraRprog/ExploratoryDataAnalysis")
getwd()
if(!file.exists("C:Users/Sergio/courseraRprog/ExploratoryDataAnalysis/dat1")){
  temp<-tempfile()
  download.file(url1, destfile=temp)
  datos<-unzip(temp)
}
datos1<-fread(datos, na.strings=c("?"),header= T,nrows=70000,
 data.table=T)
str(datos1)
## subset the data.table to include only 01/02/2007 up to 02/02/2007 
## and merge the first two columns into a single Date.Time:fechau
## convert fechau back to Date class
datos1$Date <- as.Date(datos1$Date, format="%d/%m/%Y")
datos2<-subset(datos1, (Date =="2007-02-01") | (Date=="2007-02-02"))
datos2<-mutate(datos2, fechau=paste(as.character(Date), Time, sep=" "))
datos2$fechau<- strptime(datos2$fechau,"%Y-%m-%d %H:%M:%S")
## check the datos 2 with str(datos2)
##write the data for easy reading in each PLOTs.R
##
write.csv(datos2,"datos2.csv") 
datos2<-read.csv("datos2.csv",stringsAsFactors=FALSE)
datos2$fechau<- strptime(datos2$fechau,"%Y-%m-%d %H:%M:%S")
## In case a separate R program is required for every plot, 
## the above 2 commands mut be copied below the ##PLOT line in the code below
##PLOT #1
hist(datos2$Global_active_power, main="Global Active Power", 
      col="red", xlab="Global Active Power (kilowatts)")
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()
##
## PLOT #2
plot(datos2$fechau, datos2$Global_active_power, 
     ylab="Global Active Power (kilowatts)", type= "l")
dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()
##
##PLOT #3
plot(datos2$fechau, datos2$Sub_metering_1, type="l",
     xlab="", ylab="Energy sub metering")
lines(datos2$fechau, datos2$Sub_metering_2, col="red")
lines(datos2$fechau, datos2$Sub_metering_3, col="blue")
legend("topright", col=c("black", "red", "blue"), 
 c("Sub_metering_1","Sub_metering_2 ","Sub_metering_3 "),
 lty=c(1,1), lwd=c(2,2))
dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()
##
##Plot #4
par(mfrow=c(2,2))
with(datos2, {
  plot(fechau,Global_active_power, 
    ylab="Global Active Power (kilowatts)", xlab="",type= "l")
  plot(fechau,Voltage, 
       ylab="Voltage", xlab="datetime",type= "l")
  plot(fechau,Sub_metering_1, type="l",
       xlab="", ylab="Energy sub metering")
  lines(fechau,Sub_metering_2, col="red")
  lines(fechau,Sub_metering_3, col="blue")
  legend("topright", col=c("black", "red", "blue"), 
         c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         lty=c(1,1), lwd=c(2,2), bty="n",cex=.5)
  plot(fechau,Global_reactive_power, 
       ylab="Global_reactive_power", xlab="datetime",type="l")
})
## bty="n", eliminates the box, cex=0.5 reduce size of text
dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()

