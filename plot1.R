# close any open files
rm(list = ls())

# set dir, download and unzip
if (!file.exists("./data")) { dir.create("data")} 
setwd("C:/Users/testsubject941/Documents/GitHub/ExData_Plotting1/data")
# http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileURL, "exdata-data-household_power_consumption.zip")
unzip(zipfile = "exdata-data-household_power_consumption.zip")
dir()

# take a look at content
con <- file("household_power_consumption.txt")
readLines(con, n = 2)
# [1] "Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3"
# [2] "16/12/2006;17:24:00;4.216;0.418;234.840;18.400;0.000;1.000;17.000"                                                        

# read in full file
energy <- read.csv(file = con, sep = ";", header = TRUE)
nrow(energy) # 2075259

# convert to dates & time & check
energy$sdate <- as.Date(energy$Date, "%d/%m/%Y")
energy$stime <- strptime(paste(energy$Date, energy$Time), "%d/%m/%Y %H:%M:%S") 
head(energy, n = 1)

# subset for what you want in YYYY-MM-DD
# 2007-02-01 and 2007-02-02
proj1 <- energy[energy$sdate == '2007-02-01' |energy$sdate == '2007-02-02', ]
nrow(proj1) # 2880 rows

# save files and check they are there
write.csv(proj1, file = "proj1.csv")
write.csv(energy, file = "energy.csv")
dir()

# close out big file and connection 
rm(energy)

# convert to numeric fields
for (i in c(4:10)) {
        proj1[,i] <- suppressWarnings(as.numeric(proj1[,i]))  
}   

# idiot checks
str(proj1)
summary(proj1$sdate)
sapply(split(proj1$Global_active_power, proj1$sdate),sum)

# plot1
setwd("C:/Users/testsubject941/Documents/GitHub/ExData_Plotting1")
png(file = "plot1.png",  width = 480, height = 480)
hist((proj1$Global_active_power)
     , main = "Global Active Power"
     , col= "red"
     , xlab = "Global Active Power (kilowatts)"
     , xaxt = "n" 
)
axis(side=1, at=seq(0,6, by = 2), labels= seq(0,6, by = 2)) 
dev.off() 

# check to see if it is there
dir() 