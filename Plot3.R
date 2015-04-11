## Plot3.R
## Generate plots of Energy Sub metering vs. date-time
##
library(lubridate)

CreatePlot3 <- function()
{
    ##
    ##  just read the data for Feb 1 2007 and Feb 2 2007
    ##
    filePath = "data/household_power_consumption.txt"    
    powerData = subset(read.csv(filePath, sep = ";",), 
                       Date == "1/2/2007" | Date == "2/2/2007")
    
    Sys.setlocale("LC_ALL","C")
    
    ##
    ## first, add a column to indicate timestamp (date + time)
    ##
    timeStamp = dmy_hms(paste(as.character(powerData$Date),
                              as.character(powerData$Time),
                              sep="_"))
    powerDataWithTimeStamp = cbind(powerData, timeStamp)
    
    ##
    ##  Convert the submetering data to numeric
    ##
    powerDataWithTimeStamp$Sub_metering_1 = 
        as.numeric(as.character(powerDataWithTimeStamp$Sub_metering_1))
    
    powerDataWithTimeStamp$Sub_metering_2 = 
        as.numeric(as.character(powerDataWithTimeStamp$Sub_metering_2))
    
    powerDataWithTimeStamp$Sub_metering_3 = 
        as.numeric(as.character(powerDataWithTimeStamp$Sub_metering_3))
    
    ##
    ##  Plot to png
    ##
    png("figure/plot3.png")
    
    with(powerDataWithTimeStamp,{
        plot(timeStamp,
             Sub_metering_1, 
             type = "n",
             xlab = "",
             ylab = "Energy sub metering")

        lines(timeStamp, Sub_metering_1)
        lines(timeStamp, Sub_metering_2, col="red")
        lines(timeStamp, Sub_metering_3, col= "blue")
        legend("topright", lty=1, 
               col=c("black","red","blue"), 
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    })
    
    dev.off()    
}