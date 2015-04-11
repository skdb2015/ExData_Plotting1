## Plot2.R
## Generate plot of Global active power vs. date-time
library(lubridate)

CreatePlot2 <- function()
{
    ##
    ##  just read the data for Feb 1 2007 and Feb 2 2007
    ##
    filePath = "data/household_power_consumption.txt"    
    powerData = subset(read.csv(filePath, sep = ";",), 
                       Date == "1/2/2007" | Date == "2/2/2007")
    
    ##
    ##  Plot 2 is a plot of global active power vs. day of week.
    ##
    
    ##
    ## first, add a column to indicate timestamp (date + time)
    ##
    timeStamp = dmy_hms(paste(as.character(powerData$Date),
                              as.character(powerData$Time),
                              sep="_"))
    powerDataWithTimeStamp = cbind(powerData, timeStamp)
    
    ##
    ## convert global power in the data frame to numeric
    ##
    powerDataWithTimeStamp$Global_active_power = 
        as.numeric(as.character(powerDataWithTimeStamp$Global_active_power))
    
    ##
    ##  Plot to png
    ##
    png("figure/plot2.png")
    
    with(powerDataWithTimeStamp,{
        plot(timeStamp,Global_active_power, type = "n",
             xlab = "",
             ylab = "Global Active Power (kilowatts)")
        lines(timeStamp, Global_active_power)
    })
    
    dev.off()
}