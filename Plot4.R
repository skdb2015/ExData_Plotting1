## Plot4.R
## Generate 4 plots in a single screen:
##  1. Global Active power vs. date-time
##  2. Voltage vs. date-time
##  3. Energy sub metering vs. date-time
##  4. Global reactive power vs. date-time
##
library(lubridate)

CreatePlot4 <- function()
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
    ##  Now, generate the 4 plots. Each of them will be in a function call
    ##
    
    ##
    ##  set the graphics device and the surface
    ##
    
    png("figure/plot4.png")
    
    ##
    ##  Set the environment and then invoke the functions to generate the
    ##  graphs
    ##
    par(mfrow=c(2,2), mar = c(4,4,2,2), cex=0.75)
    plot_PowerData_vs_Time(powerDataWithTimeStamp, 
                           "Global_active_power", 
                           "Global Active Power",
                           "")
    
    plot_PowerData_vs_Time(powerDataWithTimeStamp,
                           "Voltage",
                           "Voltage",
                           "datetime")
    
    plot_PowerData_vs_Time(powerDataWithTimeStamp,
                           c("Sub_metering_1",
                             "Sub_metering_2",
                             "Sub_metering_3"),
                           "Energy sub metering",
                           "",
                           c("black","red","blue"),
                           addLegend = TRUE)
    
    plot_PowerData_vs_Time(powerDataWithTimeStamp,
                           "Global_reactive_power",
                           "Global_reactive_power",
                           "datetime")
    
    dev.off()
    
}

##
##  the multiple plots in this script have some common characteristics and 
##  differ only slightly in terms of parameters. So, this one function is 
##  built to execute those common characteristics, which are:
##  1. date time on x-axis
##  2. line graphs
##  3. y axis is a numeric measurement
##  The only things that defer is the variable that will be plotted on the
##  y-axis, the name of the variable and the colors. 
##  
##  It is built to handle multiple variables on separate lines as well
##
plot_PowerData_vs_Time <- function(powerDataWithTimeStamp, 
                                   yVariables, 
                                   yLabel,
                                   xLabel,
                                   yColors = "black",
                                   addLegend = FALSE)
{
    ##
    ##  Convert the yVariables columns to numeric in the dataframe
    ##
    for (y in yVariables)
    {
        powerDataWithTimeStamp[,y] = 
            as.numeric(as.character(powerDataWithTimeStamp[,y]))
    }
    
    ##
    ##  Plot the data. It is assumed that the caller has set the drawing device
    ##  as well as layout
    ##
    plot(powerDataWithTimeStamp$timeStamp, 
         powerDataWithTimeStamp[,yVariables[1]], type = "n",
         xlab = xLabel,
         ylab = yLabel)
    
    idx = 1
    
    ##
    ##  iterate through the various y axis variables and draw a separate line
    ##  for each of those variables
    ##
    for (y in yVariables)
    {
        lines(powerDataWithTimeStamp$timeStamp, powerDataWithTimeStamp[,y], col=yColors[idx])
        idx = idx+1
    }
    
    if (addLegend)
    {
        legend("topright", lty=1, 
               col=yColors, 
               legend=yVariables)        
    }
}