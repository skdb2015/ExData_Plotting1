## Plot4.R
## Generate 4 plots in a single screen:
##  1. Global Active power vs. date-time
##  2. Voltage vs. date-time
##  3. Energy sub metering vs. date-time
##  4. Global reactive power vs. date-time
##

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
    
    par(mfrow=c(2,2), mar=c(4,4,2,2), cex=0.5) # add mar parameters if required later
    
    plot_GlobalActivePower_vs_Time(powerDataWithTimeStamp)
    plot_Voltage_vs_Time(powerDataWithTimeStamp)
    plot_EnergySubMetering_vs_Time(powerDataWithTimeStamp)
    plot_GlobalReactivePower_vs_Time(powerDataWithTimeStamp)
    
    dev.off()
    
}

##
##  1. Plot the Global Active Power vs. Date-time
##  the data frame with the source data (and time stamp column added) is the 
##  required parameter
##
plot_GlobalActivePower_vs_Time <- function(powerDataWithTimeStamp)
{
    ##
    ##  Code is taken from the code of Plot2.R sans the graphics device setting.
    ##  That is set by the caller
    ##
    
    ##
    ## convert global power in the data frame to numeric
    ##
    powerDataWithTimeStamp$Global_active_power = 
        as.numeric(as.character(powerDataWithTimeStamp$Global_active_power))
    
    ##
    ##  Plot the data. It is assumed that the caller has set the drawing device
    ##  as well as layout
    ##    
    with(powerDataWithTimeStamp,{
        plot(timeStamp,Global_active_power, type = "n",
             xlab = "",
             ylab = "Global Active Power")
        lines(timeStamp, Global_active_power)
    })        
}

##
##  2. Plot the Voltage vs. date-time.
##  the data frame with the source data (and time stamp column added) is the 
##  required parameter
##
plot_Voltage_vs_Time <- function(powerDataWithTimeStamp)
{
    ##
    ##  convert the voltage data to numeric
    ##
    powerDataWithTimeStamp$Voltage = 
        as.numeric(as.character(powerDataWithTimeStamp$Voltage))
    
    ##
    ##  Plot the data. It is assumed that the caller has set the drawing device
    ##  as well as layout
    ##    
    with(powerDataWithTimeStamp,{
        plot(timeStamp,Voltage, type = "n",
             xlab = "datetime",
             ylab = "Voltage")
        lines(timeStamp, Voltage)
    })
}

##
##  3. Plot the Energy sub metering vs. date-time.
##  the data frame with the source data (and time stamp column added) is the 
##  required parameter
##
plot_EnergySubMetering_vs_Time <- function(powerDataWithTimeStamp)
{
    ##
    ##  Code is taken from the code of Plot3.R sans the graphics device setting.
    ##  That is set by the caller
    ##
    
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
    ##  Plot the data. It is assumed that the caller has set the drawing device
    ##  as well as layout
    ##    
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
    
}

##
##  4. Plot the Global Reactive Power vs. date-time.
##  the data frame with the source data (and time stamp column added) is the 
##  required parameter
##
plot_GlobalReactivePower_vs_Time <- function(powerDataWithTimeStamp)
{
    ##
    ##  convert the Global reactive power data to numeric
    ##
    powerDataWithTimeStamp$Global_reactive_power = 
        as.numeric(as.character(powerDataWithTimeStamp$Global_reactive_power))
    
    ##
    ##  Plot the data. It is assumed that the caller has set the drawing device
    ##  as well as layout
    ##    
    with(powerDataWithTimeStamp,{
        plot(timeStamp,Global_reactive_power, type = "n",
             xlab = "datetime",
             ylab = "Global_reactive_power")
        lines(timeStamp, Global_reactive_power)
    })    
}