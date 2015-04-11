# Plot1.R
# Generate a Histogram for Global active power

CreatePlot1 <- function()
{
    ##
    ##  just read the data for Feb 1 2007 and Feb 2 2007
    ##
    filePath = "data/household_power_consumption.txt"    
    powerData = subset(read.csv(filePath, sep = ";",), 
                       Date == "1/2/2007" | Date == "2/2/2007")
    
    ##
    ##  Plot 1 is a historgram.
    ##
    png("figure/plot1.png")
    hist(as.numeric(as.character(powerData$Global_active_power)), 
         col="red", 
         main = "Global Active Power", 
         xlab = "Global Active Power (kilowatts)")
    dev.off()
}