
library (dplyr)
library(data.table)
library(lubridate)


readHPC <- function () {
 HPC <<- 
   data.table (read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings="?", as.is=TRUE))
 
 HPC <<- HPC %>% 
    rename (date_txt = Date) %>%
    mutate (Date = as.Date(date_txt,"%d/%m/%Y")) %>%
    filter (Date >="2007-02-01") %>%
    filter ("2007-02-02" >= Date)   
}

saveplot <- function (plot_f){
  filename <- paste ("project2/", as.character(substitute(plot_f)), ".png", sep="")
  png(file=filename, width=480,height=480)
  plot_f()  
  dev.off()
}

plot1 <- function () {
  with (HPC, 
        hist (Global_active_power, 
              col="red",
              main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency"))  
}

plot2 <- function (){
  with (HPC, 
        plot (Global_active_power, type = "l", col="black",xaxt = "n",
              xlab="", ylab = "Global Active Power (kilowatts)")
             )  
  axis (1, c(0, length (HPC$Global_active_power)/2, length (HPC$Global_active_power)), c("Thu", "Fri", "Sat"))
  
}

plot3 <- function () {
  with (HPC, 
        plot (Sub_metering_1, type = "l", col="black",xaxt = "n",
              xlab="", ylab = "Energy sub metering"))  
  with (HPC, 
        lines (Sub_metering_2, type = "l", col="red",xaxt = "n",
              xlab="", ylab = ""))  
  with (HPC, 
        lines (Sub_metering_3, type = "l", col="blue",xaxt = "n",
              xlab="", ylab = ""))  
  axis (1, c(0, length (HPC$Global_active_power)/2, length (HPC$Global_active_power)), c("Thu", "Fri", "Sat")) 
  
  legend ("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
          col = c("black", "red", "blue"), lty = 1, cex=.75)
}

plot4b <- function () {
  with (HPC, 
        plot (Voltage, type = "l", col="black",xaxt = "n",
              xlab="datetime", ylab = "Voltage")
  )  
  axis (1, c(0, length (HPC$Global_active_power)/2, length (HPC$Global_active_power)), c("Thu", "Fri", "Sat"))
  
}

plot4d <- function () {
  with (HPC, 
        plot (Global_reactive_power, type = "l", col="black",xaxt = "n",
              xlab="datetime", ylab = "Global_reactive_power")
  )  
  axis (1, c(0, length (HPC$Global_active_power)/2, length (HPC$Global_active_power)), c("Thu", "Fri", "Sat"))
  
}


plot4 <- function () {
  par(mfrow=c(2,2))
  plot2()
  plot4b()
  plot3()
  plot4d()
}