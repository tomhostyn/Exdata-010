library (dplyr)

loadData <- function () {
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
}

saveplot <- function (plot_f){
  filename <- paste ("project2/", as.character(substitute(plot_f)), ".png", sep="")
  #filename <- paste ("project2/", filename, sep="")
  png(file=filename, width=480,height=480)
  plot_f()  
  dev.off()
}


plot1 <- function () {
  by_year <- group_by(NEI, year) %>% 
    summarize (sum = sum (Emissions))
    plot (by_year, ylab = "Total PM2.5 Emission (tons)")
    abline (lm (sum ~ year , data = by_year))
}