

loadData <- fuction () {
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
}

saveplot <- function (plot_f){
  filename <- gsub (" ", "", paste (as.character(substitute(plot_f)), ".png"))
  filename <- gsub ("project2", filename)
  png(file=filename, width=480,height=480)
  plot_f()  
  dev.off()
}


plot1 <- function () {
  
}