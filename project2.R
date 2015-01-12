library (dplyr)
library (ggplot2)

if (!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if (!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

saveplot <- function (plot_f){
  filename <- paste ("project2/", as.character(substitute(plot_f)), ".png", sep="")
  png(file=filename, width=480,height=480)
  plot_f()  
  dev.off()
}

saveggplot <- function (plot_f){
  filename <- paste ("project2/", as.character(substitute(plot_f)), ".png", sep="")
  p <- plot_f()
  ggsave(filename=filename, plot=p, dpi=100)
}

plot1 <- function () {
  by_year <- group_by(NEI, year) %>% 
    summarize (sum = sum (Emissions))
    plot (by_year, main = "PM2.5 Emissions (US Total)", ylab = "PM2.5 Emitted (tons)", col="red", pch=2)
    abline (lm (sum ~ year , data = by_year))
}

plot2 <- function () {
  by_year <- NEI %>%
    filter (fips == "24510") %>%
    group_by(year) %>% 
    summarize (sum = sum (Emissions))
  plot (by_year, main = "PM2.5 Emissions (Baltimore City, MD)", ylab = "PM2.5 Emitted (tons)", col="red", pch=2)
  abline (lm (sum ~ year , data = by_year))
}

plot3 <- function () {
  by_year <- NEI %>%
    filter (fips == "24510") %>%
    group_by(type, year) %>% 
    summarize (sum = sum (Emissions))
    qplot (year, sum, data = by_year, facets = .~type, geom = c("point", "line"),
         main = "PM2.5 Emissions (Baltimore City, MD) by type", 
         ylab= "PM2.5 Emitted (tons)",
         xlab= "Year")  
}

plot4 <- function () {
  coalSrc <- SCC[grep ("Coal", SCC$Short.Name),"SCC"]
  by_year <- NEI %>%
    filter (SCC %in% coalSrc) %>%
    group_by(year) %>% 
    summarize (sum = sum (Emissions))
    qplot (year, sum, data = by_year, geom = c("point", "line"), 
           main = "Coal Combustion-related PM2.5 Emissions (US Total)", 
           ylab= "PM2.5 Emitted (tons)",
           xlab= "Year") 
}

plot5 <- function () {
  by_year <- NEI %>%
    filter (type == "ON-ROAD") %>%
    group_by(year) %>% 
    summarize (sum = sum (Emissions))
    qplot (year, sum, data = by_year, geom = c("point", "line"), 
           main = "Vehicle Combustion-related PM2.5 Emissions (US Total)", 
           ylab= "PM2.5 Emitted (tons)",
           xlab= "Year")    
}

plot6 <- function () {
  fips <- c("24510", "06037")
  County <- c("Baltimore City, MD", "Los Angeles County, CA")
  map <- data.frame (fips, County)
  
  by_year <- NEI %>%
    inner_join(map, by="fips") %>%    # drops all redundant counties in the process :)
    filter (type == "ON-ROAD") %>%
    group_by(County, year) %>% 
    summarize (sum = sum (Emissions))
    qplot (year, sum, data = by_year, facets = .~County, geom = c("point", "line"), 
           main = "Vehicle Combustion-related PM2.5 Emissions", 
           ylab= "PM2.5 Emitted (tons)",
           xlab= "Year")    
}