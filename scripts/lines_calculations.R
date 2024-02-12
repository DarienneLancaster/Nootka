#### Load Packages #### 
lp<-function(pck){
  if(!require(pck,character.only = TRUE))install.packages(pck);library(pck,character.only = TRUE)
}
lp("tidyverse")
lp("lubridate")
lp("dplyr")
lp("ggplot2")
lp("flextable")
lp("xlsx")

#### Set Working Directory #### 

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1")

NS07 <- read.csv("100mLines/Bottom/NS06_T1_Biosonic_20230811_104843_Bottom_100.line.csv")

# Function to calculate the great circle distance between two points 
calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
  distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
  return(distance)
}


# Apply 
NS07$GreatCircleDistance <- c(0, sapply(2:(nrow(NS07)- 1), function(i) {
  lat1 <- NS07$Latitude[i]
  lon1 <- NS07$Longitude[i]
  lat2 <- NS07$Latitude[i+1]
  lon2 <- NS07$Longitude[i+1]
  calc_great_circle_distance(lat1, lon1, lat2, lon2)
}))

NS07$DepthDifference <- c(NA, NS07$Depth[1] - NS07$Depth[+1])

