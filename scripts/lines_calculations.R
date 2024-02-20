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
lp("Pracma")

#### Set Working Directory #### 

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1")

NS07 <- read.csv("100mLines/Bottom/NS06_T1_Biosonic_20230811_104843_Bottom_100.line.csv")

# Function to calculate the great circle distance between two points 
calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
  distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
  return(distance)
}

# Apply to data 
NS07$GreatCircleDistance[2:(nrow(NS07))] <- sapply(2:(nrow(NS07)), function(i) {
  lat1 <- NS07$Latitude[i - 1]
  lon1 <- NS07$Longitude[i - 1]
  lat2 <- NS07$Latitude[i]
  lon2 <- NS07$Longitude[i]
  calc_great_circle_distance(lat1, lon1, lat2, lon2)
})

## Calculate difference in depth 
NS07$DepthDifference <- c(NA, NS07$Depth[-1] - NS07$Depth[-nrow(NS07)])

# Calculate Slope 
NS07$Slope <- sapply(1:nrow(NS07), function(i) {
  ifelse(is.na(NS07$GreatCircleDistance[i]) || is.na(NS07$DepthDifference[i]),
         NA,
         (180.0/pi) * atan(NS07$GreatCircleDistance[i] / NS07$DepthDifference[i]))
})
# calculate hypotenuse 
NS07$Hypothenuse <- sqrt(NS07$GreatCircleDistance^2 + NS07$DepthDifference^2)

# Cumulative sum of great circle distance 
NS07$CumSumGCD <- cumsum(NS07$GreatCircleDistance)

sum(NS07$GreatCircleDistance, na.rm = TRUE)
sum(NS07$Hypothenuse, na.rm = TRUE)
