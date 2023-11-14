## setwd("C:/Users/dlanc/Documents/PhD/R/Nootka") 

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")

## There is two files we will be working with, one with the Staroddi data 
## the other is manual measurements of temp and depth from ROV 
lp("tidyverse")
lp("readxl")
lp("dplyr")

ManualDT <- read_excel("odata/StarOddi_Depth_Temp.xlsx")
StarDT <- read_excel("odata/StarOddiDataNootka_.xlsx")
colnames(StarDT)[which(names(StarDT) == "Depth(m)")] <- "Depth"
## first lets filter all the empty rows under site ID 

unique(StarDT$Date)
## lets start working on subsetting the data to remove excess points 
## only care about positive depth values 
StarDT <- StarDT %>%
  filter(Depth > 0) + 
  filter(!Date == "10.08.2023")
