## setwd("C:/Users/dlanc/Documents/PhD/R/Nootka") 

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")

## There is two files we will be working with, one with the Staroddi data 
## the other is manual measurements of temp and depth from ROV 
lp("tidyverse")
lp("readxl")
lp("dplyr")
lp("writexl")

ManualDT <- read_excel("odata/StarOddi_Depth_Temp.xlsx")
StarDT <- read_excel("odata/StarOddiDataNootka_.xlsx")
colnames(StarDT)[which(names(StarDT) == "Depth(m)")] <- "Depth"
unique(StarDT$Date)

## lets start working on subsetting the data to remove excess points 
## only care about positive depth values 
StarDT <- StarDT %>%
  ## we only care about the data in depths greater then 10m (thermocline)
  filter(Depth > 10) %>%
  ## we void all the study sites from the first day (NS01 - NS04)
  filter(!Date %in% "10.08.2023")

write_xlsx(StarDT, "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/odata/StarDT.xlsx")
#

