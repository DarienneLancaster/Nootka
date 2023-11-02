#REMEMBER TO PULL from GIT BEFORE DOING ANYTHING IN THE SCRIPT!!!#####
#Remember to COMMIT and PUSH before stopping work for the day####
# Test push # 

#Coding Club is a very helpful website if you're looking for helpful tips on R. Worthwhile to work
#through some of the intro tutorials and some of the graphing tutorials
#All Tutorials
#https://ourcodingclub.github.io/tutorials.html
#Graphing/Data Visualization
#https://ourcodingclub.github.io/tutorials/datavis/
#Intro to dplyr, bullet point #3. (this is a super useful package to use, I use it for almost everything)
#https://ourcodingclub.github.io/tutorials/data-manip-intro/

# Open install_packages.R script separately and run it. 
#This creates a super easy function to install packages quickly for the rest of your work
#now install these packages using the lp function
lp("tidyverse")
lp("lubridate")
lp("dplyr")

#set working directory (make sure you have a folder named Nootka with subfolders
#odata, wdata, scripts, figures)

## setwd("C:/Users/dlanc/Documents/PhD/R/Nootka") 

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")

#load Event Measure annotation file (need to edit header of csv to delete extra info at top)
ROV<-read.csv("odata/ROVcomplete.csv",skip=4, stringsAsFactors = FALSE)
str(ROV)

#remove unnecessary columns
ROV=select(ROV, -4:-21)
ROV = select(ROV, -"Code")


#remove Start and End annotation rows
#ROV<-ROV %>%
#  filter(Notes!='Start', Notes!='start', Notes!='End', Notes!='end')
## the start and end for each site could be used to get accurate times surveying sites 

#create substrate only dataset named ROVS
ROVSub <-ROV%>%
  filter(Activity=="Attracted")

#create fish only dataset named ROVFish

ROVFish<-ROV%>%
  filter(Activity!="Attracted", Number!= "NA")

#replace blanks in Family, Genus, Species with unknown instead of NAs
ROVFish$Species[ROVFish$Species== ""] <- "unknown" 
ROVFish$Genus[ROVFish$Genus== ""] <- "unknown"
ROVFish$Family[ROVFish$Family== ""] <- "unknown"


#convert Species column to factor with levels
ROVFish$Species<- as.factor(ROVFish$Species)
#check how many levels Species column has
levels(ROVFish$Species)

#repeat for Genus and Family, etc...
ROVFish$Genus<- as.factor(ROVFish$Genus)
levels(ROVFish$Genus)
ROVFish$Family<- as.factor(ROVFish$Family)
levels(ROVFish$Family)

#ROVF$Filename<- as.factor(ROVF$Filename)
#levels(ROVF$Filename)

#ROVF$Frame<- as.factor(ROVF$Frame)
#levels(ROVF$Frame)

#rename column heads 
## this code will remain consistent even if the column numbers change 
colnames(ROVFish)[which(names(ROVFish) == "Time..mins.")] <- "Time"
colnames(ROVFish)[which(names(ROVFish) == "Depth.1")] <- "Depth"

## pausing converting Time to factor, will need to be numeric or POSIXct for calculations 
## ROVF$Time<- as.factor(ROVFish$Time)
# levels(ROVFish$Time)
# ROVFish$Depth <- as.numeric(ROVFish$Depth)
#levels(ROVF$Depth)

#create new column with Family, Genus, Species combined
ROVFish<- ROVFish%>%
  mutate(FullName = paste(Family, Genus, Species, sep = " "))

#NOTE - you'll need to convert columns to factors for the ROVS data set too.
#there are likely other variable like number that need to be converted to factors too
#you can check this with str(ROVF)

#some additional code that might be useful

#create new dataframe to sum all fish of each species together for each deployment
#(remove FullName if you want all fish together, you'll likely want to filter out pelagic schools as well)
#fishsum<- ROVF %>%
#  filter(!grepl("SP|SB",Notes))%>% #this filters out all the rows with SP or SB (grepl searches for parts of words, it's like contains)
 # group_by(Filename, FullName)%>%
  #summarize(TotFish=sum(Number))

#add zero counts for species not seen at each site
#addzero <- expand.grid(Filename = unique(fishsum$Filename), FullName = unique(fishsum$FullName))
#fishtotal<-merge(addzero, fishsum, by = c('Filename', 'FullName'), all.x = TRUE)
#convert NAs to zeros
#fishtotal$TotFish[is.na(fishtotal$TotFish)] <- 0 


#this is code for joining video data to other data like echosounder data
#based on matching names in the Filename column
#(this is just a demo merge with the ROVS dataset)
#Demo<-left_join(fishtotal,ROVS,by="Filename")

##################################################################################################################################################################

## Create unique site ID 

## this function will pull the first 4 letters of a character and then paste them in a new column to make a unique site ID 
## this will allow us to merge datasets using this column 
ROVFish$Site_ID <- substr(ROVFish$Filename, 1, 4)

## load in nootka data 
load("nootkadata.Rdata")
## Lets make a dataset with Site info for each site. 

colnames(nootkadata)

siteinfo2 <- unique(nootkadata[, c("Site_ID", "Date", "Temp", "Lat_Decimal", "Long_Decimal")])

## Create a function to calculate Fish abundance per site 

get.abundance <- function(xx){ 
  keep <- which(ROVFish$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(ROVFish$Number[keep]))
}

head(siteinfo2)
## Use Mapply to fill siteinfo2 with total number of fish 
siteinfo2$Abundance <- mapply(get.abundance, xx = siteinfo2$Site_ID)

## Get Abundance per species ## 





























