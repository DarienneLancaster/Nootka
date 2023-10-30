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

#set working directory (make sure you have a folder named Nootka with subfolders
#odata, wdata, scripts, figures)
setwd("C:/Users/dlanc/Documents/PhD/R/Nootka")
#double check working directory 
getwd()

#load Event Measure annotation file (need to edit header of csv to delete extra info at top)
ROV<-read.csv("odata/Point measurements_20230922.csv",skip=4, stringsAsFactors = FALSE)
str(ROV)

#remove unnecessary columns
ROV=select(ROV, -4:-21)
str(ROV)

#remove Start and End annotation rows
ROV<-ROV %>%
  filter(Notes!='Start', Notes!='start', Notes!='End', Notes!='end')

#create substrate only dataset named ROVS
ROVS<-ROV%>%
  filter(Activity=="Attracted")

#create fish only dataset named ROVF
ROVF<-ROV%>%
  filter(Activity!="Attracted", Number!= "NA")

#replace blanks in Family, Genus, Species with unknown instead of NAs
ROVF$Species[ROVF$Species== ""] <- "unknown" 
ROVF$Genus[ROVF$Genus== ""] <- "unknown"
ROVF$Family[ROVF$Family== ""] <- "unknown"

#convert Species column to factor with levels
ROVF$Species<- as.factor(ROVF$Species)
#check how many levels Species column has
levels(ROVF$Species)

#repeat for Genus and Family, etc...
ROVF$Genus<- as.factor(ROVF$Genus)
levels(ROVF$Genus)
ROVF$Family<- as.factor(ROVF$Family)
levels(ROVF$Family)

ROVF$Filename<- as.factor(ROVF$Filename)
levels(ROVF$Filename)

ROVF$Frame<- as.factor(ROVF$Frame)
levels(ROVF$Frame)

#rename column head #3
colnames(ROVF)[3] ="Time"
colnames(ROVF)[11] ="Depth"

ROVF$Time<- as.factor(ROVF$Time)
levels(ROVF$Time)
ROVF$Depth<- as.factor(ROVF$Depth)
levels(ROVF$Depth)

#create new column with Family, Genus, Species combined
ROVF<- ROVF%>%
  mutate(FullName = paste(Family, Genus, Species, sep = " "))

#NOTE - you'll need to convert columns to factors for the ROVS data set too.
#there are likely other variable like number that need to be converted to factors too
#you can check this with str(ROVF)

#some additional code that might be useful

#create new dataframe to sum all fish of each species together for each deployment
#(remove FullName if you want all fish together, you'll likely want to filter out pelagic schools as well)
fishsum<- ROVF %>%
  filter(!grepl("SP|SB",Notes))%>% #this filters out all the rows with SP or SB (grepl searches for parts of words, it's like contains)
  group_by(Filename, FullName)%>%
  summarize(TotFish=sum(Number))

#add zero counts for species not seen at each site
addzero <- expand.grid(Filename = unique(fishsum$Filename), FullName = unique(fishsum$FullName))
fishtotal<-merge(addzero, fishsum, by = c('Filename', 'FullName'), all.x = TRUE)
#convert NAs to zeros
fishtotal$TotFish[is.na(fishtotal$TotFish)] <- 0 


#this is code for joining video data to other data like echosounder data
#based on matching names in the Filename column
#(this is just a demo merge with the ROVS dataset)
Demo<-left_join(fishtotal,ROVS,by="Filename")


