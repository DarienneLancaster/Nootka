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

siteinfo <- unique(nootkadata[, c("Site_ID", "Date", "Temp", "Lat_Decimal", "Long_Decimal")])

head(siteinfo)

## Create a function to calculate Fish abundance per site 

get.abundance <- function(xx){ 
  keep <- which(ROVFish$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(ROVFish$Number[keep]))
}

## Use Mapply to fill siteinfo with total number of fish 
siteinfo$Abundance <- mapply(get.abundance, xx = siteinfo$Site_ID)

## Get Abundance per species ## 

## Calculate abundance of each species per site 

sitespecies <- unique(ROVFish[, c("Site_ID", "FullName")])

# create a function 
get.abundance.species <- function(xx, ss){ 
  keep <- which(ROVFish$Site_ID == xx & ROVFish$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(ROVFish$Number[keep]))
}

## apply function 
sitespecies$Abundance <- mapply(get.abundance.species, xx = sitespecies$Site_ID, ss = sitespecies$FullName)

## reshape the dataframe so it shows each species per site along with its abundance at that site
sitespecies <- reshape(sitespecies, v.names = "Abundance", idvar = "Site_ID", timevar = "FullName", direction = "wide")
sitespecies[is.na(sitespecies)] <- 0

#### SPECIES RICHNESS PER SITE 
speciesrichnes <- function(x) { 
  apply(sitespecies[, 2:22 ]> 0, 1, sum)
}
sitespecies$SpeciesRichness <- mapply(speciesrichnes, x = 2)

## now lets add species richness total from sitespecies into siteinfo

addSR <-  function(xx, ss){ 
  keep <- which(sitespecies$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(sitespecies$SpeciesRichness[keep]))
}

siteinfo$speciesrichness <- mapply(addSR, xx = siteinfo$Site_ID)

###################################################################################################################################################################

## Calculate species richness for only sebastes 

#create sebastes only dataset 

Sebastes <-  ROVFish%>%
  filter(Genus == "Sebastes")


sitesebastes <- unique(Sebastes[, c("Site_ID", "FullName")])

# create a function 
get.abundance.sebastes <- function(xx, ss){ 
  keep <- which(Sebastes$Site_ID == xx & Sebastes$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(Sebastes$Number[keep]))
}

## apply function 
sitesebastes$Abundance <- mapply(get.abundance.sebastes, xx = sitesebastes$Site_ID, ss = sitesebastes$FullName)

## reshape the dataframe so it shows each species per site along with its abundance at that site
sitesebastes <- reshape(sitesebastes, v.names = "Abundance", idvar = "Site_ID", timevar = "FullName", direction = "wide")
sitesebastes[is.na(sitesebastes)] <- 0

## Calculate Sebastes total abundance per site 
sebastesabundance <- function(x) {
  apply(sitesebastes[, 2:11], 1, sum)
}

sitesebastes$Total_Abund <- mapply(sebastesabundance, x = 2)

## add this to site info 
addSA <- function(xx, ss){
  keep <- which(sitesebastes$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(sitesebastes$Total_Abund[keep]))
}

siteinfo$SebastesAbundance <- mapply(addSA, xx = siteinfo$Site_ID)


#### Sebastes species richness per site 
sebspeciesrichnes <- function(x) { 
  apply(sitesebastes[, 2:11 ]> 0, 1, sum)
}
sitesebastes$SpeciesRichness <- mapply(sebspeciesrichnes, x = 2)

## now lets add sebastes species richness total from sitesebastes into siteinfo

addSSR <-  function(xx, ss){ 
  keep <- which(sitesebastes$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(sitesebastes$SpeciesRichness[keep]))
}

siteinfo$SebastesSR <- mapply(addSSR, xx = siteinfo$Site_ID)

#####################################################################################################################################################

## lets look at the relative frequency of each species 

## Create a function to count the number of each species of fish using the FullName

speciescount <- function(xx){ 
  keep <- which(ROVFish$FullName == xx)
  if (length(keep) == 0) return(0)
  return(sum(ROVFish$Number[keep]))
}

## create a data set to populate 
# to create a data frame, you need more then only column to add to the datafrane 

species <- unique(ROVFish[, c("Family", "Genus", "Species", "FullName")])

species$count <- mapply(speciescount, xx = species$FullName )


# now we have calculated the total number of each species seen across all of the sites
# we can use this to calculate the relative frequency of each species across all surveys 

## function to calculate relative frequency 

library(tidyverse)

species1 <- species %>% group_by(FullName) %>% 
   summarise( total_count = sum(count), .groups = "drop" ) %>% 
   mutate( frequency = total_count / sum(total_count) ) 
View(species1)

library(ggplot2)


species1 %>%
  mutate(name = fct_reorder(FullName, desc(frequency))) %>%
  ggplot( aes(x= reorder(FullName, - frequency), y=frequency)) +
  geom_bar(stat="identity", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## lets remove unknown unknown unknown to get a better understanding of known species  
species2 <- species %>%
  filter(!FullName == "unknown unknown unknown")

species2 <- species2 %>% group_by(FullName) %>% 
  summarise( total_count = sum(count), .groups = "drop" ) %>% 
  mutate( frequency = total_count / sum(total_count) ) 
View(species2)

species2 %>%
  mutate(name = fct_reorder(FullName, desc(frequency))) %>%
  ggplot( aes(x= reorder(FullName, - frequency), y=frequency)) +
  geom_bar(stat="identity", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## redo for only sebastes 

rockfish <- species %>%
  filter(Genus == "Sebastes")

rockfish <- rockfish %>% group_by(FullName) %>% 
  summarise( total_count = sum(count), .groups = "drop" ) %>% 
  mutate( frequency = total_count / sum(total_count) ) 

rockfish %>%
  mutate(name = fct_reorder(FullName, desc(frequency))) %>%
  ggplot( aes(x= reorder(FullName, - frequency), y=frequency)) +
  geom_bar(stat="identity", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()


####################################################################################################################################################

## Write code for fish schools abundance and number 

# subset data to get pelagic schools only 
# pelagic schools were activity == scavenging 

pelagic <-  ROVFish%>%
  filter(Activity == "Scavenging")

## pelagic only by number of school and fish in each school 
# we need to figure out a way to total the fish in each school even considering the SP_C 

