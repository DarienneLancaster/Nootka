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

## load in subarea data 
subarea<-read.csv("odata/Sitebysubarea.csv")
head(subarea)
colnames(subarea)[which(names(subarea) == "Site.ID")] <- "Site_ID"

#load Event Measure annotation file (need to edit header of csv to delete extra info at top)
ROV<-read.csv("odata/ROVcomplete.csv", skip=4, stringsAsFactors = FALSE)


#remove unnecessary columns
ROV=select(ROV, -4:-21)
ROV = select(ROV, -"Code")


#remove Start and End annotation rows
#ROV<-ROV %>%
#  filter(Notes!='Start', Notes!='start', Notes!='End', Notes!='end')
## the start and end for each site could be used to get accurate times surveying sites 



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
siteinfo <- merge(siteinfo, subarea, by = "Site_ID")

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


## plot species richness 
  
ggplot(siteinfo %>% filter(!Site_ID %in% c("NS01", "NS02", "NS03", "NS04")),
       aes(x = Site_ID, y = SebastesSR)) +
    geom_histogram(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
    labs(x = "\n Site ID", y = "Rockfish species richness") 
 
## plot rockfish abundance per site

siteinfo_1 <- tidyr::pivot_longer(siteinfo, cols=c('SebastesAbundance', 'Abundance'), names_to='variable', 
                           values_to="value")

ggplot(siteinfo_1 %>% filter(!Site_ID %in% c("NS01", "NS02", "NS03", "NS04")),
       aes(x=Site_ID, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "\n Site ID", y = "Abundance") +
  scale_fill_manual(values = c(SebastesAbundance = "blue", Abundance = "red")) +
  guides(fill = guide_legend(title = NULL))
 # geom_bar(stat='identity', position='dodge')


ggplot(siteinfo %>% filter(!Site_ID %in% c("NS01", "NS02", "NS03", "NS04")),
       aes(x = Site_ID, y = SebastesAbundance)) +
  geom_histogram(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  labs( x = "\n Site ID", y = "Rockfish abundance")
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


result_table <- species1 %>%
  mutate(name = forcats::fct_reorder(FullName, desc(total_count))) %>%
  arrange(desc(total_count)) %>%
  select(FullName, total_count = total_count)

# Display the table
kable(result_table)

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


## just Rockfish lets put this in a table with the total count 
lp('knitr')

result_table1 <- rockfish %>%
  mutate(name = forcats::fct_reorder(FullName, desc(total_count))) %>%
  arrange(desc(total_count)) %>%
  select(FullName, total_count = total_count)

# Display the table
kable(result_table1)

write.csv(result_table1,"wdata/total_count_rockfish.csv", row.names = FALSE )
####################################################################################################################################################

## Write code for fish schools abundance and number 

# subset data to get pelagic schools only 
# pelagic schools were activity == scavenging 

pelagic <-  ROVFish%>%
  filter(Activity == "Scavenging")
## realized that this is not needed because there are large fish schools that
# where labeled "benthic" and catergorized as passing 
## pelagic only by number of school and fish in each school 
ROVFish <- ROVFish %>% 
  mutate(rowID = as.numeric(row_number()))
str(ROVFish)
process_school_type <- function(ROVFish, school_type, continuation_type){
  ROVFish %>% 
    mutate(SchoolID = cumsum(Notes == school_type)) %>%
    group_by(SchoolID) %>%
    filter(Notes %in% c(school_type, continuation_type) | SchoolID == 0) %>%
    mutate(ValidNumber = ifelse(Notes != "", Number, NA_integer_)) %>%
    summarise(
      Notes = first(Notes[Notes == school_type]), 
      TotalNumber = sum(ValidNumber, na.rm = TRUE),
     Site_IDS = list(unique(Site_ID)),
     Activitys = list(unique(Activity)), 
      .groups = "drop"
    )
}

# run each school type through the function 
sp_data <- process_school_type(ROVFish, "SP", "SP_C")
sp_data
bp_data <- process_school_type(ROVFish, "BP", "BP_C")
bp_data
sb_data <- process_school_type(ROVFish, "SB", "SB_C")
sb_data

# Combine the processed data back into one dataframe
combined_data <- bind_rows(sp_data, bp_data, sb_data)

## now this combined data has the site_ID and activity type associated with 
# the schools, now we can calculate how many schools in each site and add to 

## first we need to clean up the data set 
## remove the Notes labeled as "NA", it appears to be errors in these values 
cleandata <-combined_data %>%
  filter(Notes!= "NA")

## insure Site_IDS are characters. 
cleandata$Site_ID <- as.character(cleandata$Site_IDS)
## code to create a new column with school ID that is unique per school site (SP, BP etc.)
# this will make it easier to do something with the data 
cleandata$School_ID <- paste(cleandata$Notes, cleandata$SchoolID, sep = ".")
## remove unnessecary columns 
cleandata <- select(cleandata, -c(Activitys, Notes, SchoolID, Site_IDS))

## make the data into a wide form to figure out how many schools are at each site
wide_data <- cleandata %>%
  pivot_wider(names_from = School_ID, values_from = TotalNumber, values_fill = 0)
view(cleandata)
## Calculate the total fish in schools at a site. 
numberofschoolingfish <- function(x) {
  apply(wide_data[, 2:46], 1, sum)
}

wide_data$TotalSF <- mapply(numberofschoolingfish, x = 2)

## create a function that adds this to site info 
addSF <- function(xx, ss){
  keep <- which(wide_data$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(wide_data$TotalSF[keep]))
}
## add the total number of schooling fish to Site info 
siteinfo$TotalSF <- mapply(addSF, xx = siteinfo$Site_ID)


#### function that counts the fish school at each site
numberFS <- function(x) { 
  apply(wide_data[, 2:46 ]> 0, 1, sum)
}
wide_data$NumberFS <- mapply(numberFS, x = 2)

## now lets add sebastes species richness total from sitesebastes into siteinfo

addFS <-  function(xx, ss){ 
  keep <- which(wide_data$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(wide_data$NumberFS[keep]))
}

siteinfo$NumberFS <- mapply(addFS, xx = siteinfo$Site_ID)

## plot rockfish abundance per site
ggplot(siteinfo %>% filter(!Site_ID %in% c("NS01", "NS02", "NS03", "NS04")),
       aes(x = Site_ID, y = NumberFS)) +
  geom_histogram(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  labs( x = "\n Site ID", y = "Number of pelagic schools")

write.csv(siteinfo,"wdata/siteinfo.csv", row.names = FALSE)


#############################################################################################
## Calculate the abundance and species richness of non-schooling fish. 
fishschool <-c("End", "Start","SP", "SP_C","BB_C","BP", "SB", "BP_C", "SB_C", "BB")

##Remove all fishschools that are denoted as these values
ROV_noschool <- ROVFish %>%
  filter(! Notes %in% fishschool)
## looking over the dataset there are multiple fishschools that weren't denoted in "Notes" 
ROV_noschool <- ROVFish %>%
  # Fish schools, if we call over 6 fish schooling 
  filter(! Number > 6)

# Calculate abundance of each species per site 

sitenoschool <- unique(ROV_noschool[, c("Site_ID", "FullName")])

# create a function 
get.noschool.abundance.species <- function(xx, ss){ 
  keep <- which(ROV_noschool$Site_ID == xx & ROV_noschool$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(ROV_noschool$Number[keep]))
}

## apply function 
sitenoschool$Abundance <- mapply(get.noschool.abundance.species, xx = sitenoschool$Site_ID, ss = sitenoschool$FullName)

## reshape the dataframe so it shows each species per site along with its abundance at that site
sitenoschool <- reshape(sitenoschool, v.names = "Abundance", idvar = "Site_ID", timevar = "FullName", direction = "wide")
sitenoschool[is.na(sitenoschool)] <- 0

#### SPECIES RICHNESS PER SITE 
speciesrichnes <- function(x) { 
  apply(sitenoschool[, 2:21 ]> 0, 1, sum)
}
sitenoschool$SRnoschool <- mapply(speciesrichnes, x = 2)

## now lets add species richness into siteinfo

addSR <-  function(xx, ss){ 
  keep <- which(sitenoschool$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(sitenoschool$SRnoschool[keep]))
}

siteinfo$SRnoschool <- mapply(addSR, xx = siteinfo$Site_ID)

## lets calculate abundance of non-schooling fish 
noschoolabundance <- function(x) {
  apply(sitenoschool[, 2:21], 1, sum)
}

sitenoschool$AbundanceNoschool <- mapply(noschoolabundance, x = 2)

## lets add the abundance of non-schooling fish 
addabundance <-  function(xx, ss){ 
  keep <- which(sitenoschool$Site_ID == xx)
  if (length(keep) == 0) return(0)
  return(sum(sitenoschool$AbundanceNoschool[keep]))
}

siteinfo$Abundancenoschool <- mapply(addabundance, xx = siteinfo$Site_ID)
###############################################################################################
# Create substrate only dataset named ROVS
ROVSubSlope <-ROV%>%
  filter(Activity=="Attracted" | Notes == "End")
## remove unnessecary columns 
## this function will pull the first 4 letters of a character and then paste them in a new column to make a unique site ID 
## this will allow us to merge datasets using this column 
ROVSubSlope$Site_ID <- substr(ROVSubSlope$Filename, 1, 4)
## remove unnecessary columns 
ROVSubSlope<- select(ROVSubSlope, -c(Stage, Family, Genus, Species, Number, Activity, Filename))
## rename columns 
colnames(ROVSubSlope)[which(names(ROVSubSlope) == "Depth.1")] <- "Depth"
colnames(ROVSubSlope)[which(names(ROVSubSlope) == "Time..mins.")] <- "Time"

## ensure time is numeric so that you can preform a calculation with the time 
ROVSubSlope$Time <- as.numeric(ROVSubSlope$Time)

## Code that creates a new column with just substrate information 
ROVSubSlope$Sub <- substr(ROVSub$Sub_Slope, 1, 1)
## lets repeat this for slope only 
ROVSubSlope$Slope <- substr(ROVSub$Sub_Slope, 3, 4)


## create a new dataset called duration time 
Sub_Duration_data <- ROVSubSlope %>%
  ## arrange 
  arrange(Site_ID, Time) %>%
  ## group the data by Site ID - this will help ensure that calculations 
  # are only done for each site 
  group_by(Site_ID) %>%
  # this code calculates the time at each subslope, and includes the end of transet 
  mutate(SubDuration = ifelse(Sub != lead(Sub) | lead(Notes == "End"), 
                           lead(Time) - Time, 
                           0)) %>%
  ungroup()

## repeat for slope 
Slope_Duration_data <- ROVSubSlope %>%
  ## arrange 
  arrange(Site_ID, Time) %>%
  ## group the data by Site ID - this will help ensure that calculations 
  # are only done for each site 
  group_by(Site_ID) %>%
  # this code calculates the time at each subslope, and includes the end of transet 
  mutate(SlopeDuration = ifelse(Slope != lead(Slope) | lead(Notes == "End"), 
                              lead(Time) - Time, 
                              0)) %>%
  ungroup()

## now we need to calculate the total duration at each site by using 
# the first sub_slope value and the notes == "end" point 
Sub_Duration_data <- Sub_Duration_data %>%
  group_by(Site_ID) %>%
mutate(Total_Duration = ifelse(Notes == "End", 
                               last(Time) - first(Time), 
                               NA_real_)) %>%
group_by(Site_ID) %>%
  mutate(Total_Duration = last(Total_Duration)) %>%
  ungroup() %>%
  mutate(Proportion = SubDuration / Total_Duration)

Slope_Duration_data <- Slope_Duration_data %>%
  group_by(Site_ID) %>%
  mutate(Total_Duration = ifelse(Notes == "End", 
                                 last(Time) - first(Time), 
                                 NA_real_)) %>%
  group_by(Site_ID) %>%
  mutate(Total_Duration = last(Total_Duration)) %>%
  ungroup() %>%
  mutate(Proportion = SlopeDuration / Total_Duration)

## now lets clean the dataset by removing the Notes == "End" column and the 
## Column named notes 
Sub_Duration_data <- filter(Sub_Duration_data, Notes!='End') 
Sub_Duration_data <-  select(Sub_Duration_data, -c(Frame, Depth, Time, Slope,
                                           SubDuration, Total_Duration, Notes, Sub_Slope))
Slope_Duration_data <- filter(Slope_Duration_data, Notes!='End') 
Slope_Duration_data <-  select(Slope_Duration_data, -c(Frame, Depth, Time, Sub,
                                                   SlopeDuration, Total_Duration, Notes, Sub_Slope))
## lets pool the data 
Sub_wide_duration <- Sub_Duration_data %>%
  group_by(Site_ID, Sub) %>%
  summarize(Proportion = sum(Proportion)) %>%
  pivot_wider(names_from = Sub, values_from = Proportion)%>%
  replace(is.na(.), 0)

Slope_Wide_duration <- Slope_Duration_data %>%
  group_by(Site_ID, Slope) %>%
  summarize(Proportion = sum(Proportion)) %>%
  pivot_wider(names_from = Slope, values_from = Proportion) %>%
  replace(is.na(.), 0)
View(Slope_Wide_duration)

## we might have not needed to do the prior code. Lets try something 
Sub_Duration_data <- Sub_Duration_data %>%
  mutate(Percent = round(Proportion * 100))

Slope_Duration_data <- Slope_Duration_data %>%
  mutate(Percent = round(Proportion * 100))

##Slope_Duration_data <- filter(Slope_Duration_data, Percent !=0) 

## lets change the names to make the visual look better 
# Mapping values
sub_mapping <- c("B" = "Boulder", "C" = "Cobble", "M" = "Mud", "R" = "Bed Rock", "S" = "Sand", "P" = "Pepple")
Sub_Duration_data <- Sub_Duration_data %>% 
  mutate(Sub = recode(Sub, !!!sub_mapping))



slope_mapping <-c("0" = "0-30", "30" = "30-60", "60" = "60-90", "90" = "90")
Slope_Duration_data <- Slope_Duration_data %>% 
  mutate(Slope = recode(Slope, !!!slope_mapping))
## Particular site 

## this needs some help - need to figure out why they aren't adding up to 100 for the slope- 

NS05_Sub <- Sub_Duration_data %>% 
  select(Sub_Duration_data, Site_ID == "NS05")

NS05_Slope <- subset(Slope_Duration_data, Site_ID == "NS05")
## create the pie chart 
ggplot(NS05_Slope, aes(x = "", y = Percent, fill = Slope)) +
  geom_col(color = "black") +
  geom_text(aes(label = Percent),
        position = position_stack(vjust = 0.5)) +
  ## creates a title 
  labs(title = paste( "Substrate types - NS05")) +
  ## creates the pie chart
  coord_polar(theta = "y")
################################################################################################################################

## Add Field of View measurements in siteinfo 
FOV<-read.csv("odata/FOV.csv")

## for sites that do not have a calculated field of view as the lasers where not positioned properly, 
# lets fill in the NA's with the column average 
colnames(FOV)
FOVmean <- FOV %>% 
          mutate_at(vars(Width, SD, SE, Height, Area, Volume), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
FOVvolume <- FOVmean %>% 
              select(Site_ID, Volume)
## now lets add the volume surveyed into site ID 
siteinfo <- merge(siteinfo, FOVvolume, by = "Site_ID")

#########################################################################################
## Load in Depth and Temperature datasheet from Star oddi 

DT <- read.csv("odata/StarDT.csv")
colnames(DT)
colnames(DT)[which(names(DT) == "Temp..C.")] <- "Temp"

## lets remove all values with depths > 10m to accommodate the thermocline 

DT <- DT %>% 
    filter(! Depth < 10) %>% 
filter(! Activity == "Ascend") %>% 
  filter(! Site_ID == "")



## Now lets calculate the average temperature for the descend to get 
#the average temperature of the water column 
unique(DT$Activity)

average_temp_descend <- DT %>%
  ## there is spaces in some of the activity types - this subsets the dataset
  filter(Activity %in% c("Descend ","Descend")) %>%
  group_by(Site_ID) %>%
  summarise(avg_temp = mean(Temp, na.rm = TRUE))

# the average depth while the ROV was on the bottom 
average_depth_bottom <- DT %>%
  filter(Activity %in% c("Bottom", "Bottom ")) %>%
  group_by(Site_ID) %>%
  summarise(avg_depth = mean(Depth, na.rm = TRUE))

siteinfo <- merge(siteinfo, average_temp_descend, by ="Site_ID")
siteinfo <- merge(siteinfo, average_depth_bottom, by = "Site_ID")
