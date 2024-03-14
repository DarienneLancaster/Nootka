# By: Hutton Noth 
# Date: Fall 2023

####  Load in Packages ####
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

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")


#### Create siteinfo Data frame #### 

load("nootkadata.Rdata")
siteinfo <- unique(nootkadata[, c("Site_ID", "Date", "Temp", "Lat_Decimal", "Long_Decimal")])


#### Load  & Clean Event Measure Annotation Data #### 

# skipping the first 4 rows of data / removing empty rows (strings)
#ROV<-read.csv("odata/ROVcomplete.csv", skip=4, stringsAsFactors = FALSE)

ROV <-read.csv("odata/NootkaROV_20240301_finalDL.csv",  skip=4, stringsAsFactors = FALSE)
# remove unnecessary columns 
ROV = select(ROV, -4:-21)
ROV = select(ROV, -"Code")

# rename columns 
colnames(ROV)[which(names(ROV) == "Time..mins.")] <- "Time"
colnames(ROV)[which(names(ROV) == "Depth.1")] <- "Depth"

# create a column for Site_ID by pulling the first 4 letters from filename 
ROV$Site_ID <- substr(ROV$Filename, 1, 4)

# replace blanks with unknown 
ROV$Species[ROV$Species== ""] <- "unknown" 
ROV$Genus[ROV$Genus== ""] <- "unknown"
ROV$Family[ROV$Family== ""] <- "unknown"
ROV$Site_ID <- substr(ROV$Filename, 1, 4)

# create a new column with species full name 
ROV<- ROV%>%
  mutate(FullName = paste(Family, Genus, Species, sep = " "))








############## Site information ###########################
#### Subarea for each site #### 

subarea <-read.csv("odata/Sitebysubarea.csv")
colnames(subarea)[which(names(subarea) == "Site.ID")] <- "Site_ID"
new_row <- data.frame(Site_ID = "NS29", Subarea = "Subarea 25-6")
subarea <- rbind(subarea, new_row)

# add subarea to siteinfo 
siteinfo <- merge(siteinfo, subarea, by = "Site_ID", all.x = TRUE)


#### Substrate and Slope - Proportion at each site #### 

# subset subslope information 
SubSlope <- ROV %>%
  filter(Activity=="Attracted" | Notes == "End") %>%
  select(Site_ID, Time, Depth, Sub_Slope, Notes)

# create a new column with just substrate information  
SubSlope$Sub <- substr(SubSlope$Sub_Slope, 1, 1)

# rename all sand and mud to soft and all other substrate to be labeled as hard substrate 
SubSlope <- SubSlope %>%
  mutate(Sub = ifelse(Sub %in% c("S", "M"), "soft",
                      ifelse(Sub %in% c("C", "R", "P", "B"), "hard", Sub)))

# create new dataset called duration time 
SubDuration <- SubSlope %>% 
  arrange(Site_ID, Time) %>% 
  select(Site_ID, Time, Notes, Sub) %>%
  group_by(Site_ID) %>%
  # calculate the duration at each substrate type 
  mutate(SubDuration = Time - lead(Time)) %>% ungroup()
# make values positive 
SubDuration$SubDuration <- abs(SubDuration$SubDuration)

# now calculate the total time at each site
SubDuration <- SubDuration %>%
  group_by(Site_ID) %>%
  mutate(SiteDuration = ifelse(Notes == "End", 
                               last(Time) - first(Time), 
                               NA_real_)) %>%
  group_by(Site_ID) %>%
  mutate(SiteDuration = last(SiteDuration)) %>%
  ungroup() %>%
  # add a column called proportion that ca
  mutate(Proportion = SubDuration / SiteDuration)

# clean up data 
SubDuration <- SubDuration %>% select(- Notes) %>% 
  filter(!is.na(SubDuration)) %>%
  mutate(Percent = round(Proportion * 100))
## this code combined together Sub of a certain type so we can see the dominate slope
SubDuration <- SubDuration %>%
  group_by(Site_ID, Sub, SiteDuration) %>%
  summarize(
    Time = sum(Time),
    SubDuration = sum(SubDuration),
    SubProportion = sum(Proportion),
    SubPercent = sum(Percent),
    .groups = "drop"  
  )

DominateSub <- SubDuration %>%
  group_by(Site_ID) %>%
  top_n(1, SubPercent) %>%
  ungroup()

# Slope calculations 
SubSlope$Slope <- substr(SubSlope$Sub_Slope, 3, 4)
# Slope Duration 
SlopeDuration <- SubSlope %>% 
  arrange(Site_ID, Time) %>% 
  select(Site_ID, Time, Notes, Slope) %>%
  group_by(Site_ID) %>%
  # calculate the duration at each substrate type 
  mutate(SlopeDuration = Time - lead(Time)) %>% ungroup()
# make values positive 
SlopeDuration$SlopeDuration <- abs(SlopeDuration$SlopeDuration)

# now we need to calculate the total time at each site
SlopeDuration <- SlopeDuration %>%
  group_by(Site_ID) %>%
  mutate(SiteDuration = ifelse(Notes == "End", 
                               last(Time) - first(Time), 
                               NA_real_)) %>%
  group_by(Site_ID) %>%
  mutate(SiteDuration = last(SiteDuration)) %>%
  ungroup() %>%
  # add a column called proportion that ca
  mutate(Proportion = SlopeDuration / SiteDuration)

# clean up data 
SlopeDuration <- SlopeDuration %>% select(- Notes) %>% 
  filter(!is.na(SlopeDuration)) %>% 
  mutate(Percent = round(Proportion * 100))

## this code combined together Slopes of a certain type so we can see the dominate slope
SlopeDuration <- SlopeDuration %>%
  group_by(Site_ID, Slope, SiteDuration) %>%
  summarize(
    Time = sum(Time),
    SlopeDuration = sum(SlopeDuration),
    SlopeProportion = sum(Proportion),
    SlopePercent = sum(Percent),
    .groups = "drop"  
  )
DominateSlope <- SlopeDuration %>%
  group_by(Site_ID) %>%
  top_n(1, SlopePercent) %>%
  ungroup()

# add these to the siteinfo data frame 
DominateSub$TopSub <- DominateSub$Sub
DomSub <- DominateSub %>% select("Site_ID", "TopSub", "SubPercent")

DominateSlope$TopSlope <- DominateSlope$Slope
DomSlope <- DominateSlope %>% select("Site_ID", "TopSlope", "SlopePercent")

siteinfo <- merge(siteinfo, DomSub, by = "Site_ID", all.x = TRUE)
siteinfo <- merge(siteinfo, DomSlope, by = "Site_ID", all.x = TRUE)


#### Field of View - Volume of water surveyed #### 
# load in data 
FOV<-read.csv("odata/FOV.csv")
#when I load in FOV file it gives weird name for Site_ID
#FOV<-FOV%>%
 # rename(Site_ID=ï..Site_ID)

# fill in sites with no calculate FOV with the average 
FOVmean <- FOV %>% 
  mutate_at(vars(Width, SD, SE, Height, Area, Volume), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

FOVvolume <- FOVmean %>% select(Site_ID, Volume)

## now lets add the volume surveyed into site ID 
siteinfo <- merge(siteinfo, FOVvolume, by = "Site_ID", all.x = TRUE)


#### Depth and Temperature #### 

# load in depth and temperature data 
DT <- read.csv("odata/StarDT.csv")
colnames(DT)[which(names(DT) == "Temp..C.")] <- "Temp"
#colnames(DT)[which(names(DT) == "ï..Number")] <- "Number"

# remove depths < 10m, ascending data, and blank data 
DT <- DT %>% 
  filter(! Depth < 10) %>% 
  filter(! Activity == "Ascend") %>% 
  filter(! Site_ID == "")

# calculate the average temperature in the water column 
average_temp_descend <- DT %>%
  ## there is spaces in some of the activity types - this subsets the dataset
  filter(Activity %in% c("Descend ","Descend")) %>%
  group_by(Site_ID) %>%
  summarise(avg_temp = mean(Temp, na.rm = TRUE))

# calculate the average depth while the ROV was on the bottom 
average_depth_bottom <- DT %>%
  filter(Activity %in% c("Bottom", "Bottom ")) %>%
  group_by(Site_ID) %>%
  summarise(avg_depth = mean(Depth, na.rm = TRUE))


siteinfo <- merge(siteinfo, average_temp_descend, by ="Site_ID", all.x = TRUE)
siteinfo <- merge(siteinfo, average_depth_bottom, by = "Site_ID", all.x = TRUE)

tempdepth <- select(siteinfo, "Site_ID", "avg_temp", "avg_depth")

#### FUNCTIONS - ABUNDANCE / SPECIES RICHNESS ####  

### Calculate Abundance and Species Richness of all species ### 

# subset dataframe to only be fish
ROVFish<- ROV%>%
  filter(Activity!= "Attracted" , Number!= "NA")

# create dataset that has only Site_ID and FullName 
sitespecies <- unique(ROVFish[, c("Site_ID", "FullName")])

# function to get abundance of each species per site 
get.abundance <- function(xx, ss){ 
  keep <- which(ROVFish$Site_ID == xx & ROVFish$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(ROVFish$Number[keep]))}

# apply function 
sitespecies$Abundance <- mapply(get.abundance, xx = sitespecies$Site_ID, ss = sitespecies$FullName)

# reshape the dataframe to make it wide 
sitespecies <- reshape(sitespecies, v.names = "Abundance", idvar = "Site_ID", timevar = "FullName", direction = "wide")

# fill in any NA's as zero
sitespecies[is.na(sitespecies)] <- 0

# function to calculate total abundance 
TotalAbundance <- function(x) {apply(sitespecies[, 2:22], 1, sum)}
# function to apply it to sitespecies 
sitespecies$TotalAbundance <- mapply(TotalAbundance, x = 2)

# function to get species richness per site 
SpeciesRichness <- function(x) {apply(sitespecies[, 2:22 ]> 0, 1, sum)}

# function to apply it to sitespecies 
sitespecies$SpeciesRichness <- mapply(SpeciesRichness, x = 2)

# add these to siteinfo 
subsitespecies <- select(sitespecies, c("Site_ID", "TotalAbundance", "SpeciesRichness"))
siteinfo <- merge(siteinfo, subsitespecies, by = "Site_ID", all.x = TRUE)
siteinfo <- siteinfo %>% mutate(SpeciesRichness = ifelse(is.na(SpeciesRichness), 0, SpeciesRichness),
                                TotalAbundance = ifelse(is.na(TotalAbundance), 0, TotalAbundance))
#### Calculate Rockfish Abundance and Species Richness #### 

# subset the the ROVFish data to only include sebastes 
Sebastes <-  ROVFish%>% filter(Genus == "Sebastes")

# create a dataset with only relevant data 
sitesebastes <- unique(Sebastes[, c("Site_ID", "FullName")])

# function to get abundance of each species per site 
get.sebastes.abundance <- function(xx, ss){ 
  keep <- which(Sebastes$Site_ID == xx & Sebastes$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(Sebastes$Number[keep]))}

# apply function 
sitesebastes$Abundance <- mapply(get.sebastes.abundance, 
                                 xx = sitesebastes$Site_ID, ss = sitesebastes$FullName)
# reshape the dataframe to make it wide 
sitesebastes <- reshape(sitesebastes, v.names = "Abundance", idvar = "Site_ID", timevar = "FullName", direction = "wide")

# fill in any NA's as zero
sitesebastes[is.na(sitesebastes)] <- 0

# calculate the abundance of rockfish at each site
sebastesabundance <- function(x) {apply(sitesebastes[, 2:10], 1, sum)}

# apply the function 
sitesebastes$RFAbundance <- mapply(sebastesabundance, x = 2)

# calculate the species richness of rockfish per site 
RFSpeciesRichness <- function(x) {apply(sitesebastes[, 2:10 ]> 0, 1, sum)}

# apply the function 
sitesebastes$RFSpeciesRichness <- mapply(RFSpeciesRichness, x = 2)

## add to siteinfo 
RF <- select(sitesebastes, c("Site_ID", "RFAbundance", "RFSpeciesRichness"))
siteinfo <- merge(siteinfo, RF, by = "Site_ID", all.x = TRUE)
siteinfo <- siteinfo %>% mutate(RFAbundance = ifelse(is.na(RFAbundance), 0, RFAbundance),
                                RFSpeciesRichness = ifelse(is.na(RFSpeciesRichness), 0, RFSpeciesRichness))

#### Calculate Fish School Abundance and Number #### 

# filter out non-fishschool  and blank data
fishschool <- ROV %>%
  filter(Number > 10, Number!= "NA") 

# code to calculate fish schools 
summary_fishschool <- fishschool %>%
  group_by(Site_ID, Notes) %>%
  summarise(Total_Number = sum(Number, na.rm = TRUE), .groups = "drop")

site_fishschool <- summary_fishschool %>%
  group_by(Site_ID) %>%
  summarise(total_number_schoolingfish = sum(Total_Number),
            number_FS = n_distinct(Notes))

siteinfo <- merge(siteinfo, site_fishschool, by = "Site_ID", all.x = TRUE)

## now lets do it for each type of fish school 
summary_fishschool <- summary_fishschool %>%
  mutate(fishschool_type = substr(Notes, 1, 2))

summary_fishschool_summary <- summary_fishschool %>%
  group_by(Site_ID, fishschool_type) %>%
  summarise(num_FS = n_distinct(Notes),
            sum_SF = sum(Total_Number),
            .groups = "drop")

summary_fishschool_pivoted <- summary_fishschool_summary %>%
  pivot_wider(names_from = fishschool_type,
              values_from = c(num_FS, sum_SF),
              names_prefix = "--",
              names_sep = "")
summary_fishschool_pivoted[is.na(summary_fishschool_pivoted)] <- 0
siteinfo <- merge(siteinfo, summary_fishschool_pivoted, by = "Site_ID", all.x = TRUE)

#### Calculate Non-Schooling Fish Abundance and Species Richness #### 

# filter out any groups of fish over 10 
nonschooling <- ROV %>% 
              filter(Number < 10, Number!= "NA") 

# create outline to apply function 
sitenonschooling <- unique(nonschooling[, c("Site_ID", "FullName")])

# create a function 
get.nonschooling <- function(xx, ss){ 
  keep <- which(nonschooling$Site_ID == xx & nonschooling$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(nonschooling$Number[keep]))
}

# apply function 
sitenonschooling$Abundance <- mapply(get.nonschooling, xx = sitenonschooling$Site_ID, ss = sitenonschooling$FullName)

# reshape the dataframe 
NSwide <- reshape(sitenonschooling, v.names = "Abundance", idvar = "Site_ID", timevar = "FullName", direction = "wide")
NSwide[is.na(NSwide)] <- 0

# function to calculate abundance for non-schooling fish 
nonschoolingabundance <- function(x) {
  apply(NSwide[, 2:8], 1, sum)
}

NSwide$AbundanceNonSchooling <- mapply(nonschoolingabundance, x = 2)

# function to calculate species richness for non-schooling fish 
nonschoolspeciesrichness <- function(x) { 
  apply(NSwide[, 2:8]> 0, 1, sum)
}
NSwide$SRNonSchooling <- mapply(nonschoolspeciesrichness, x = 2)

# subset dataframe 
nonschooling <- select(NSwide, c("Site_ID", "AbundanceNonSchooling", "SRNonSchooling"))

# merge this to siteinfo 
siteinfo <- merge(siteinfo, nonschooling, by = "Site_ID", na.rm = TRUE, all = TRUE)


#### Calculate Relative Frequency of each Species ##### 

# Subset data 
species <- unique(ROV[, c("Family", "Genus", "Species", "FullName")])
species[is.na(species)] <- 0
# function to count the number of each species using the full name 
speciescount <- function(xx){ 
  keep <- which(ROV$FullName == xx)
  if (length(keep) == 0) return(0)
  return(sum(ROV$Number[keep]))
}
species$count <- mapply(speciescount, xx = species$FullName )

species <- species %>%filter(count!= "NA")


# calculate the relative frequency 
species1 <- species %>% group_by(FullName) %>% 
  summarise(total_count = sum(count), .groups = "drop" ) %>% 
  mutate(frequency = total_count / sum(total_count) ) 

# plot 
species1 %>%
  mutate(name = fct_reorder(FullName, desc(frequency))) %>%
  ggplot( aes(x= reorder(FullName, - frequency), y=frequency)) +
  geom_bar(stat="identity", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

## lets remove unknown unknown unknown to get a better understanding of known species  

result_table <- species1 %>%
  mutate(name = forcats::fct_reorder(FullName, desc(total_count))) %>%
  arrange(desc(total_count)) %>%
  select(FullName, total_count = total_count)

view(result_table)

flextable(result_table) 

# rockfish only
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

flextable(rockfish)

result_table1 <- rockfish %>%
  mutate(name = forcats::fct_reorder(FullName, desc(total_count))) %>%
  arrange(desc(total_count)) %>%
  select(FullName, total_count = total_count)
flextable(result_table1)



#### Subarea level - abundance and species richness #### 
# figure out the species richness and abundance by subarea 
ROVFish <- merge(ROVFish, subarea, by = "Site_ID", all.x = TRUE) 

# count the number of sites within a subarea 
subarea_counts <- subarea %>% 
  group_by(Subarea) %>% 
  summarise(TotalSites = n_distinct(Site_ID))

# create a species by subarea dataframe 
subareaspecies <- unique(ROVFish[, c("Subarea", "FullName")])


# find species abundance per subarea 
get.abundance <- function(xx, ss){ 
  keep <- which(ROVFish$Subarea == xx & ROVFish$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(ROVFish$Number[keep]))}
subareaspecies$Abundance <- mapply(get.abundance, xx = subareaspecies$Subarea, ss = subareaspecies$FullName)
speciesinsubarea <- subareaspecies %>%  select(Subarea, FullName, Abundance)
subareaspecies <- reshape(subareaspecies, v.names = "Abundance", idvar = "Subarea", timevar = "FullName", direction = "wide")
subareaspecies[is.na(subareaspecies)] <- 0
TotalAbundance <- function(x) {apply(subareaspecies[, 2:22], 1, sum)}
subareaspecies$TotalAbundance <- mapply(TotalAbundance, x = 2)
sum(subareaspecies$TotalAbundance) # we counted 13658 

# find species richness per subarea 
SpeciesRichness <- function(x) {apply(subareaspecies[, 2:22 ]> 0, 1, sum)}
subareaspecies$SpeciesRichness <- mapply(SpeciesRichness, x = 2)
Sebastes <-  ROVFish%>% filter(Genus == "Sebastes")
subarea1 <- select(subareaspecies, c("Subarea", "TotalAbundance", "SpeciesRichness"))

# find rockfish abundance per subarea 
subareasebastes <- unique(Sebastes[, c("Subarea", "FullName")])
get.sebastes.abundance <- function(xx, ss){ 
  keep <- which(Sebastes$Subarea == xx & Sebastes$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(Sebastes$Number[keep]))}
subareasebastes$Abundance <- mapply(get.sebastes.abundance, 
                                 xx = subareasebastes$Subarea, ss = subareasebastes$FullName)
subareasebastes <- reshape(subareasebastes, v.names = "Abundance", idvar = "Subarea", timevar = "FullName", direction = "wide")
subareasebastes[is.na(subareasebastes)] <- 0
sebastesabundance <- function(x) {apply(subareasebastes[, 2:11], 1, sum)}
subareasebastes$RFAbundance <- mapply(sebastesabundance, x = 2)

# find rockfish species richness per subarea 
RFSpeciesRichness <- function(x) {apply(subareasebastes[, 2:11 ]> 0, 1, sum)}
subareasebastes$RFSpeciesRichness <- mapply(RFSpeciesRichness, x = 2)
RF <- select(subareasebastes, c("Subarea", "RFAbundance", "RFSpeciesRichness"))

Subareadata <- merge(subarea_counts, subarea1, by = "Subarea", all.x = TRUE)
Subareadata <- merge(Subareadata, RF, by = "Subarea", all.x = TRUE)
  View(Subareadata)
  
  flextable(subarea_table, col_keys = c("Subarea", "TotalSites"))
  flextable(subarea_table)
  
# calculate total sites in each subarea 
  subarea_table <- Subareadata %>%
  mutate(name = forcats::fct_reorder(Subarea, desc(TotalSites))) %>%
  arrange(desc(TotalSites)) %>% 
  select(- name)
flextable(subarea_table)

# lets make a list of species and counts within each subarea
  
  # Subarea 6
  subarea6 <- ROVFish %>% 
    filter(Subarea == "Subarea 25-6") 
  subspecies6 <-  unique(subarea6[, c("Family", "Genus","Species","FullName")])
  speciescount6 <- function(xx){ 
    keep <- which(subarea6$FullName == xx)
    if (length(keep) == 0) return(0)
    return(sum(subarea6$Number[keep]))
  }
  subspecies6$count <- mapply(speciescount6, xx = subspecies6$FullName )
  
  subspecies6 <- subspecies6 %>%
    mutate(name = forcats::fct_reorder(FullName, desc(count))) %>%
    arrange(desc(count)) %>% 
    select(- Family, - Genus, -Species)
  flextable(subspecies6, col_keys = c("FullName", "count"))
  
  # Subarea 8 
  subarea8 <- ROVFish %>% 
    filter(Subarea == "Subarea 25-8") 
  subspecies8 <-  unique(subarea8[, c("Family", "Genus","Species","FullName")])
  speciescount8 <- function(xx){ 
    keep <- which(subarea8$FullName == xx)
    if (length(keep) == 0) return(0)
    return(sum(subarea8$Number[keep]))
  }
  subspecies8$count <- mapply(speciescount8, xx = subspecies8$FullName )
  
  subspecies8 <- subspecies8 %>%
    mutate(name = forcats::fct_reorder(FullName, desc(count))) %>%
    arrange(desc(count)) %>% 
    select(- Family, - Genus, -Species)
  flextable(subspecies8, col_keys = c("FullName", "count"))
  
  # Subarea 4
  subarea4 <- ROVFish %>% 
    filter(Subarea == "Subarea 25-4") 
  subspecies4 <-  unique(subarea4[, c("Family", "Genus","Species","FullName")])
  speciescount4 <- function(xx){ 
    keep <- which(subarea4$FullName == xx)
    if (length(keep) == 0) return(0)
    return(sum(subarea4$Number[keep]))
  }
  subspecies4$count <- mapply(speciescount4, xx = subspecies4$FullName )
  subspecies4 <- subspecies4 %>%
    mutate(name = forcats::fct_reorder(FullName, desc(count))) %>%
    arrange(desc(count)) %>% 
    select(- Family, - Genus, -Species)
  flextable(subspecies4, col_keys = c("FullName", "count"))
  
  # Subarea 15 
  subarea15 <- ROVFish %>% 
    filter(Subarea == "Subarea 25-15") 
  subspecies15 <-  unique(subarea15[, c("Family", "Genus","Species","FullName")])
  speciescount15 <- function(xx){ 
    keep <- which(subarea15$FullName == xx)
    if (length(keep) == 0) return(0)
    return(sum(subarea15$Number[keep]))
  }
  subspecies15$count <- mapply(speciescount15, xx = subspecies15$FullName )
  subspecies15 <- subspecies15 %>%
    mutate(name = forcats::fct_reorder(FullName, desc(count))) %>%
    arrange(desc(count)) %>% 
    select(- Family, - Genus, -Species)
  flextable(subspecies15, col_keys = c("FullName", "count"))
  
  # Subarea 5
  subarea5 <- ROVFish %>% 
    filter(Subarea == "Subarea 25-5") 
  subspecies5 <-  unique(subarea5[, c("Family", "Genus","Species","FullName")])
  speciescount5 <- function(xx){ 
    keep <- which(subarea5$FullName == xx)
    if (length(keep) == 0) return(0)
    return(sum(subarea5$Number[keep]))
  }
  subspecies5$count <- mapply(speciescount5, xx = subspecies5$FullName )
  subspecies5 <- subspecies5 %>%
    mutate(name = forcats::fct_reorder(FullName, desc(count))) %>%
    arrange(desc(count)) %>% 
    select(- Family, - Genus, -Species)
  flextable(subspecies5, col_keys = c("FullName", "count"))


  
############## Bin Information ############################
## try to create 5 bins of time per site. 

## remove the times that the ROv gets pulled from the time from aanotations
ROV$Time <- as.numeric(ROV$Time)
ROV$Time <- as.numeric(ROV$Time)

pulled_indices <- which(ROV$Notes == "pulled")
resume_indices <- which(ROV$Notes == "resume")

for (i in 1:length(pulled_indices)) {
  # figure out what Site_ID have pulls 
  site_id <- ROV$Site_ID[pulled_indices[i]]
  
  # match them with the resume 
  resume_index <- resume_indices[resume_indices > pulled_indices[i]][1]
  
  # calculate the duration it was pulled
  duration <- ROV$Time[resume_index] - ROV$Time[pulled_indices[i]]
  
  # make time resumed = time pulled
  ROV$Time[resume_index] <- ROV$Time[pulled_indices[i]]
  
  # subtract the duration pulled from the following time after the resume
  following_time <- which(ROV$Site_ID == site_id & ROV$Time > ROV$Time[resume_index])
  ROV$Time[following_time] <- ROV$Time[following_time] - duration
}


# Measure the total duration for each of the sites
durationdata <- ROV %>%
  group_by(Site_ID) %>%
  mutate(Total_Duration = max(Time) - min(Time))

# the length of the bin will be the total duration / 5
durationdata <- durationdata %>%
  mutate(Bin_length = Total_Duration / 5)

## place each observation within a bin based on time
create_bins <- function(data) {
  data %>%
    group_by(Site_ID) %>%
    mutate(
      Min_Time = min(Time),
      Bin = case_when(
        between(Time, Min_Time, Min_Time + Bin_length) ~ 1,
        between(Time, Min_Time + Bin_length, Min_Time + 2 * Bin_length) ~ 2,
        between(Time, Min_Time + 2 * Bin_length, Min_Time + 3 * Bin_length) ~ 3,
        between(Time, Min_Time + 3 * Bin_length, Min_Time + 4 * Bin_length) ~ 4,
        TRUE ~ 5
      )
    ) %>%
    select(-Min_Time)
}

# Apply the function 
durationdata <- create_bins(durationdata)


##### Run calculations to get Bin level data ##### 
# Create bin level dataframe 
bininfo <- siteinfo %>% select("Site_ID")
bininfo2 <- siteinfo %>% select("Site_ID", "Date", "Temp", "Lat_Decimal", "Long_Decimal", "avg_temp", "avg_depth", "Volume")
bininfo2$Volume <- bininfo2$Volume / 5
bininfo <- bininfo %>%
  # expand each Site_ID to have 5 different BinID 
  expand(Site_ID, BinID = 1:5) %>%
  # name the BinID by pasting Site_ID_BinID (1-5)
  mutate(BinID = paste0(Site_ID, "_", BinID)) %>%
  # add all other site data 
  left_join(bininfo2, by = "Site_ID")

# subset dataframe to only be fish
BinFish<- durationdata %>%
  filter(Number!= "NA")

# create a column that merges Site_ID and bin number 
BinFish$BinID <- paste(BinFish$Site_ID, BinFish$Bin, sep = "_")

# create dataset that has only BinID and FullName 
binspecies <- unique(BinFish[, c("BinID", "FullName")])

# function to get abundance of each species per site 
get.abundance <- function(xx, ss){ 
  keep <- which(BinFish$BinID == xx & BinFish$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(BinFish$Number[keep]))}

# apply function 
binspecies$Abundance <- mapply(get.abundance, xx = binspecies$BinID, ss = binspecies$FullName)

# reshape the dataframe to make it wide 
binspecieswide <- binspecies %>%
  pivot_wider(names_from = FullName, values_from = Abundance)
# fill in any NA's as zero
binspecieswide[is.na(binspecieswide)] <- 0

# function to calculate total abundance 
TotalAbundance <- function(x) {apply(binspecieswide[, 2:22], 1, sum)}
# function to apply it to binspecies 
binspecieswide$TotalAbundance <- mapply(TotalAbundance, x = 2)

# function to get species richness per site 
SpeciesRichness <- function(x) {apply(binspecieswide[, 2:22 ]> 0, 1, sum)}

# function to apply it to binspecies 
binspecieswide$SpeciesRichness <- mapply(SpeciesRichness, x = 2)

## add to bininfo
bin <- select(binspecieswide, c("BinID", "TotalAbundance", "SpeciesRichness"))
bin <- bin %>% replace(is.na(.), 0)
bininfo <- merge(bininfo, bin, by = "BinID", all.x = TRUE)

#### Bin level calculate Rockfish Abundance and Species Richness #### 

# subset the the BinFish data to only include sebastes 
binsebastes <-  BinFish%>% filter(Genus == "Sebastes")

# create a dataset with only relevant data 
binsebastes <- unique(binsebastes[, c("BinID", "FullName")])

# function to get abundance of each species per site 
get.bin.sebastes.abundance <- function(xx, ss){ 
  keep <- which(binsebastes$BinID == xx & binsebastes$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(Sebastes$Number[keep]))}

# apply function 
binsebastes$Abundance <- mapply(get.bin.sebastes.abundance, 
                                 xx = binsebastes$BinID, ss = binsebastes$FullName)
# reshape the dataframe to make it wide 
binsebastes <- binsebastes %>%
  pivot_wider(names_from = FullName, values_from = Abundance)
# fill in any NA's as zero
binsebastes[is.na(binsebastes)] <- 0

# calculate the abundance of rockfish at each site
binsebastesabundance <- function(x) {apply(binsebastes[, 2:11], 1, sum)}

# apply the function 
binsebastes$RFAbundance <- mapply(binsebastesabundance, x = 2)

# calculate the species richness of rockfish per site 
RFSpeciesRichness <- function(x) {apply(binsebastes[, 2:11 ]> 0, 1, sum)}

# apply the function 
binsebastes$RFSpeciesRichness <- mapply(RFSpeciesRichness, x = 2)

## add to siteinfo 
binRF <- select(binsebastes, c("BinID", "RFAbundance", "RFSpeciesRichness"))
bininfo <- merge(bininfo, binRF, by = "BinID", all.x = TRUE)

#### Bin level Calculate Fish School Abundance and Number #### 

# filter out non-fishschool  and blank data
fishschoolbin <- BinFish %>%
  filter(Number > 10, Number!= "NA") 

# code to calculate fish schools 
Bin_sum_fishschool <- fishschoolbin %>%
  group_by(BinID, Notes) %>%
  summarise(Total_Number = sum(Number, na.rm = TRUE), .groups = "drop")

Bin_fishschool <- Bin_sum_fishschool %>%
  group_by(BinID) %>%
  summarise(total_number_schoolingfish = sum(Total_Number),
            number_FS = n_distinct(Notes))

bininfo <- merge(bininfo, Bin_fishschool, by = "BinID", all.x = TRUE)

## now lets do it for each type of fish school 
Bin_sum_fishschool <- Bin_sum_fishschool %>%
  mutate(fishschool_type = substr(Notes, 1, 2))

Bin_fishschool_summary <- summary_fishschool %>%
  group_by(Site_ID, fishschool_type) %>%
  summarise(num_FS = n_distinct(Notes),
            sum_SF = sum(Total_Number),
            .groups = "drop")

Bin_fishschool_pivoted <- Bin_fishschool_summary %>%
  pivot_wider(names_from = fishschool_type,
              values_from = c(num_FS, sum_SF),
              names_prefix = "--",
              names_sep = "")
Bin_fishschool_pivoted[is.na(Bin_fishschool_pivoted)] <- 0
siteinfo <- merge(siteinfo, Bin_fishschool_pivoted, by = "Site_ID", all.x = TRUE)

#### Calculate Non-Schooling Fish Abundance and Species Richness #### 

# filter out non-fishschool 
binfishschool <- BinFish%>%
  filter(Number > 10) 
binfishschools <- unique(binfishschool[, c("BinID", "FullName")])
# remove FS with number = 0 
get.bin.schoolingfish <- function(xx, ss){ 
  keep <- which(binfishschool$BinID == xx & binfishschool$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(binfishschool$Number[keep]))}
binfishschools$Abundance <- mapply(get.bin.schoolingfish, 
                                xx = binfishschools$BinID, ss = binfishschools$FullName)
unique(binfishschools$FullName)
# transform the data to wide 
binfishschoolswide <- binfishschools %>%
  pivot_wider(names_from = FullName, values_from = Abundance, values_fill = 0)

View(binfishschoolswide)
# calculate total number of schooling fish per bin
schoolingfish <- function(x) {apply(binfishschoolswide[, 2:9], 1, sum)}
# apply the function 
binfishschoolswide$SFCount <- mapply(schoolingfish, x = 2)

## Count number of fish schools 
numberFS <- function(x) { 
  apply(binfishschoolswide[, 2:9]> 0, 1, sum)
}

binfishschoolswide$NumberFS <- mapply(numberFS, x = 2)


#### Bin level Calculate Non-Schooling Fish Abundance and Species Richness #### 

# filter out any groups of fish over 10 
nonschooling <- BinFish %>% 
  filter( Number < 10)

# create outline to apply function 
binnonschooling <- unique(nonschooling[, c("BinID", "FullName")])

# create a function 
get.nonschooling <- function(xx, ss){ 
  keep <- which(nonschooling$BinID == xx & nonschooling$FullName == ss)
  if (length(keep) == 0) return(0)
  return(sum(nonschooling$Number[keep]))
}

# apply function 
binnonschooling$Abundance <- mapply(get.nonschooling, xx = binnonschooling$BinID, ss = binnonschooling$FullName)

# reshape the dataframe 
NSbinwide <- binnonschooling %>%
  pivot_wider(names_from = FullName, values_from = Abundance, values_fill = 0)
NSbinwide[is.na(NSbinwide)] <- 0
ncol(NSbinwide)

# function to calculate abundance for non-schooling fish 
nonschoolingabundance <- function(x) {
  apply(NSbinwide[, 2:24], 1, sum)
}

NSbinwide$AbundanceNonSchooling <- mapply(nonschoolingabundance, x = 2)

# function to calculate species richness for non-schooling fish 
nonschoolspeciesrichness <- function(x) { 
  apply(NSbinwide[, 2:24]> 0, 1, sum)
}
NSbinwide$SRNonSchooling <- mapply(nonschoolspeciesrichness, x = 2)

# subset dataframe 
nonschooling <- select(NSbinwide, c("BinID", "AbundanceNonSchooling", "SRNonSchooling"))

# merge this to siteinfo 
bininfo <- merge(bininfo, nonschooling, by = "BinID", na.rm = TRUE, all = TRUE)

# Save 
save(bininfo, file = "bininfo.RData")
write.csv(bininfo,"wdata/bininfo.csv")

save(siteinfo, file = "siteinfo.Rdata")

