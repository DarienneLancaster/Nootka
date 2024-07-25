#Fish Density Summary Stats (Total Count, Mean Density, SD Density)

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
lp("RColorBrewer")
display.brewer.all()

#### Create siteinfo Data frame #### 

load("wdata/nootkadata.Rdata")
siteinfo <- unique(nootkadata[, c("Site_ID", "Date", "Temp", "Lat_Decimal", "Long_Decimal")])


#### Load  & Clean Event Measure Annotation Data #### 


ROV <-read.csv("odata/NootkaROV_20240426_finalDL_NS42noGPSused.csv",  skip=4, stringsAsFactors = FALSE)
# remove unnecessary columns 
ROV = dplyr::select(ROV, -4:-21)
ROV = dplyr::select(ROV, -"Code")

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
  mutate(FullName = paste(Family, Genus, Species, sep = " "))%>%
  mutate(Latin = paste(Genus, Species, sep = " "))

#summary stats on ROV dataframe

AllF<-ROV%>%
  group_by(FullName)%>%
  summarize(Total_Fish = sum(Number, na.rm = TRUE))%>%
  arrange(desc(Total_Fish))

flextable(AllF)

#####################go back over how these are named and filtered (getting weird values in table that don't add up)
#create new column with species common names
ROV$Common<- ifelse(ROV$Species == "caurinus", "Copper rockfish",
                     ifelse(ROV$Species == "maliger", "Quillback rockfish",
                            ifelse(ROV$Species == "pinniger", "Canary rockfish",
                                   ifelse(ROV$Species == "miniatus", "Vermillion rockfish",
                                          ifelse(ROV$Species == "melanops", "Black rockfish",
                                                 ifelse(ROV$Species == "elongatus", "Lingcod",
                                                        ifelse(ROV$Species == "decagrammus", "Kelp greenling",
                                                               ifelse(ROV$Species == "emphaeus", "Puget Sound rockfish",
                                                                      ifelse(ROV$Species == "flavidus" & grepl("BP|SP|P", ROV$Notes), "Yellowtail rockfish Pelagic",
                                                                             ifelse(ROV$Species == "flavidus" & !grepl("BP|SP|P", ROV$Notes), "Yellowtail rockfish Benthic",
                                                                             ifelse(ROV$Species == "pictus", "Painted greenling",
                                                                                    ifelse(ROV$Species == "armatus", "Staghorn sculpin",
                                                                                           ifelse(ROV$Species == "entomelas", "Widow rockfish",
                                                                                                  ifelse(ROV$Species == "pallasii", "Pacific herring",
                                                                                                         ifelse(ROV$Species == "vacca", "Pile Perch",
                                                                                                                ifelse(ROV$Species == "colliei", "Spotted ratfish", 
                                                                                                                       ifelse(ROV$Species == "rhina", "Skate spp.", 
                                                                                                                              ifelse(ROV$Species == "bilineata", "Flatfish spp.", 
                                                                                                                                     ifelse(ROV$Species == "frenatus", "Kelp Perch", 
                                                                                                                                            ifelse(ROV$Species == "sordidus", "Flatfish spp.", 
                                                                                                                                                   ifelse(ROV$Species == "vetulus", "Flatfish spp.",
                                                                                                                                                          ifelse(ROV$Species == "unknown" & grepl("SB", ROV$Notes) & grepl("J", ROV$Stage), "YOY rockfish Benthic",
                                                                                                                                                                 ifelse(ROV$Species == "unknown" & grepl("SP", ROV$Notes) & grepl("J", ROV$Stage), "YOY rockfish Pelagic",
                                                                                                                                                                      ifelse(ROV$Species == "unknown" & grepl("BP|SP|P", ROV$Notes), "Unknown Pelagic",
                                                                                                                                                                            ifelse(ROV$Genus == "Sebastes" & ROV$Species == "unknown" & !grepl("BP|SP|P|SB|BB", ROV$Notes), "Sebastes spp.",
                                                                                                                                                                                   ifelse(ROV$Genus == "unknown" & ROV$Species == "unknown", "Unknown Benthic","Unknown"))))))))))))))))))))))))))
                                                                                                                                                                    



#### subset dataframe to only be fish####
ROVFish<- ROV%>%
  filter(Activity!= "Attracted" , Number!= "NA", Notes!= "Start")

# create dataset that has only Site_ID and FullName 
sitespecies <- unique(ROVFish[, c("Site_ID", "Common")])


####calculate total abundance of all fish by site####
# function to get abundance of each species per site 
get.abundance <- function(xx, ss){ 
  keep <- which(ROVFish$Site_ID == xx & ROVFish$Common == ss)
  if (length(keep) == 0) return(0)
  return(sum(ROVFish$Number[keep]))}

# apply function 
sitespecies$Abundance <- mapply(get.abundance, xx = sitespecies$Site_ID, ss = sitespecies$Common)

# reshape the dataframe to make it wide 
sitespecies <- reshape(sitespecies, v.names = "Abundance", idvar = "Site_ID", timevar = "Common", direction = "wide")

# fill in any NA's as zero
sitespecies[is.na(sitespecies)] <- 0

# function to calculate total abundance 
TotalAbundance <- function(x) {apply(sitespecies[, 2:23], 1, sum)}
# function to apply it to sitespecies 
sitespecies$TotalAbundance <- mapply(TotalAbundance, x = 2)

# function to get species richness per site 
SpeciesRichness <- function(x) {apply(sitespecies[, 2:23 ]> 0, 1, sum)}

# function to apply it to sitespecies 
sitespecies$SpeciesRichness <- mapply(SpeciesRichness, x = 2)

### calculate totals for benthic fish####

Benthic <- ROV %>% 
  filter(Number < 10, Number!= "NA", Notes != "P") %>%
  filter(grepl("Scorpaenidae|Hexagrammidae|Cottidae", Family) |
           grepl("Ratfish|Flatfish|Benthic|Skate", Common))


# create dataset that has only Site_ID and FullName 
sitebenthic <- unique(Benthic[, c("Site_ID", "Common")])

# function to get abundance of each species per site 
get.abundance <- function(xx, ss){ 
  keep <- which(Benthic$Site_ID == xx & Benthic$Common == ss)
  if (length(keep) == 0) return(0)
  return(sum(Benthic$Number[keep]))}

# apply function 
sitebenthic$Abundance <- mapply(get.abundance, xx = sitebenthic$Site_ID, ss = sitebenthic$Common)

# reshape the dataframe to make it wide 
sitebenthic <- reshape(sitebenthic, v.names = "Abundance", idvar = "Site_ID", timevar = "Common", direction = "wide")

# fill in any NA's as zero
sitebenthic[is.na(sitebenthic)] <- 0

# function to calculate total abundance 
TotalAbundance <- function(x) {apply(sitebenthic[, 2:15], 1, sum)}
# function to apply it to sitebenthic
sitebenthic$TA_AllBen <- mapply(TotalAbundance, x = 2)

##add to sitespecies dataframe

sitespecies <- sitespecies %>%
  left_join(sitebenthic %>% dplyr::select(Site_ID, TA_AllBen), by = "Site_ID")

### calculate totals for rocky reef fish ####

RRF <- ROV %>% 
  filter(Number < 10, Number!= "NA", Notes != "P") %>%
  filter(grepl("Scorpaenidae|Hexagrammidae|Cottidae", Family))


# create dataset that has only Site_ID and FullName 
siteRRF <- unique(RRF[, c("Site_ID", "Common")])

# function to get abundance of each species per site 
get.abundance <- function(xx, ss){ 
  keep <- which(RRF$Site_ID == xx & RRF$Common == ss)
  if (length(keep) == 0) return(0)
  return(sum(RRF$Number[keep]))}

# apply function 
siteRRF$Abundance <- mapply(get.abundance, xx = siteRRF$Site_ID, ss = siteRRF$Common)

# reshape the dataframe to make it wide 
siteRRF <- reshape(siteRRF, v.names = "Abundance", idvar = "Site_ID", timevar = "Common", direction = "wide")

# fill in any NA's as zero
siteRRF[is.na(siteRRF)] <- 0

# function to calculate total abundance 
TotalAbundance <- function(x) {apply(siteRRF[, 2:13], 1, sum)}
# function to apply it to siteRRF
siteRRF$TA_RRF <- mapply(TotalAbundance, x = 2)

##add to sitespecies dataframe

sitespecies <- sitespecies %>%
  left_join(siteRRF %>% dplyr::select(Site_ID, TA_RRF), by = "Site_ID")

### calculate totals for Other benthics (aka mudfish) ####

Oth_Ben <- ROV %>% 
  filter(Number < 10, Number!= "NA", Notes != "P") %>%
  filter(!grepl("Scorpaenidae|Hexagrammidae|Cottidae", Family))


# create dataset that has only Site_ID and FullName 
siteOth_Ben <- unique(Oth_Ben[, c("Site_ID", "Common")])

# function to get abundance of each species per site 
get.abundance <- function(xx, ss){ 
  keep <- which(Oth_Ben$Site_ID == xx & Oth_Ben$Common == ss)
  if (length(keep) == 0) return(0)
  return(sum(Oth_Ben$Number[keep]))}

# apply function 
siteOth_Ben$Abundance <- mapply(get.abundance, xx = siteOth_Ben$Site_ID, ss = siteOth_Ben$Common)

# reshape the dataframe to make it wide 
siteOth_Ben <- reshape(siteOth_Ben, v.names = "Abundance", idvar = "Site_ID", timevar = "Common", direction = "wide")

# fill in any NA's as zero
siteOth_Ben[is.na(siteOth_Ben)] <- 0

# function to calculate total abundance 
TotalAbundance <- function(x) {apply(siteOth_Ben[, 2:7], 1, sum)}
# function to apply it to siteOth_Ben
siteOth_Ben$TA_Oth_Ben <- mapply(TotalAbundance, x = 2)

##add to sitespecies dataframe

sitespecies <- sitespecies %>%
  left_join(siteOth_Ben %>% dplyr::select(Site_ID, TA_Oth_Ben), by = "Site_ID")




### calculate totals for schooling benthic####

Ben_S <- ROV %>% 
  filter(Number > 9, Number!= "NA", Notes != "P") %>%
  filter(grepl("Benthic", Common))


# create dataset that has only Site_ID and FullName 
siteBen_S <- unique(Ben_S[, c("Site_ID", "Common")])

# function to get abundance of each species per site 
get.abundance <- function(xx, ss){ 
  keep <- which(Ben_S$Site_ID == xx & Ben_S$Common == ss)
  if (length(keep) == 0) return(0)
  return(sum(Ben_S$Number[keep]))}

# apply function 
siteBen_S$Abundance <- mapply(get.abundance, xx = siteBen_S$Site_ID, ss = siteBen_S$Common)

# reshape the dataframe to make it wide 
siteBen_S <- reshape(siteBen_S, v.names = "Abundance", idvar = "Site_ID", timevar = "Common", direction = "wide")

# fill in any NA's as zero
siteBen_S[is.na(siteBen_S)] <- 0

# function to calculate total abundance 
TotalAbundance <- function(x) {apply(siteBen_S[, 2:3], 1, sum)}
# function to apply it to siteBen_S
siteBen_S$TA_Ben_S <- mapply(TotalAbundance, x = 2)

##add to sitespecies dataframe

sitespecies <- sitespecies %>%
  left_join(siteBen_S %>% dplyr::select(Site_ID, TA_Ben_S), by = "Site_ID")

### calculate totals for pelagics####

Pel_S <- ROV %>% 
  filter(Number > 9, Number!= "NA")%>%
  filter(grepl("BP|SP", Notes))


# create dataset that has only Site_ID and FullName 
sitePel_S <- unique(Pel_S[, c("Site_ID", "Common")])

# function to get abundance of each species per site 
get.abundance <- function(xx, ss){ 
  keep <- which(Pel_S$Site_ID == xx & Pel_S$Common == ss)
  if (length(keep) == 0) return(0)
  return(sum(Pel_S$Number[keep]))}

# apply function 
sitePel_S$Abundance <- mapply(get.abundance, xx = sitePel_S$Site_ID, ss = sitePel_S$Common)

# reshape the dataframe to make it wide 
sitePel_S <- reshape(sitePel_S, v.names = "Abundance", idvar = "Site_ID", timevar = "Common", direction = "wide")

# fill in any NA's as zero
sitePel_S[is.na(sitePel_S)] <- 0

# function to calculate total abundance 
TotalAbundance <- function(x) {apply(sitePel_S[, 2:7], 1, sum)}
# function to apply it to sitePel_S
sitePel_S$TA_Pel_S <- mapply(TotalAbundance, x = 2)

##add to sitespecies dataframe

sitespecies <- sitespecies %>%
  left_join(sitePel_S %>% dplyr::select(Site_ID, TA_Pel_S), by = "Site_ID")



####calculate total count, mean density, and mean SD####

# fill in any NA's as zero
sitespecies[is.na(sitespecies)] <- 0

#add FOV volume column to sitespecies
#### Field of View - Volume of water surveyed #### 
# load in data (new FOV file has total distance travelled rather than 100m transect length)
FOV<-read.csv("odata/FOV20240510.csv")
#when I load in FOV file it gives weird name for Site_ID
# FOV<-FOV%>%
#  rename(Site_ID=ï..Site_ID)

# fill in sites with no calculate FOV with the average 
FOVmean <- FOV %>% 
  mutate_at(vars(Width, SD, SE, Height, Area, Volume), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

FOVvolume <- FOVmean %>% dplyr::select(Site_ID, Volume)

## now lets add the volume surveyed into site ID 
sitespecies <- merge(sitespecies, FOVvolume, by = "Site_ID", all.x = TRUE)

#calculate densities (divide all values by Volume then multiply by 100 (for density per 100m transect))
sitedensity <- sitespecies %>%
  mutate(across(
    .cols = where(is.numeric), 
    ~ (. / Volume) * 100
  ))

########################
###calculate mean per column
Mean_Density <- sitedensity %>%
  summarize(across(where(is.numeric), ~ round(mean(., na.rm = TRUE), digits = 2)))

Mean_Density_long<-Mean_Density%>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Mean")

SD_Density <- sitedensity %>%
  summarize(across(where(is.numeric), ~ round(sd(., na.rm = TRUE), digits = 2)))

SD_Density_long<-SD_Density%>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "SD")

#join mean and sd dataframes
Mean_SD_Density<-left_join(Mean_Density_long, SD_Density_long, by = "Species")

Total_Count <- sitespecies %>%
  summarize(across(where(is.numeric), ~ round(sum(., na.rm = TRUE), digits = 0)))

Total_Count_long<-Total_Count%>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Total Count")

#join total count dataframes
Fish_Summary<-left_join(Total_Count_long, Mean_SD_Density, by = "Species")

#########################################################
#format table

Fish_Summary$Species <- gsub("Abundance\\.", "", Fish_Summary$Species)

Latin<-ROV%>%
  dplyr::select(Latin, Common) %>%
  distinct(Common, .keep_all = TRUE)%>%
  rename(Species = Common)

Fish_Summary<-left_join(Fish_Summary, Latin, by= "Species")

lp("glue")

Fish_Summary1 <- Fish_Summary %>%
  mutate(species = if_else(
    !is.na(Latin), 
    glue("{Species} ({Latin})"), 
    Species
  ))

Fish_Summary1<- Fish_Summary1%>%
  dplyr::select(species, `Total Count`, Mean, SD)

flextable(Fish_Summary1)

#calculate mean and SD of species richness
SpeciesRichness<- sitespecies%>%
  summarise(mean(SpeciesRichness),
            sd(SpeciesRichness))


############################################################################
AllAdultBen<-ROV%>%
  group_by(Site_ID)%>%
  filter(Number < 10, Number!= "NA") %>%
  filter(grepl("Scorpaenidae|Hexagrammidae|Cottidae", Family))%>%
  count(Common)%>%
  arrange(desc(Site_ID))
flextable(AllAdultBen)

#plot species richness and # calling fish coloured by site
srplot<-ggplot(AllAdultBen, aes(x=Common, y=n))+
  geom_bar(position="stack", stat="identity", colour = "black", fill="cyan4")+ 
  theme_bw()+
  #scale_fill_brewer(palette = "Set2")+
  # scale_fill_manual("Site", values = c("Danger Rocks" = "cyan4", "Taylor Islet" = "goldenrod2"))+ 
  
  #scale_x_discrete(labels = label_wrap_gen(10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +                                                      # Changing the theme to get rid of the grey background
  ylab("Benthic Fish (count)") +                                                   # Changing the text of the y axis label
  xlab("Species")  + 
  theme(axis.text.x = element_text(size = 12, face = "plain", angle = 75, hjust=1, colour = "black")) 
print(srplot)


