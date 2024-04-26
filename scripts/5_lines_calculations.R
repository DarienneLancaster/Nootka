# By: Hutton Noth 
# Date: Feb 20th, 2024 

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

#setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1")


#### Develop a code to replicate Excel document then pull into a for loop #### 
#NS07 <- read.csv("100mLines/Bottom/NS06_T1_Biosonic_20230811_104843_Bottom_100.line.csv")
NS07 <- read.csv("odata/NS06_T1_Biosonic_20230811_104843_Bottom_100.line.csv")
# Subset or dataframe, remove all columns that are not useful 
NS07 <- NS07 %>% select("Latitude","Longitude","Depth")

# Fill in a column 
NS07 <- NS07 %>% mutate(Site_ID = "NS07")
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
         (180.0/pi) * atan(NS07$DepthDifference[i]/NS07$GreatCircleDistance[i]))
})
# calculate hypotenuse 
NS07$Hypotenuse <- sqrt(NS07$GreatCircleDistance^2 + NS07$DepthDifference^2)

# Calculate the cumulative sum of great circle distance 
NS07$GreatCircleDistance[is.na(NS07$GreatCircleDistance)] <- 0
NS07$CumulativeGCD <- cumsum(NS07$GreatCircleDistance)

# do the same for the Hypotenuse
NS07$Hypotenuse[is.na(NS07$Hypotenuse)] <- 0
NS07$CumulativeHypo <- cumsum(NS07$Hypotenuse)

# Label the bins to be 20m segments of the great circle distance 
# definte the bin width
bin_width <- 20
slope_width <-5

# create a column for the bins and label them based on where they fit in 20m intervals
NS07 <- NS07 %>%
  mutate(Bin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + bin_width, bin_width), labels = FALSE))

# create a column for slope bin annd same them based on what part of the 5m segment it falls in 
NS07 <- NS07 %>%
  mutate(SlopeBin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + slope_width, slope_width), labels = FALSE))

####  Lets attempt a for loop #### 
# create a list of all the file names we need to pull through the code 
# specify the path on the computer to the folder with the files you want to run through the code
#changed path to work on both our computers, just need to load Transect 1 into odata folder
folder_path <- "odata/Transect 1/100mLines/Bottom"

# get a list of file names 
file_names <- list.files(folder_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


merged_df <- data.frame()
## for loop 

# make path for the for-loop to pull from 
folder_path <- "odata/Transect 1/100mLines/Bottom"
save_path <- "odata/Transect 1/100mLines/Bottom/RCODED"

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(folder_path, file_name))
  
  # clean and subset the data to keep only what we need 
  file <- file %>%
    select("Latitude", "Longitude", "Depth")
  
  # remove any duplicate pings with same lat long and depth
  file <- distinct(file, Latitude, Longitude, Depth, .keep_all = TRUE)
  
  # write column with containing the site_IDs 
  file$Site_ID <- site_id
  
  # calculate the great circle distance 
  calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
    distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) *
                        cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
    return(distance)
  }
  
  # apply the GCD function to our data 
  file$GreatCircleDistance[2:(nrow(file))] <- sapply(2:(nrow(file)), function(j) {
    lat1 <- file$Latitude[j - 1]
    lon1 <- file$Longitude[j - 1]
    lat2 <- file$Latitude[j]
    lon2 <- file$Longitude[j]
    calc_great_circle_distance(lat1, lon1, lat2, lon2)
  })
  
  ## difference in depth 
  file$DepthDifference <- c(NA, file$Depth[-1] - file$Depth[-nrow(file)])
  
  # measures slope 
  file$Slope <- sapply(1:nrow(file), function(j) {
    ifelse(is.na(file$GreatCircleDistance[j]) || is.na(file$DepthDifference[j]),
           NA,
           (180.0/pi) * atan(file$DepthDifference[j]/file$GreatCircleDistance[j]))
  })
  
  # calculate the hypotenuse 
  file$Hypotenuse <- sqrt(file$GreatCircleDistance^2 + file$DepthDifference^2)
  
  # cumulative sum of great circle distance to help bin the data
  file$GreatCircleDistance[is.na(file$GreatCircleDistance)] <- 0
  file$CumulativeGCD <- cumsum(file$GreatCircleDistance)
  
  # do the same for the Hypotenuse
  file$Hypotenuse[is.na(file$Hypotenuse)] <- 0
  file$CumulativeHypo <- cumsum(file$Hypotenuse)
  
  # set bin width to 20 
  bin_width <- 20
  slope_width <- 5
  
  # create a column for bin annd same them based on what part of the 20m segement it falls in 
  file <- file %>%
    mutate(Bin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + bin_width, bin_width), labels = FALSE))
  
  # create a column for slope bin annd same them based on what part of the 5m segment it falls in 
  file <- file %>%
    mutate(SlopeBin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + slope_width, slope_width), labels = FALSE))
  
  # create a df what has all the data from each file in it 
  merged_df <- rbind(merged_df, file)
  # export the files to path to save CSV in a folder 
  write.csv(file, file.path(save_path, paste0(site_id, "_", file_name)), row.names = FALSE)
}

# create binID to match other data frames 
merged_df$BinID <- paste(merged_df$Site_ID, merged_df$Bin, sep = "_")

merged_df <-merged_df %>%
  filter(!is.na(Bin))

# load in bininfo from saved on cleandata.R 
#load("C:/Users/HuttonNoth(HFS)/Desktop/Biosonic_ROV_2023/Nootka_Aug2023/R/Nootka/bininfo.RData")
load("wdata/bininfo.RData")

#### lets calculate different variables for each of the bin level 

#first make a column that changes negative slope values to positive
merged_df$noneg_slope<-abs(merged_df$Slope)
#also try a column that makes all negative slope values NA
merged_df<-merged_df %>% mutate(negna_slope = (Slope = replace(Slope, which(Slope<0), NA)))

#calculate average slope over 5m slope bins
# create slopebinID s 
merged_df$SlopeID <- paste(merged_df$Site_ID, merged_df$SlopeBin, sep = "_")

#calculate slope in 5m bins using only start and end Lat/Long for each 5m bin
BEslope<-merged_df%>%
  group_by(SlopeID)%>%
  slice(c(1,n()))



# Function to calculate the great circle distance between two points 
calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
  distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
  return(distance)
}

# calculate GCD on start and end 5m bin coordinates only
BEslope$GreatCircleDistance[2:(nrow(BEslope))] <- sapply(2:(nrow(BEslope)), function(i) {
  lat1 <- BEslope$Latitude[i - 1]
  lon1 <- BEslope$Longitude[i - 1]
  lat2 <- BEslope$Latitude[i]
  lon2 <- BEslope$Longitude[i]
  ifelse((BEslope$SlopeID[i]) != (BEslope$SlopeID[i -1]),
         NA,
  calc_great_circle_distance(lat1, lon1, lat2, lon2))
})


## Calculate difference in depth

BEslope$DepthDifference[2:(nrow(BEslope))] <- sapply(2:(nrow(BEslope)), function(i) {
  ifelse((BEslope$SlopeID[i]) != (BEslope$SlopeID[i -1]),
         NA,
         (BEslope$Depth[i-1] - BEslope$Depth[i]))
})


# Calculate 5m Slope 
BEslope$Slope5m <- sapply(1:nrow(BEslope), function(i) {
  ifelse(is.na(BEslope$GreatCircleDistance[i]) || is.na(BEslope$DepthDifference[i]),
         NA,
         (180.0/pi) * atan(BEslope$DepthDifference[i]/BEslope$GreatCircleDistance[i]))
})

#average 5m slope bins for full sites
Ave5mSlopeSite<- BEslope%>%
  group_by(Site_ID)%>%
  filter(!is.na(Slope5m))%>%
  mutate(Slope5m =abs(Slope5m))%>%
  summarise(Bin5m_Ave_Slope_Site = mean(Slope5m))

#average 5m slope bins for 20m bins
Ave5mSlopeBin<- BEslope%>%
  group_by(BinID)%>%
  filter(!is.na(Slope5m))%>%
  mutate(Slope5m =abs(Slope5m))%>%
  summarise(Bin5m_Ave_Slope_Bin = mean(Slope5m))


#join 5m slope averages for site and bin to full dataframe

merged_df<-left_join(merged_df, Ave5mSlopeSite, by= "Site_ID")
merged_df<-left_join(merged_df, Ave5mSlopeBin, by= "BinID")

BinBottom <- merged_df %>%
  group_by(BinID) %>%
  summarize(
    Average_Slope = mean(Slope, na.rm = TRUE),
    Average_5m_slope = mean(Bin5m_Ave_Slope_Bin, na.rm = TRUE),
    Average_noneg_slope = mean(noneg_slope, na.rm = TRUE),
    Average_negna_slope = mean(negna_slope, na.rm = TRUE),
    Std_Dev_Slope = sd(Slope, na.rm = TRUE), 
    profilelength = sum(GreatCircleDistance, na.rm = TRUE),
    chainlength = sum(Hypotenuse, na.rm = TRUE),
    Average_Depth = mean(Depth, na.rm = TRUE),
    SD_Depth = sd(Depth, na.rm = TRUE),
  )
BinBottom <- BinBottom %>% mutate(PaperRatio = 100*(profilelength / chainlength))
BinBottom <- BinBottom %>% mutate(Ratio = (chainlength/profilelength))
BinBottom <- BinBottom %>% mutate(ChainDiff = (chainlength-profilelength))

binlines <- bininfo %>% select("BinID")
binlines <- left_join(binlines, BinBottom, by = "BinID")

# Lets calculate site level variables 

#first make a column that changes negative slope values to positive
merged_df$noneg_slope<-abs(merged_df$Slope)
#also try a column that makes all negative slope values NA
merged_df<-merged_df %>% mutate(negna_slope = (Slope = replace(Slope, which(Slope<0), NA)))

SiteBottom <- merged_df %>%
  group_by(Site_ID) %>%
  summarize(
    Average_Slope = mean(Slope, na.rm = TRUE),
    Average_5m_slope = mean(Bin5m_Ave_Slope_Site, na.rm = TRUE),
    Average_noneg_slope = mean(noneg_slope, na.rm = TRUE),
    Average_negna_slope = mean(negna_slope, na.rm = TRUE),
    Std_Dev_Slope = sd(Slope, na.rm = TRUE), 
    profilelength = sum(GreatCircleDistance, na.rm = TRUE),
    chainlength = sum(Hypotenuse, na.rm = TRUE),
    Average_Depth = mean(Depth, na.rm = TRUE),
    SD_Depth = sd(Depth, na.rm = TRUE),
  )
SiteBottom <- SiteBottom %>% mutate(PaperRatio = 100*(profilelength / chainlength))
SiteBottom <- SiteBottom %>% mutate(Ratio = (chainlength/profilelength))
SiteBottom <- SiteBottom %>% mutate(ChainDiff = (chainlength-profilelength))


sitelines <- siteinfo %>% select("Site_ID")
sitelines <- left_join(sitelines, SiteBottom, by = "Site_ID")


#### Deadzone for-loop ####

# define paths 
# deadzone_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100mLines/Deadzone"
# deadzone_save_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100mLines/Deadzone/RCODED"

deadzone_path <- "odata/Transect 1/100mLines/Deadzone"
deadzone_save_path <- "odata/Transect 1/100mLines/Deadzone/RCODED"

# list all relevant files 
deadzone_names <- list.files(deadzone_path)

# filter for only CSV files 
deadzone_csv <- deadzone_names[grep("\\.csv$", deadzone_names)]

# make dataframe 
deadzone_df <- data.frame(deadzone_names = deadzone_csv)

# site_Id column 
deadzone_df$Site_ID <- substr(deadzone_df$deadzone_names, 1, 4)

# make empty datagram 
deadzone_merge <- data.frame()

# now lets run the for-loop on the deadzone line 
for (i in 1:nrow(deadzone_df)) {
  # set site id and file names to run through the for loop 
  deadzone_file_name <- deadzone_df$deadzone_names[i]
  deadzone_site_id <- deadzone_df$Site_ID[i]
  
  # read in the files 
  deadzone_file <- read.csv(file.path(deadzone_path, deadzone_file_name))
  
  # clean up the dataframe 
  deadzone_file <- deadzone_file %>%
    select("Latitude", "Longitude", "Depth")
  
  # remove duplicate pings 
  deadzone_file <- distinct(deadzone_file, Latitude, Longitude, Depth, .keep_all = TRUE)
  
  # Add site ID 
  deadzone_file$Site_ID <- deadzone_site_id
  
  # great circle distance 
  calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
    distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
    return(distance)
  }
  
  # apply GCD to data 
  deadzone_file$GreatCircleDistance[2:(nrow(deadzone_file))] <- sapply(2:(nrow(deadzone_file)), function(j) {
    lat1 <- deadzone_file$Latitude[j - 1]
    lon1 <- deadzone_file$Longitude[j - 1]
    lat2 <- deadzone_file$Latitude[j]
    lon2 <- deadzone_file$Longitude[j]
    calc_great_circle_distance(lat1, lon1, lat2, lon2)
  })
  
  ## measure the depth change 
  deadzone_file$DepthDifference <- c(NA, deadzone_file$Depth[-1] - deadzone_file$Depth[-nrow(deadzone_file)])
  
  # calculate the slope 
  deadzone_file$Slope <- sapply(1:nrow(deadzone_file), function(j) {
    ifelse(is.na(deadzone_file$GreatCircleDistance[j]) || is.na(deadzone_file$DepthDifference[j]),
           NA,
           (180.0/pi) * atan(deadzone_file$GreatCircleDistance[j] / deadzone_file$DepthDifference[j]))
  })
  
  # calculate the hypotenuse 
  deadzone_file$Hypotenuse <- sqrt(deadzone_file$GreatCircleDistance^2 + deadzone_file$DepthDifference^2)
  
  # measure the cumulative sum of GCD to allow the data to get binned 
  deadzone_file$GreatCircleDistance[is.na(deadzone_file$GreatCircleDistance)] <- 0
  deadzone_file$CumulativeGCD <- cumsum(deadzone_file$GreatCircleDistance)
  
  # do the same for the Hypotenuse 
  deadzone_file$Hypotenuse[is.na(deadzone_file$Hypotenuse)] <- 0
  deadzone_file$CumulativeHypo <- cumsum(deadzone_file$Hypotenuse)
  
  # make the min 20m 
  bin_width <- 20
  
  # column for bin and name them based on what part of the 20m segment it falls in 
  deadzone_file <- deadzone_file %>%
    mutate(Bin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + bin_width, bin_width), labels = FALSE))
  
  # create a df that holds all the data
  deadzone_merge <- rbind(deadzone_merge, deadzone_file)
  
  # export csv of the calculations
  write.csv(deadzone_file, file.path(deadzone_save_path, paste0(deadzone_site_id, "_", deadzone_file_name)), row.names = FALSE)
}

# create binID to match other data frames 
deadzone_merge$BinID <- paste(deadzone_merge$Site_ID, deadzone_merge$Bin, sep = "_")

# remove the unnecessary columns 
deadzone_merge <- deadzone_merge %>%filter(!is.na(Bin))

deadzone_merge$DZDepth <- deadzone_merge$Depth
#subset the data 
deadzone_clean <- deadzone_merge %>% select(Latitude, Longitude, GreatCircleDistance, DZDepth)

# Now lets add the depth of the deadzone to the bottom dataframe
combined_df <- merge(merged_df,  deadzone_clean, 
                  by = c("Latitude", "Longitude", "GreatCircleDistance"))

# now lets create a column to calculate the difference in depth of the deadzone to bottom line 
combined_df <- combined_df %>% mutate(DZDifference = abs(Depth - DZDepth))
combined_df <- combined_df %>% mutate(Area = DZDifference * GreatCircleDistance)



#### lets calculate different variables for each of variables at bin level ####
BinDeadzone <- combined_df %>%
  group_by(BinID) %>%
  summarize(
    Average_DZDiff = mean(DZDifference, na.rm = TRUE),
    Std_Dev_DZDiff = sd(DZDifference, na.rm = TRUE), 
    CumulativeArea= sum(Area, na.rm = TRUE),
  )

binlines <- left_join(binlines, BinDeadzone, by = "BinID")

# Lets calculate site level variables 
SiteDeadzone <- combined_df %>%
  group_by(Site_ID) %>%
  summarize(
    Average_DZDiff = mean(DZDifference, na.rm = TRUE),
    Std_Dev_DZDiff = sd(DZDifference, na.rm = TRUE), 
    CumulativeArea= sum(Area, na.rm = TRUE),
  )
sitelines <- left_join(sitelines, SiteDeadzone, by = "Site_ID")



# save(sitelines, file = "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/full_lines.RData")
# save(binlines, file = "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/bin_lines.RData")

save(sitelines, file = "wdata/full_lines.RData")
save(binlines, file = "wdata/bin_lines.RData")

####checking if acoustic data depth range matches ROV data depth range
#calculate start and end depth for each transect and each bin
BEdepth_site<-merged_df%>%
  group_by(Site_ID)%>%
  slice(c(1,n()))%>%
  select(Site_ID, Depth)%>%
  mutate(unique_ID = row_number())%>%
  mutate(Site_ID_unique = paste0(Site_ID, "_", unique_ID)) %>%
  select(-unique_ID)

#calculate start and end depth for each transect and each bin
BEdepth_bin<-merged_df%>%
  group_by(BinID)%>%
  slice(c(1,n()))%>%
  select(BinID, Depth)

EVdata <-read.csv("odata/NootkaROV_20240301_finalDL.csv",  skip=4, stringsAsFactors = FALSE)
# create a column for Site_ID by pulling the first 4 letters from filename 
EVdata$Site_ID <- substr(EVdata$Filename, 1, 4)

BEdepth_siteEV<-EVdata%>%
  filter(Notes!="Start", Notes!="End")%>%
  group_by(Site_ID)%>%
  slice(c(1,n()))%>%
  select(Site_ID, Depth.1)%>%
  rename(Depth_ROV = Depth.1)%>%
  mutate(unique_ID = row_number())%>%
  mutate(Site_ID_unique = paste0(Site_ID, "_", unique_ID)) %>%
  select(-unique_ID)

#convert ft to meters
BEdepth_siteEV$Depth_ROV<-BEdepth_siteEV$Depth_ROV/3.281

AVdepth<-left_join(BEdepth_site, BEdepth_siteEV, "Site_ID_unique")

