# By: Hutton Noth 
# Date: Feb 20th, 2024 

#Adjustements by: Darienne Lancaster
#April 20, 2024

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

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")

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
    dplyr::select("Latitude", "Longitude", "Depth")
  
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

#calculate slope in 20m bins using only start and end Lat/Long for each 5m bin
BEslope20<-merged_df%>%
  group_by(BinID)%>%
  slice(c(1,n()))

# calculate GCD on start and end 20m bin coordinates only
BEslope20$GreatCircleDistance[2:(nrow(BEslope20))] <- sapply(2:(nrow(BEslope20)), function(i) {
  lat1 <- BEslope20$Latitude[i - 1]
  lon1 <- BEslope20$Longitude[i - 1]
  lat2 <- BEslope20$Latitude[i]
  lon2 <- BEslope20$Longitude[i]
  ifelse((BEslope20$BinID[i]) != (BEslope20$BinID[i -1]),
         NA,
         calc_great_circle_distance(lat1, lon1, lat2, lon2))
})

## Calculate difference in depth 20m bins

BEslope20$DepthDifference[2:(nrow(BEslope20))] <- sapply(2:(nrow(BEslope20)), function(i) {
  ifelse((BEslope20$BinID[i]) != (BEslope20$BinID[i -1]),
         NA,
         (BEslope20$Depth[i-1] - BEslope20$Depth[i]))
})

# calculate the hypotenuse 
BEslope20$Hypotenuse <- sqrt(BEslope20$GreatCircleDistance^2 + BEslope20$DepthDifference^2)

#GCD 20m bins 
BEslope20<- BEslope20%>%
  group_by(BinID)%>%
  filter(!is.na(GreatCircleDistance))%>%
  rename(Hypotenuse20=Hypotenuse)%>%
  rename(GreatCircleDistance20=GreatCircleDistance)%>%
  dplyr::select(c(Hypotenuse20,GreatCircleDistance20,Site_ID,BinID))

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
    Layer_depth_min = min(Depth, na.rm = TRUE), 
    Layer_depth_max = max(Depth, na.rm = TRUE)
  )

binlines <- bininfo %>% dplyr::select("BinID")
binlines <- left_join(binlines, BinBottom, by = "BinID")

#merge 20m hypotenuse(total distance traveled) and GreatCircleDistance20 (better proxy for total linear distance than summing all Great Circle Distances) with binlines
BEslope20bin<- BEslope20%>%
  dplyr::select(c(Hypotenuse20, GreatCircleDistance20,BinID))

binlines<-left_join(binlines, BEslope20bin, by = "BinID")

#remove random extra chunk from NS06_1
binlines<-binlines%>%
  slice(-26)


binlines <- binlines %>% mutate(PaperRatio = 100*(GreatCircleDistance20 / chainlength)) #this is the original formula used in rugosity paper that uses a set length of chain, this doesn't work for our method
binlines <- binlines %>% mutate(Ratio = (chainlength/GreatCircleDistance20)) #this is a ratio adapted to our length over 100m method (this is the one we want to use)
binlines <- binlines %>% mutate(ChainDiff = (chainlength-GreatCircleDistance20)) #this is a simpler version that just subtracts 100m transect from full bottom line length


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
    Layer_depth_min = min(Depth, na.rm = TRUE), 
    Layer_depth_max = max(Depth, na.rm = TRUE)
  )


#merge full hypotenuse(total distance traveled) with sitelines
BEslope20site<- BEslope20%>%
  dplyr::select(c(Hypotenuse20,GreatCircleDistance20, BinID,Site_ID))

BEslope20site <- BEslope20site %>%
  group_by(Site_ID) %>%
  summarize(
    totaldist = sum(Hypotenuse20, na.rm = TRUE),
    GreatCircleDistancefull = sum(GreatCircleDistance20, na.rm = TRUE),
  )


SiteBottom<-left_join(SiteBottom, BEslope20site, by = "Site_ID")

SiteBottom <- SiteBottom %>% mutate(PaperRatio = 100*(totaldist / chainlength))
SiteBottom <- SiteBottom %>% mutate(Ratio = (chainlength/totaldist))
SiteBottom <- SiteBottom %>% mutate(ChainDiff = (chainlength-totaldist))


sitelines <- siteinfo %>% dplyr::select("Site_ID")
sitelines <- left_join(sitelines, SiteBottom, by = "Site_ID")



# #### Deadzone for-loop for Large, 1m, and Manual deadzones ####
##################################################################################

##Large deadzone###
LGdeadzone_path <- "odata/Transect 1/100mLines/Deadzone"
LGdeadzone_save_path <- "odata/Transect 1/100mLines/Deadzone/RCODED"

# list all relevant files 
LGdeadzone_names <- list.files(LGdeadzone_path)

# filter for only CSV files 
LGdeadzone_csv <- LGdeadzone_names[grep("\\.csv$", LGdeadzone_names)]

# make dataframe 
LGdeadzone_df <- data.frame(LGdeadzone_names = LGdeadzone_csv)

# site_Id column 
LGdeadzone_df$Site_ID <- substr(LGdeadzone_df$LGdeadzone_names, 1, 4)

# make empty datagram 
LGdeadzone_merge <- data.frame()

# now lets run the for-loop on the LGdeadzone line 
for (i in 1:nrow(LGdeadzone_df)) {
  # set site id and file names to run through the for loop 
  LGdeadzone_file_name <- LGdeadzone_df$LGdeadzone_names[i]
  LGdeadzone_site_id <- LGdeadzone_df$Site_ID[i]
  
  # read in the files 
  LGdeadzone_file <- read.csv(file.path(LGdeadzone_path, LGdeadzone_file_name))
  
  # clean up the dataframe 
  LGdeadzone_file <- LGdeadzone_file %>%
    dplyr::select("Latitude", "Longitude", "Depth")
  
  # remove duplicate pings 
  LGdeadzone_file <- distinct(LGdeadzone_file, Latitude, Longitude, Depth, .keep_all = TRUE)
  
  # Add site ID 
  LGdeadzone_file$Site_ID <- LGdeadzone_site_id
  
  # great circle distance 
  calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
    distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
    return(distance)
  }
  
  # apply GCD to data 
  LGdeadzone_file$GreatCircleDistance[2:(nrow(LGdeadzone_file))] <- sapply(2:(nrow(LGdeadzone_file)), function(j) {
    lat1 <- LGdeadzone_file$Latitude[j - 1]
    lon1 <- LGdeadzone_file$Longitude[j - 1]
    lat2 <- LGdeadzone_file$Latitude[j]
    lon2 <- LGdeadzone_file$Longitude[j]
    calc_great_circle_distance(lat1, lon1, lat2, lon2)
  })
  
  ## measure the depth change 
  LGdeadzone_file$DepthDifference <- c(NA, LGdeadzone_file$Depth[-1] - LGdeadzone_file$Depth[-nrow(LGdeadzone_file)])
  
  # calculate the slope 
  LGdeadzone_file$Slope <- sapply(1:nrow(LGdeadzone_file), function(j) {
    ifelse(is.na(LGdeadzone_file$GreatCircleDistance[j]) || is.na(LGdeadzone_file$DepthDifference[j]),
           NA,
           (180.0/pi) * atan(LGdeadzone_file$GreatCircleDistance[j] / LGdeadzone_file$DepthDifference[j]))
  })
  
  # calculate the hypotenuse 
  LGdeadzone_file$Hypotenuse <- sqrt(LGdeadzone_file$GreatCircleDistance^2 + LGdeadzone_file$DepthDifference^2)
  
  # measure the cumulative sum of GCD to allow the data to get binned 
  LGdeadzone_file$GreatCircleDistance[is.na(LGdeadzone_file$GreatCircleDistance)] <- 0
  LGdeadzone_file$CumulativeGCD <- cumsum(LGdeadzone_file$GreatCircleDistance)
  
  # do the same for the Hypotenuse 
  LGdeadzone_file$Hypotenuse[is.na(LGdeadzone_file$Hypotenuse)] <- 0
  LGdeadzone_file$CumulativeHypo <- cumsum(LGdeadzone_file$Hypotenuse)
  
  # make the min 20m 
  bin_width <- 20
  
  # column for bin and name them based on what part of the 20m segment it falls in 
  LGdeadzone_file <- LGdeadzone_file %>%
    mutate(Bin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + bin_width, bin_width), labels = FALSE))
  
  # create a df that holds all the data
  LGdeadzone_merge <- rbind(LGdeadzone_merge, LGdeadzone_file)
  
  # export csv of the calculations
  write.csv(LGdeadzone_file, file.path(LGdeadzone_save_path, paste0(LGdeadzone_site_id, "_", LGdeadzone_file_name)), row.names = FALSE)
}

# create binID to match other data frames 
LGdeadzone_merge$BinID <- paste(LGdeadzone_merge$Site_ID, LGdeadzone_merge$Bin, sep = "_")

# remove the unnecessary columns 
LGdeadzone_merge <- LGdeadzone_merge %>%filter(!is.na(Bin))

LGdeadzone_merge$LG_DZDepth <- LGdeadzone_merge$Depth
#subset the data 
LGdeadzone_clean <- LGdeadzone_merge %>% dplyr::select(Latitude, Longitude, GreatCircleDistance, LG_DZDepth)

# Now lets add the depth of the LGdeadzone to the bottom dataframe
combined_df <- merge(merged_df,  LGdeadzone_clean, 
                     by = c("Latitude", "Longitude", "GreatCircleDistance"))

# now lets create a column to calculate the difference in depth of the LGdeadzone to bottom line 
combined_df <- combined_df %>% mutate(LG_DZDifference = abs(Depth - LG_DZDepth))
combined_df <- combined_df %>% mutate(LG_DZ_Area = LG_DZDifference * GreatCircleDistance)



#### lets calculate different variables for each of variables at bin level ####
Bin_LG_Deadzone <- combined_df %>%
  group_by(BinID) %>%
  summarize(
    Average_LG_DZDiff = mean(LG_DZDifference, na.rm = TRUE),
    Std_Dev_LG_DZDiff = sd(LG_DZDifference, na.rm = TRUE), 
    Cumulative_LG_DZ_Area= sum(LG_DZ_Area, na.rm = TRUE),
  )

binlines <- left_join(binlines, Bin_LG_Deadzone, by = "BinID")

# Lets calculate site level variables 
Site_LG_Deadzone <- combined_df %>%
  group_by(Site_ID) %>%
  summarize(
    Average_LG_DZDiff = mean(LG_DZDifference, na.rm = TRUE),
    Std_Dev_LG_DZDiff = sd(LG_DZDifference, na.rm = TRUE), 
    Cumulative_LG_DZ_Area= sum(LG_DZ_Area, na.rm = TRUE),
  )
sitelines <- left_join(sitelines, Site_LG_Deadzone, by = "Site_ID")

##################################################################################
#deadzone 1m 


##1m deadzone###
m1deadzone_path <- "odata/Transect 1/100mLines/Deadzone/1m"
m1deadzone_save_path <- "odata/Transect 1/100mLines/Deadzone/1m/m1RCODED"

# list all relevant files 
m1deadzone_names <- list.files(m1deadzone_path)

# filter for only CSV files 
m1deadzone_csv <- m1deadzone_names[grep("\\.csv$", m1deadzone_names)]

# make dataframe 
m1deadzone_df <- data.frame(m1deadzone_names = m1deadzone_csv)

# site_Id column 
m1deadzone_df$Site_ID <- substr(m1deadzone_df$m1deadzone_names, 1, 4)

# make empty datagram 
m1deadzone_merge <- data.frame()

# now lets run the for-loop on the m1deadzone line 
for (i in 1:nrow(m1deadzone_df)) {
  # set site id and file names to run through the for loop 
  m1deadzone_file_name <- m1deadzone_df$m1deadzone_names[i]
  m1deadzone_site_id <- m1deadzone_df$Site_ID[i]
  
  # read in the files 
  m1deadzone_file <- read.csv(file.path(m1deadzone_path, m1deadzone_file_name))
  
  # clean up the dataframe 
  m1deadzone_file <- m1deadzone_file %>%
    dplyr::select("Latitude", "Longitude", "Depth")
  
  # remove duplicate pings 
  m1deadzone_file <- distinct(m1deadzone_file, Latitude, Longitude, Depth, .keep_all = TRUE)
  
  # Add site ID 
  m1deadzone_file$Site_ID <- m1deadzone_site_id
  
  # great circle distance 
  calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
    distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
    return(distance)
  }
  
  # apply GCD to data 
  m1deadzone_file$GreatCircleDistance[2:(nrow(m1deadzone_file))] <- sapply(2:(nrow(m1deadzone_file)), function(j) {
    lat1 <- m1deadzone_file$Latitude[j - 1]
    lon1 <- m1deadzone_file$Longitude[j - 1]
    lat2 <- m1deadzone_file$Latitude[j]
    lon2 <- m1deadzone_file$Longitude[j]
    calc_great_circle_distance(lat1, lon1, lat2, lon2)
  })
  
  ## measure the depth change 
  m1deadzone_file$DepthDifference <- c(NA, m1deadzone_file$Depth[-1] - m1deadzone_file$Depth[-nrow(m1deadzone_file)])
  
  # calculate the slope 
  m1deadzone_file$Slope <- sapply(1:nrow(m1deadzone_file), function(j) {
    ifelse(is.na(m1deadzone_file$GreatCircleDistance[j]) || is.na(m1deadzone_file$DepthDifference[j]),
           NA,
           (180.0/pi) * atan(m1deadzone_file$GreatCircleDistance[j] / m1deadzone_file$DepthDifference[j]))
  })
  
  # calculate the hypotenuse 
  m1deadzone_file$Hypotenuse <- sqrt(m1deadzone_file$GreatCircleDistance^2 + m1deadzone_file$DepthDifference^2)
  
  # measure the cumulative sum of GCD to allow the data to get binned 
  m1deadzone_file$GreatCircleDistance[is.na(m1deadzone_file$GreatCircleDistance)] <- 0
  m1deadzone_file$CumulativeGCD <- cumsum(m1deadzone_file$GreatCircleDistance)
  
  # do the same for the Hypotenuse 
  m1deadzone_file$Hypotenuse[is.na(m1deadzone_file$Hypotenuse)] <- 0
  m1deadzone_file$CumulativeHypo <- cumsum(m1deadzone_file$Hypotenuse)
  
  # make the min 20m 
  bin_width <- 20
  
  # column for bin and name them based on what part of the 20m segment it falls in 
  m1deadzone_file <- m1deadzone_file %>%
    mutate(Bin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + bin_width, bin_width), labels = FALSE))
  
  # create a df that holds all the data
  m1deadzone_merge <- rbind(m1deadzone_merge, m1deadzone_file)
  
  # export csv of the calculations
  write.csv(m1deadzone_file, file.path(m1deadzone_save_path, paste0(m1deadzone_site_id, "_", m1deadzone_file_name)), row.names = FALSE)
}

# create binID to match other data frames 
m1deadzone_merge$BinID <- paste(m1deadzone_merge$Site_ID, m1deadzone_merge$Bin, sep = "_")

# remove the unnecessary columns 
m1deadzone_merge <- m1deadzone_merge %>%filter(!is.na(Bin))

m1deadzone_merge$m1_DZDepth <- m1deadzone_merge$Depth
#subset the data 
m1deadzone_clean <- m1deadzone_merge %>% dplyr::select(Latitude, Longitude, GreatCircleDistance, m1_DZDepth)

# Now lets add the depth of the m1deadzone to the bottom dataframe
combined_df_LG_1m <- merge(combined_df,  m1deadzone_clean, 
                     by = c("Latitude", "Longitude", "GreatCircleDistance"))

# now lets create a column to calculate the difference in depth of the m1deadzone to bottom line 
combined_df_LG_1m <- combined_df_LG_1m %>% mutate(m1_DZDifference = abs(Depth - m1_DZDepth))
combined_df_LG_1m <- combined_df_LG_1m %>% mutate(m1_DZ_Area = m1_DZDifference * GreatCircleDistance)



#### lets calculate different variables for each of variables at bin level ####
Bin_m1_Deadzone <- combined_df_LG_1m %>%
  group_by(BinID) %>%
  summarize(
    Average_m1_DZDiff = mean(m1_DZDifference, na.rm = TRUE),
    Std_Dev_m1_DZDiff = sd(m1_DZDifference, na.rm = TRUE), 
    Cumulative_m1_DZ_Area= sum(m1_DZ_Area, na.rm = TRUE),
  )

binlines <- left_join(binlines, Bin_m1_Deadzone, by = "BinID")

# Lets calculate site level variables 
Site_m1_Deadzone <- combined_df_LG_1m %>%
  group_by(Site_ID) %>%
  summarize(
    Average_m1_DZDiff = mean(m1_DZDifference, na.rm = TRUE),
    Std_Dev_m1_DZDiff = sd(m1_DZDifference, na.rm = TRUE), 
    Cumulative_m1_DZ_Area= sum(m1_DZ_Area, na.rm = TRUE),
  )
sitelines <- left_join(sitelines, Site_m1_Deadzone, by = "Site_ID")

##################################################################################
#deadzone manual


##Large deadzone###
MANdeadzone_path <- "odata/Transect 1/100mLines/Deadzone/ManualDZ"
MANdeadzone_save_path <- "odata/Transect 1/100mLines/Deadzone/ManualDZ/MAN_RCODED"

# list all relevant files 
MANdeadzone_names <- list.files(MANdeadzone_path)

# filter for only CSV files 
MANdeadzone_csv <- MANdeadzone_names[grep("\\.csv$", MANdeadzone_names)]

# make dataframe 
MANdeadzone_df <- data.frame(MANdeadzone_names = MANdeadzone_csv)

# site_Id column 
MANdeadzone_df$Site_ID <- substr(MANdeadzone_df$MANdeadzone_names, 1, 4)

# make empty datagram 
MANdeadzone_merge <- data.frame()

# now lets run the for-loop on the MANdeadzone line 
for (i in 1:nrow(MANdeadzone_df)) {
  # set site id and file names to run through the for loop 
  MANdeadzone_file_name <- MANdeadzone_df$MANdeadzone_names[i]
  MANdeadzone_site_id <- MANdeadzone_df$Site_ID[i]
  
  # read in the files 
  MANdeadzone_file <- read.csv(file.path(MANdeadzone_path, MANdeadzone_file_name))
  
  # clean up the dataframe 
  MANdeadzone_file <- MANdeadzone_file %>%
    dplyr::select("Latitude", "Longitude", "Depth")
  
  # remove duplicate pings 
  MANdeadzone_file <- distinct(MANdeadzone_file, Latitude, Longitude, Depth, .keep_all = TRUE)
  
  # Add site ID 
  MANdeadzone_file$Site_ID <- MANdeadzone_site_id
  
  # great circle distance 
  calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
    distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
    return(distance)
  }
  
  # apply GCD to data 
  MANdeadzone_file$GreatCircleDistance[2:(nrow(MANdeadzone_file))] <- sapply(2:(nrow(MANdeadzone_file)), function(j) {
    lat1 <- MANdeadzone_file$Latitude[j - 1]
    lon1 <- MANdeadzone_file$Longitude[j - 1]
    lat2 <- MANdeadzone_file$Latitude[j]
    lon2 <- MANdeadzone_file$Longitude[j]
    calc_great_circle_distance(lat1, lon1, lat2, lon2)
  })
  
  ## measure the depth change 
  MANdeadzone_file$DepthDifference <- c(NA, MANdeadzone_file$Depth[-1] - MANdeadzone_file$Depth[-nrow(MANdeadzone_file)])
  
  # calculate the slope 
  MANdeadzone_file$Slope <- sapply(1:nrow(MANdeadzone_file), function(j) {
    ifelse(is.na(MANdeadzone_file$GreatCircleDistance[j]) || is.na(MANdeadzone_file$DepthDifference[j]),
           NA,
           (180.0/pi) * atan(MANdeadzone_file$GreatCircleDistance[j] / MANdeadzone_file$DepthDifference[j]))
  })
  
  # calculate the hypotenuse 
  MANdeadzone_file$Hypotenuse <- sqrt(MANdeadzone_file$GreatCircleDistance^2 + MANdeadzone_file$DepthDifference^2)
  
  # measure the cumulative sum of GCD to allow the data to get binned 
  MANdeadzone_file$GreatCircleDistance[is.na(MANdeadzone_file$GreatCircleDistance)] <- 0
  MANdeadzone_file$CumulativeGCD <- cumsum(MANdeadzone_file$GreatCircleDistance)
  
  # do the same for the Hypotenuse 
  MANdeadzone_file$Hypotenuse[is.na(MANdeadzone_file$Hypotenuse)] <- 0
  MANdeadzone_file$CumulativeHypo <- cumsum(MANdeadzone_file$Hypotenuse)
  
  # make the min 20m 
  bin_width <- 20
  
  # column for bin and name them based on what part of the 20m segment it falls in 
  MANdeadzone_file <- MANdeadzone_file %>%
    mutate(Bin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + bin_width, bin_width), labels = FALSE))
  
  # create a df that holds all the data
  MANdeadzone_merge <- rbind(MANdeadzone_merge, MANdeadzone_file)
  
  # export csv of the calculations
  write.csv(MANdeadzone_file, file.path(MANdeadzone_save_path, paste0(MANdeadzone_site_id, "_", MANdeadzone_file_name)), row.names = FALSE)
}

# create binID to match other data frames 
MANdeadzone_merge$BinID <- paste(MANdeadzone_merge$Site_ID, MANdeadzone_merge$Bin, sep = "_")

# remove the unnecessary columns 
MANdeadzone_merge <- MANdeadzone_merge %>%filter(!is.na(Bin))

MANdeadzone_merge$MAN_DZDepth <- MANdeadzone_merge$Depth
#subset the data 
MANdeadzone_clean <- MANdeadzone_merge %>% dplyr::select(Latitude, Longitude, GreatCircleDistance, MAN_DZDepth)

# Now lets add the depth of the MANdeadzone to the bottom dataframe
combined_df_LG_1m_MAN <- merge(combined_df_LG_1m,  MANdeadzone_clean, 
                           by = c("Latitude", "Longitude", "GreatCircleDistance"))

# now lets create a column to calculate the difference in depth of the MANdeadzone to bottom line 
combined_df_LG_1m_MAN <- combined_df_LG_1m_MAN %>% mutate(MAN_DZDifference = abs(Depth - MAN_DZDepth))
combined_df_LG_1m_MAN <- combined_df_LG_1m_MAN %>% mutate(MAN_DZ_Area = MAN_DZDifference * GreatCircleDistance)



#### lets calculate different variables for each of variables at bin level ####
Bin_MAN_Deadzone <- combined_df_LG_1m_MAN %>%
  group_by(BinID) %>%
  summarize(
    Average_MAN_DZDiff = mean(MAN_DZDifference, na.rm = TRUE),
    Std_Dev_MAN_DZDiff = sd(MAN_DZDifference, na.rm = TRUE), 
    Cumulative_MAN_DZ_Area= sum(MAN_DZ_Area, na.rm = TRUE),
  )

binlines <- left_join(binlines, Bin_MAN_Deadzone, by = "BinID")

# Lets calculate site level variables 
Site_MAN_Deadzone <- combined_df_LG_1m_MAN %>%
  group_by(Site_ID) %>%
  summarize(
    Average_MAN_DZDiff = mean(MAN_DZDifference, na.rm = TRUE),
    Std_Dev_MAN_DZDiff = sd(MAN_DZDifference, na.rm = TRUE), 
    Cumulative_MAN_DZ_Area= sum(MAN_DZ_Area, na.rm = TRUE),
  )
sitelines <- left_join(sitelines, Site_MAN_Deadzone, by = "Site_ID")


# save(binlines, file = "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/bin_lines.RData")


save(sitelines, file = "wdata/sitelines.RData")
write.csv(sitelines, "wdata/sitelines.csv")
save(binlines, file = "wdata/binlines.RData")
write.csv(binlines, "wdata/binlines.csv")

####checking if acoustic data depth range matches ROV data depth range
#calculate start and end depth for each transect and each bin
BEdepth_site<-merged_df%>%
  group_by(Site_ID)%>%
  slice(c(1,n()))%>%
  dplyr::select(Site_ID, Depth)%>%
  mutate(unique_ID = row_number())%>%
  mutate(Site_ID_unique = paste0(Site_ID, "_", unique_ID)) %>%
  dplyr::select(-unique_ID)

#calculate start and end depth for each transect and each bin
BEdepth_bin<-merged_df%>%
  group_by(BinID)%>%
  slice(c(1,n()))%>%
  dplyr::select(BinID, Depth)

EVdata <-read.csv("odata/NootkaROV_20240301_finalDL.csv",  skip=4, stringsAsFactors = FALSE)
# create a column for Site_ID by pulling the first 4 letters from filename 
EVdata$Site_ID <- substr(EVdata$Filename, 1, 4)

BEdepth_siteEV<-EVdata%>%
  filter(Notes!="Start", Notes!="End")%>%
  group_by(Site_ID)%>%
  slice(c(1,n()))%>%
  dplyr::select(Site_ID, Depth.1)%>%
  rename(Depth_ROV = Depth.1)%>%
  mutate(unique_ID = row_number())%>%
  mutate(Site_ID_unique = paste0(Site_ID, "_", unique_ID)) %>%
  dplyr::select(-unique_ID)

#convert ft to meters
BEdepth_siteEV$Depth_ROV<-BEdepth_siteEV$Depth_ROV/3.281

AVdepth<-left_join(BEdepth_site, BEdepth_siteEV, "Site_ID_unique")




##################################################################################
#### Transect 3 Bottom Line forloop ####
# Lets calculate site level variables 

folder_path_t3 <- "odata/Transect 3/Bottom Lines"

# get a list of file names 
file_names_t3 <- list.files(folder_path_t3)

# select only the csv files 
csv_files_t3 <- file_names_t3[grep("\\.csv$", file_names_t3)]

# make it into a dataframe
file_df_t3 <- data.frame(file_name = csv_files_t3)

# create a column in the dataframe for site ID 
file_df_t3$Site_ID <- substr(file_df_t3$file_name, 1, 4)

merged_df_t3 <- data.frame()

# make path for the for-loop to pull from 
save_path_t3 <- "odata/Transect 3/Bottom Lines/RCODED"

# loop each row in the file_df_t3 that contains all the names of files of interest
for (i in 1:nrow(file_df_t3)) {
  # set the file name and site ID 
  file_name <- file_df_t3$file_name[i]
  site_id <- file_df_t3$Site_ID[i]
  
  # pull in files from our specified pathway 
  file <- read.csv(file.path(folder_path_t3, file_name))
  
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
  
  # difference in depth 
  file$DepthDifference <- c(NA, file$Depth[-1] - file$Depth[-nrow(file)])
  
  # measure slope 
  file$Slope <- sapply(1:nrow(file), function(j) {
    ifelse(is.na(file$GreatCircleDistance[j]) || is.na(file$DepthDifference[j]),
           NA,
           (180.0/pi) * atan(file$DepthDifference[j] / file$GreatCircleDistance[j]))
  })
  
  # calculate the hypotenuse 
  file$Hypotenuse <- sqrt(file$GreatCircleDistance^2 + file$DepthDifference^2)
  
  # cumulative sum of great circle distance to help bin the data
  file$GreatCircleDistance[is.na(file$GreatCircleDistance)] <- 0
  file$CumulativeGCD <- cumsum(file$GreatCircleDistance)
  
  # do the same for the Hypotenuse
  file$Hypotenuse[is.na(file$Hypotenuse)] <- 0
  file$CumulativeHypo <- cumsum(file$Hypotenuse)
  
  # create a dataframe that holds all the data
  merged_df_t3 <- rbind(merged_df_t3, file)
  
  # export csv of the calculations
  write.csv(file, file.path(save_path_t3, paste0(site_id, "_", file_name)), row.names = FALSE)
}


#### Transect 3 bottom Variables ##### 
# First make a column that changes negative slope values to positive
merged_df_t3$noneg_slope <- abs(merged_df_t3$Slope)

# Also try a column that makes all negative slope values NA
merged_df_t3 <- merged_df_t3 %>%
  mutate(negna_slope = replace(Slope, Slope < 0, NA))

SiteBottom_t3 <- merged_df_t3 %>%
  group_by(Site_ID) %>%
  summarize(
    Average_Slope = mean(Slope, na.rm = TRUE),
 #   Average_5m_slope = mean(Bin5m_Ave_Slope_Site, na.rm = TRUE),
    Average_noneg_slope = mean(noneg_slope, na.rm = TRUE),
    Average_negna_slope = mean(negna_slope, na.rm = TRUE),
    Std_Dev_Slope = sd(Slope, na.rm = TRUE), 
    profilelength = sum(GreatCircleDistance, na.rm = TRUE),
    chainlength = sum(Hypotenuse, na.rm = TRUE)
  )

SiteBottom_t3 <- SiteBottom_t3 %>%
  mutate(PaperRatio = 100 * (profilelength / chainlength)) %>%
  mutate(Ratio = chainlength / profilelength) %>%
  mutate(ChainDiff = chainlength - profilelength)

sitelines_t3 <- siteinfo %>% select("Site_ID")
sitelines_t3 <- left_join(sitelines_t3, SiteBottom_t3, by = "Site_ID")



#### Transect 3 
deadzone_path_t3 <- "odata/Transect 3/Deadzone Lines"
deadzone_save_path_t3 <- "odata/Transect 3/Deadzone Lines"

# List all relevant files
deadzone_names_t3 <- list.files(deadzone_path_t3)

# Filter for only CSV files
deadzone_csv_t3 <- deadzone_names_t3[grep("\\.csv$", deadzone_names_t3)]

# Make dataframe
deadzone_df_t3 <- data.frame(deadzone_names = deadzone_csv_t3)

# Site_Id column
deadzone_df_t3$Site_ID <- substr(deadzone_df_t3$deadzone_names, 1, 4)

# Make empty dataframe
deadzone_merge_t3 <- data.frame()

# Now let's run the for-loop on the deadzone line
for (i in 1:nrow(deadzone_df_t3)) {
  # Set site ID and file names to run through the for loop
  deadzone_file_name_t3 <- deadzone_df_t3$deadzone_names[i]
  deadzone_site_id_t3 <- deadzone_df_t3$Site_ID[i]
  
  # Read in the files
  deadzone_file_t3 <- read.csv(file.path(deadzone_path_t3, deadzone_file_name_t3))
  
  # Clean up the dataframe
  deadzone_file_t3 <- deadzone_file_t3 %>%
    select("Latitude", "Longitude", "Depth")
  
  # Remove duplicate pings
  deadzone_file_t3 <- distinct(deadzone_file_t3, Latitude, Longitude, Depth, .keep_all = TRUE)
  
  # Add site ID
  deadzone_file_t3$Site_ID <- deadzone_site_id_t3
  
  # Great circle distance function
  calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
    distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
    return(distance)
  }
  
  # Apply GCD to data
  deadzone_file_t3$GreatCircleDistance[2:(nrow(deadzone_file_t3))] <- sapply(2:(nrow(deadzone_file_t3)), function(j) {
    lat1 <- deadzone_file_t3$Latitude[j - 1]
    lon1 <- deadzone_file_t3$Longitude[j - 1]
    lat2 <- deadzone_file_t3$Latitude[j]
    lon2 <- deadzone_file_t3$Longitude[j]
    calc_great_circle_distance(lat1, lon1, lat2, lon2)
  })
  
  # Measure the depth change
  deadzone_file_t3$DepthDifference <- c(NA, deadzone_file_t3$Depth[-1] - deadzone_file_t3$Depth[-nrow(deadzone_file_t3)])
  
  # Calculate the slope
  deadzone_file_t3$Slope <- sapply(1:nrow(deadzone_file_t3), function(j) {
    ifelse(is.na(deadzone_file_t3$GreatCircleDistance[j]) || is.na(deadzone_file_t3$DepthDifference[j]),
           NA,
           (180.0/pi) * atan(deadzone_file_t3$DepthDifference[j] / deadzone_file_t3$GreatCircleDistance[j]))
  })
  
  # Calculate the hypotenuse
  deadzone_file_t3$Hypotenuse <- sqrt(deadzone_file_t3$GreatCircleDistance^2 + deadzone_file_t3$DepthDifference^2)
  
  # Measure the cumulative sum of GCD to allow the data to get binned
  deadzone_file_t3$GreatCircleDistance[is.na(deadzone_file_t3$GreatCircleDistance)] <- 0
  deadzone_file_t3$CumulativeGCD <- cumsum(deadzone_file_t3$GreatCircleDistance)
  
  # Do the same for the Hypotenuse
  deadzone_file_t3$Hypotenuse[is.na(deadzone_file_t3$Hypotenuse)] <- 0
  deadzone_file_t3$CumulativeHypo <- cumsum(deadzone_file_t3$Hypotenuse)
 
  # Create a df that holds all the data
  deadzone_merge_t3 <- rbind(deadzone_merge_t3, deadzone_file_t3)
  
  # Export csv of the calculations
  write.csv(deadzone_file_t3, file.path(deadzone_save_path_t3, paste0(deadzone_site_id_t3, "_", deadzone_file_name_t3)), row.names = FALSE)
}




deadzone_merge_t3$DZDepth <- deadzone_merge_t3$Depth

# Subset the data
deadzone_clean_t3 <- deadzone_merge_t3 %>% select(Latitude, Longitude, GreatCircleDistance, DZDepth)

# Now let's add the depth of the deadzone to the bottom dataframe
combined_df_t3 <- merge(merged_df_t3, deadzone_clean_t3, 
                        by = c("Latitude", "Longitude", "GreatCircleDistance"))

# Now let's create a column to calculate the difference in depth of the deadzone to bottom line
combined_df_t3 <- combined_df_t3 %>% mutate(DZDifference = abs(Depth - DZDepth))
combined_df_t3 <- combined_df_t3 %>% mutate(Area = DZDifference * GreatCircleDistance)






