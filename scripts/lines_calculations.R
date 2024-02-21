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

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1")


#### Develop a code to replicate Excel document then pull into a for loop #### 
NS07 <- read.csv("100mLines/Bottom/NS06_T1_Biosonic_20230811_104843_Bottom_100.line.csv")

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
         (180.0/pi) * atan(NS07$GreatCircleDistance[i] / NS07$DepthDifference[i]))
})
# calculate hypotenuse 
NS07$Hypothenuse <- sqrt(NS07$GreatCircleDistance^2 + NS07$DepthDifference^2)

# Calculate the cumulative sum of great circle distance 
NS07$GreatCircleDistance[is.na(NS07$GreatCircleDistance)] <- 0
NS07$CumulativeGCD <- cumsum(NS07$GreatCircleDistance)

# do the same for the hypothenuse
NS07$Hypothenuse[is.na(NS07$Hypothenuse)] <- 0
NS07$CumulativeHypo <- cumsum(NS07$Hypothenuse)

# Label the bins to be 20m segments of the great circle distance 
# definte the bin width
bin_width <- 20

# create a column for the bins and label them based on where they fit in 20m intervals
NS07 <- NS07 %>%
  mutate(Bin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + bin_width, bin_width), labels = FALSE))

####  Lets attempt a for loop #### 
# create a list of all the file names we need to pull through the code 
# specify the path on the computer to the folder with the files you want to run through the code 
folder_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100mLines/Bottom"

# get a list of file names 
file_names <- list.files(folder_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)
View(file_df)

merged_df <- data.frame()
## for loop 

# Specify the path to the folder containing your files
folder_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100mLines/Bottom"
save_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100mLines/Bottom/RCODED"

# Loop through each row of the file_df data frame
for (i in 1:nrow(file_df)) {
  # Get the file name and corresponding Site_ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # Read the file
  file <- read.csv(file.path(folder_path, file_name))
  
  # Select columns
  file <- file %>%
    select("Latitude", "Longitude", "Depth")
  
  #remove duplicates to avoid errors 
  file <- distinct(file, Latitude, Longitude, Depth, .keep_all = TRUE)
  
  # Add Site_ID column
  file$Site_ID <- site_id
  
  # Function to calculate the great circle distance between two points 
  calc_great_circle_distance <- function(lat1, lon1, lat2, lon2) {
    distance <- (acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos((lon2 - lon1) * pi / 180)) * 180 / pi) * 60 * 1852
    return(distance)
  }
  
  # Apply to data 
  file$GreatCircleDistance[2:(nrow(file))] <- sapply(2:(nrow(file)), function(j) {
    lat1 <- file$Latitude[j - 1]
    lon1 <- file$Longitude[j - 1]
    lat2 <- file$Latitude[j]
    lon2 <- file$Longitude[j]
    calc_great_circle_distance(lat1, lon1, lat2, lon2)
  })
  
  ## Calculate difference in depth 
  file$DepthDifference <- c(NA, file$Depth[-1] - file$Depth[-nrow(file)])
  
  # Calculate Slope 
  file$Slope <- sapply(1:nrow(file), function(j) {
    ifelse(is.na(file$GreatCircleDistance[j]) || is.na(file$DepthDifference[j]),
           NA,
           (180.0/pi) * atan(file$GreatCircleDistance[j] / file$DepthDifference[j]))
  })
  
  # calculate hypotenuse 
  file$Hypothenuse <- sqrt(file$GreatCircleDistance^2 + file$DepthDifference^2)
  
  # Calculate the cumulative sum of great circle distance 
  file$GreatCircleDistance[is.na(file$GreatCircleDistance)] <- 0
  file$CumulativeGCD <- cumsum(file$GreatCircleDistance)
  
  # do the same for the hypothenuse
  file$Hypothenuse[is.na(file$Hypothenuse)] <- 0
  file$CumulativeHypo <- cumsum(file$Hypothenuse)
  
  #### Label the bins to be 20m segments of the great circle distance ####
  # definte the bin width
  bin_width <- 20
  
  # create a column for the bins and label them based on where they fit in 20m intervals
  file <- file %>%
    mutate(Bin = cut(CumulativeGCD, breaks = seq(0, max(CumulativeGCD) + bin_width, bin_width), labels = FALSE))
  
  #merge the files
  merged_df <- rbind(merged_df, file)
  # Save the processed file with the Site_ID label
  write.csv(file, file.path(save_path, paste0(site_id, "_", file_name)), row.names = FALSE)
}


merged_df$Bin_ID <- paste(merged_df$Site_ID, merged_df$Bin, sep = "_")

merged_df <-merged_df %>%
  filter(!is.na(Bin))
