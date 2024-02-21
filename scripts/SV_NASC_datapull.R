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

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1")


## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 
Full15_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100x15"

# get a list of file names 
file_names <- list.files(Full15_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full15_df <- data.frame()
## for loop 

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Full15_path, file_name))
  
  # clean and subset the data to keep only what we need 
  file <- file %>%
    select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs 
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it 
  full15_df <- rbind(full15_df, file)
}

# now lets rename the columns to help us merge them. 
full15_df <- full15_df %>%
  rename(Sv_mean_15 = Sv_mean,
         NASC_15 = NASC,
         Depth_mean_15 = Depth_mean)


## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 
Full10_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100x10"

# get a list of file names 
file_names <- list.files(Full10_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full10_df <- data.frame()
## for loop 

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Full10_path, file_name))
  
  # clean and subset the data to keep only what we need 
  file <- file %>%
    select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs 
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it 
  full10_df <- rbind(full10_df, file)
}

# now lets rename the columns to help us merge them. 
full10_df <- full10_df %>%
  rename(Sv_mean_10 = Sv_mean,
         NASC_10 = NASC,
         Depth_mean_10 = Depth_mean)



## now lets do this for the 100x5 
Full5_path <-  "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100x5"

# get a list of file names 
file_names <- list.files(Full5_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full5_df <- data.frame()
## for loop 

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Full5_path, file_name))
  
  # clean and subset the data to keep only what we need 
  file <- file %>%
    select("Sv_mean", "NASC", "Depth_mean","Layer_depth_min","Layer_depth_max" )
  
  # write column with containing the site_IDs 
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it 
  full5_df <- rbind(full5_df, file)
}

# now lets rename the columns to help us merge them. 
full5_df <- full5_df %>%
  rename(Sv_mean_5 = Sv_mean,
         NASC_5 = NASC,
         Depth_mean_5 = Depth_mean)

full_df <- merge(full15_df, full10_df, by = c("Layer_depth_min", "Layer_depth_max", "Site_ID" ))
full_df <- merge(full_df, full5_df, by = c( "Layer_depth_min", "Layer_depth_max", "Site_ID" ))
view(full_df)
