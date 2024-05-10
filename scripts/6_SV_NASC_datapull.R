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

#### 100x15 #### 

## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 
#Hutton path
#Full15_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/100x15"

######pull 100X15 from Large DZ files####################################################################

Full15LG_path <- "odata/Transect 1/100x15"

# get a list of file names
file_names <- list.files(Full15LG_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full15LG_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full15LG_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  full15LG_df <- rbind(full15LG_df, file)
}

# now lets rename the columns to help us merge them.
full15LG_df <- full15LG_df %>%
  rename(Sv_mean_15LG = Sv_mean,
         NASC_15LG = NASC,
         Depth_mean_15LG = Depth_mean)%>%
  dplyr::select(-c(4,5))
######pull 100X15 from 1m DZ files#####################################################################

Full15_1m_path <- "odata/Transect 1/100x15/1MDZ"

# get a list of file names
file_names <- list.files(Full15_1m_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full15_1m_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full15_1m_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  full15_1m_df <- rbind(full15_1m_df, file)
}

# now lets rename the columns to help us merge them.
full15_1m_df <- full15_1m_df %>%
  rename(Sv_mean_15_1m = Sv_mean,
         NASC_15_1m = NASC,
         Depth_mean_15_1m = Depth_mean)%>%
  dplyr::select(-c(4,5))

######pull 100X15 from Manual DZ files#####################################################################

Full15_MAN_path <- "odata/Transect 1/100x15/ManualDZ"

# get a list of file names
file_names <- list.files(Full15_MAN_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full15_MAN_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full15_MAN_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  full15_MAN_df <- rbind(full15_MAN_df, file)
}

# now lets rename the columns to help us merge them.
full15_MAN_df <- full15_MAN_df %>%
  rename(Sv_mean_15_MAN = Sv_mean,
         NASC_15_MAN = NASC,
         Depth_mean_15_MAN = Depth_mean)%>%
  dplyr::select(-c(4,5))

full15_allDZ_df<-left_join(full15_MAN_df, full15_1m_df, by="Site_ID")
full15_allDZ_df<-left_join(full15_allDZ_df, full15LG_df, by= "Site_ID")

#############################################################################
#######pull 100X10 from Large DZ files########################################################################

Full10_LG_path <- "odata/Transect 1/100x10"

# get a list of file names
file_names <- list.files(Full10_LG_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full10_LG_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full10_LG_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  full10_LG_df <- rbind(full10_LG_df, file)
}

# now lets rename the columns to help us merge them.
full10_LG_df <- full10_LG_df %>%
  rename(Sv_mean_10_LG = Sv_mean,
         NASC_10_LG = NASC,
         Depth_mean_10_LG = Depth_mean)%>%
  dplyr::select(-c(4,5))

#######pull 100X10 from 1m DZ files####################################################################

Full10_1m_path <- "odata/Transect 1/100x10/1mDZ"

# get a list of file names
file_names <- list.files(Full10_1m_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full10_1m_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full10_1m_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  full10_1m_df <- rbind(full10_1m_df, file)
}

# now lets rename the columns to help us merge them.
full10_1m_df <- full10_1m_df %>%
  rename(Sv_mean_10_1m = Sv_mean,
         NASC_10_1m = NASC,
         Depth_mean_10_1m = Depth_mean)%>%
  dplyr::select(-c(4,5))

#######pull 100X10 from Manual DZ files#######################################################################

Full10_MAN_path <- "odata/Transect 1/100x10/ManualDZ"

# get a list of file names
file_names <- list.files(Full10_MAN_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full10_MAN_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full10_MAN_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  full10_MAN_df <- rbind(full10_MAN_df, file)
}

# now lets rename the columns to help us merge them.
full10_MAN_df <- full10_MAN_df %>%
  rename(Sv_mean_10_MAN = Sv_mean,
         NASC_10_MAN = NASC,
         Depth_mean_10_MAN = Depth_mean)%>%
  dplyr::select(-c(4,5))

full10_allDZ_df<-left_join(full10_MAN_df, full10_1m_df, by="Site_ID")
full10_allDZ_df<-left_join(full10_allDZ_df, full10_LG_df, by= "Site_ID")

###############################################################################
########pull 100X5 from LG DZ files#######################################################################

Full5_LG_path <- "odata/Transect 1/100x5"

# get a list of file names
file_names <- list.files(Full5_LG_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full5_LG_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full5_LG_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  full5_LG_df <- rbind(full5_LG_df, file)
}

# now lets rename the columns to help us merge them.
full5_LG_df <- full5_LG_df %>%
  rename(Sv_mean_5_LG = Sv_mean,
         NASC_5_LG = NASC,
         Depth_mean_5_LG = Depth_mean)%>%
  dplyr::select(-c(4,5))

########pull 100X5 from 1m DZ files#######################################################################

Full5_1m_path <- "odata/Transect 1/100x5/1mDZ"

# get a list of file names
file_names <- list.files(Full5_1m_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


full5_1m_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full5_1m_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  full5_1m_df <- rbind(full5_1m_df, file)
}

# now lets rename the columns to help us merge them.
full5_1m_df <- full5_1m_df %>%
  rename(Sv_mean_5_1m = Sv_mean,
         NASC_5_1m = NASC,
         Depth_mean_5_1m = Depth_mean)%>%
  dplyr::select(-c(4,5))

########pull 100X5 from Manual DZ files#######################################################################

Full5_MAN_path <- "odata/Transect 1/100x5/ManualDZ"

# get a list of file names
file_names <- list.files(Full5_MAN_path)
file_names
# select only the csv files
csv_files <- file_names[grep("\\.csv$", file_names)]
csv_files
# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


Bin5_LG_df <- data.frame()
## for loop

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway
  file <- read.csv(file.path(Full5_MAN_path, file_name))
  
  # clean and subset the data to keep only what we need
  file <- file %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it
  Bin5_LG_df <- rbind(Bin5_LG_df, file)
}

# now lets rename the columns to help us merge them.
Bin5_LG_df <- Bin5_LG_df %>%
  rename(Sv_mean_5_MAN = Sv_mean,
         NASC_5_MAN = NASC,
         Depth_mean_5_MAN = Depth_mean)%>%
  dplyr::select(-c(4,5))

full5_allDZ_df<-left_join(Bin5_LG_df, full5_1m_df, by="Site_ID")
full5_allDZ_df<-left_join(full5_allDZ_df, full5_LG_df, by= "Site_ID")
siteNASC<-left_join(full5_allDZ_df, full10_allDZ_df, by= "Site_ID")
siteNASC<-left_join(siteNASC, full15_allDZ_df, by= "Site_ID")


##############################################################################
##########pull 20x5 from Large DZ ####
#### Now lets try for the 20x5 data which will be more complicated because anytime the deadzone is about a line it wont have that interval. 
## for loop 

## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 
#Bin5_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/20x5"

Bin5_path <- "odata/Transect 1/20x5"
# get a list of file names 
file_names <- list.files(Bin5_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


bin5_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin5_path, file_name), na.strings = c("", "NA"))
  
  # clean and subset the data to keep only what we need 
  file <- file %>%
    dplyr::select("Interval", "Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # make sure there is 5 intervals for ever site - fill empty bins with NA values 
  intervals <- 1:5
  if (!all(intervals %in% file$Interval)) {
    missing_intervals <- setdiff(intervals, unique(file$Interval))
    missing_data <- data.frame(Interval = missing_intervals)
    file <- merge(file, missing_data, all = TRUE)
  }
  
  # order the data by the interval number 
  file <- file[order(file$Interval), ]
  
  # write column with containing the site_IDs 
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it 
  bin5_df <- rbind(bin5_df, file)
}
## lets add in the BinID 
bin5_df$BinID <- paste(bin5_df$Site_ID, bin5_df$Interval, sep = "_")
bin5_df <- bin5_df %>%
  rename(Sv_mean_5 = Sv_mean,
         NASC_5 = NASC,
         Depth_mean_5 = Depth_mean,
         Layer_depth_min_5 = Layer_depth_min,
         Layer_depth_max_5 = Layer_depth_max)


##########pull 20X10 from Large DZ #### 

## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 
#Hutton path
#Bin10_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/20x10"

Bin10_path <- "odata/Transect 1/20x10"
# get a list of file names 
file_names <- list.files(Bin10_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


bin10_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin10_path, file_name), na.strings = c("", "NA"))
  
  # clean and subset the data to keep only what we need 
  file <- file %>%
    dplyr::select("Interval", "Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # make sure there is 5 intervals for ever site - fill empty bins with NA values 
  intervals <- 1:5
  if (!all(intervals %in% file$Interval)) {
    missing_intervals <- setdiff(intervals, unique(file$Interval))
    missing_data <- data.frame(Interval = missing_intervals)
    file <- merge(file, missing_data, all = TRUE)
  }
  
  # order the data by the interval number 
  file <- file[order(file$Interval), ]
  
  # write column with containing the site_IDs 
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it 
  bin10_df <- rbind(bin10_df, file)
}
## lets add in the BinID 
bin10_df$BinID <- paste(bin10_df$Site_ID, bin10_df$Interval, sep = "_")
bin10_df <- bin10_df %>%
  rename(Sv_mean_10 = Sv_mean,
         NASC_10 = NASC,
         Depth_mean_10 = Depth_mean, 
         Layer_depth_min_10 = Layer_depth_min,
         Layer_depth_max_10 = Layer_depth_max)
bin10totals<-bin10_df%>%
  count(Site_ID)

##########pull 20X15 from Large DZ #### 

## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 
#Hutton path
#Bin15_path <- "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/BIOSONIC/Analysis/Exports/Transect 1/20x15"

Bin15_path <- "odata/Transect 1/20x15"

# get a list of file names 
file_names <- list.files(Bin15_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


bin15_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin15_path, file_name), na.strings = c("", "NA"))
  
  # clean and subset the data to keep only what we need 
  file <- file %>%
    dplyr::select("Interval", "Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # make sure there is 5 intervals for ever site - fill empty bins with NA values 
  intervals <- 1:5
  if (!all(intervals %in% file$Interval)) {
    missing_intervals <- setdiff(intervals, unique(file$Interval))
    missing_data <- data.frame(Interval = missing_intervals)
    file <- merge(file, missing_data, all = TRUE)
  }
  
  # order the data by the interval number 
  file <- file[order(file$Interval), ]
  
  # write column with containing the site_IDs 
  file$Site_ID <- site_id
  
  # create a df what has all the data from each file in it 
  bin15_df <- rbind(bin15_df, file)
}
## lets add in the BinID 
bin15_df$BinID <- paste(bin15_df$Site_ID, bin15_df$Interval, sep = "_")
bin15_df <- bin15_df %>%
  rename(Sv_mean_15 = Sv_mean,
         NASC_15 = NASC,
         Depth_mean_15 = Depth_mean,
         Layer_depth_min_15 = Layer_depth_min,
         Layer_depth_max_15 = Layer_depth_max )


#### Complete 20m dataframe #### 
#bin15_df <- bin15_df %>% select("Site_ID", "BinID", "Sv_mean_15","NASC_15","Depth_mean_15","Layer_depth_min","Layer_depth_max")
#bin10_df <- bin15_df %>% select("Site_ID", "BinID", "Sv_mean_15","NASC_15","Depth_mean_15",
                                
names(bin15_df)
bin_df <- merge(bin15_df, bin10_df, by = c("Site_ID", "Interval", "BinID" ))
bin_df <- merge(bin_df, bin5_df, by = c("Site_ID", "Interval", "BinID"))
#view(bin_df)


#save(bin_df, file = "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/bin_df.RData")
#save(full_df, file = "C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/full_df.RData")
save(bin_df, file = "wdata/bin_df.RData")
save(full_df, file = "wdata/full_df.RData")

