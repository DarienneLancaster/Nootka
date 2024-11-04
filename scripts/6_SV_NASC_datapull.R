##work with NASC exports to calculate NASC for each deadzone buffer and rocky reef fish zone (RFZ) combination

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

save(full15_1m_df, file = "wdata/full15_1m_df.RData")

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

save(full10_1m_df, file = "wdata/full10_1m_df.RData")
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
#create full site dataframe with all deadzone values (Large, manual, 1m) for all rockfish zones (15,10,5)
siteNASC<-left_join(full5_allDZ_df, full10_allDZ_df, by= "Site_ID")
siteNASC<-left_join(siteNASC, full15_allDZ_df, by= "Site_ID")


##############################################################################
##########pull 20x5 from Large DZ ####
#### Now lets try for the 20x5 data which will be more complicated because anytime the deadzone is about a line it wont have that interval. 
## for loop 

## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 

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
  rename(Sv_mean_5_LG = Sv_mean,
         NASC_5_LG = NASC,
         Depth_mean_5_LG = Depth_mean,
         Layer_depth_min_5_LG = Layer_depth_min,
         Layer_depth_max_5_LG = Layer_depth_max)%>%
  dplyr::select(-c(5,6))



##########pull 20X5 from 1m DZ #### 
Bin5_1m_path <- "odata/Transect 1/20x5/1m"
# get a list of file names 
file_names <- list.files(Bin5_1m_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


Bin5_1m_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin5_1m_path, file_name), na.strings = c("", "NA"))
  
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
  Bin5_1m_df <- rbind(Bin5_1m_df, file)
}
## lets add in the BinID 
Bin5_1m_df$BinID <- paste(Bin5_1m_df$Site_ID, Bin5_1m_df$Interval, sep = "_")
Bin5_1m_df <- Bin5_1m_df %>%
  rename(Sv_mean_5_1m = Sv_mean,
         NASC_5_1m = NASC,
         Depth_mean_5_1m = Depth_mean,
         Layer_depth_min_5_1m = Layer_depth_min,
         Layer_depth_max_5_1m = Layer_depth_max)%>%
  dplyr::select(-c(5,6))

##########pull 20X5 from Manual DZ #### 
Bin5_MAN_path <- "odata/Transect 1/20x5/ManualDZ"
# get a list of file names 
file_names <- list.files(Bin5_MAN_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


Bin5_MAN_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin5_MAN_path, file_name), na.strings = c("", "NA"))
  
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
  Bin5_MAN_df <- rbind(Bin5_MAN_df, file)
}
## lets add in the BinID 
Bin5_MAN_df$BinID <- paste(Bin5_MAN_df$Site_ID, Bin5_MAN_df$Interval, sep = "_")
Bin5_MAN_df <- Bin5_MAN_df %>%
  rename(Sv_mean_5_MAN = Sv_mean,
         NASC_5_MAN = NASC,
         Depth_mean_5_MAN = Depth_mean,
         Layer_depth_min_5_MAN = Layer_depth_min,
         Layer_depth_max_5_MAN = Layer_depth_max)%>%
  dplyr::select(-c(5,6))



## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 

##########pull 20X10 from Large DZ #### 
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
  rename(Sv_mean_10_LG = Sv_mean,
         NASC_10_LG = NASC,
         Depth_mean_10_LG = Depth_mean, 
         Layer_depth_min_10_LG = Layer_depth_min,
         Layer_depth_max_10_LG = Layer_depth_max)%>%
  dplyr::select(-c(5,6))


##########pull 20X10 from 1m DZ #### 
Bin10_1m_path <- "odata/Transect 1/20x10/1m"
# get a list of file names 
file_names <- list.files(Bin10_1m_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


Bin10_1m_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin10_1m_path, file_name), na.strings = c("", "NA"))
  
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
  Bin10_1m_df <- rbind(Bin10_1m_df, file)
}
## lets add in the BinID 
Bin10_1m_df$BinID <- paste(Bin10_1m_df$Site_ID, Bin10_1m_df$Interval, sep = "_")
Bin10_1m_df <- Bin10_1m_df %>%
  rename(Sv_mean_10_1m = Sv_mean,
         NASC_10_1m = NASC,
         Depth_mean_10_1m = Depth_mean, 
         Layer_depth_min_10_1m = Layer_depth_min,
         Layer_depth_max_10_1m = Layer_depth_max)%>%
  dplyr::select(-c(5,6))


##########pull 20X10 from Manual DZ #### 
Bin10_MAN_path <- "odata/Transect 1/20x10/ManualDZ"
# get a list of file names 
file_names <- list.files(Bin10_MAN_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


Bin10_MAN_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin10_MAN_path, file_name), na.strings = c("", "NA"))
  
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
  Bin10_MAN_df <- rbind(Bin10_MAN_df, file)
}
## lets add in the BinID 
Bin10_MAN_df$BinID <- paste(Bin10_MAN_df$Site_ID, Bin10_MAN_df$Interval, sep = "_")
Bin10_MAN_df <- Bin10_MAN_df %>%
  rename(Sv_mean_10_MAN = Sv_mean,
         NASC_10_MAN = NASC,
         Depth_mean_10_MAN = Depth_mean, 
         Layer_depth_min_10_MAN = Layer_depth_min,
         Layer_depth_max_10_MAN = Layer_depth_max)%>%
  dplyr::select(-c(5,6))

## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 

##########pull 20X15 from Large DZ #### 

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
  rename(Sv_mean_15_LG = Sv_mean,
         NASC_15_LG = NASC,
         Depth_mean_15_LG = Depth_mean,
         Layer_depth_min_15_LG = Layer_depth_min,
         Layer_depth_max_15_LG = Layer_depth_max )%>%
  dplyr::select(-c(5,6))

##########pull 20X15 from 1m DZ #### 

Bin15_1m_path <- "odata/Transect 1/20x15/1m Deadzone"

# get a list of file names 
file_names <- list.files(Bin15_1m_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


Bin15_1m_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin15_1m_path, file_name), na.strings = c("", "NA"))
  
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
  Bin15_1m_df <- rbind(Bin15_1m_df, file)
}
## lets add in the BinID 
Bin15_1m_df$BinID <- paste(Bin15_1m_df$Site_ID, Bin15_1m_df$Interval, sep = "_")
Bin15_1m_df <- Bin15_1m_df %>%
  rename(Sv_mean_15_1m = Sv_mean,
         NASC_15_1m = NASC,
         Depth_mean_15_1m = Depth_mean,
         Layer_depth_min_15_1m = Layer_depth_min,
         Layer_depth_max_15_1m = Layer_depth_max )%>%
  dplyr::select(-c(5,6))

##########pull 20X15 from Manual DZ #### 

Bin15_MAN_path <- "odata/Transect 1/20x15/ManualDZ"

# get a list of file names 
file_names <- list.files(Bin15_MAN_path)

# select only the csv files 
csv_files <- file_names[grep("\\.csv$", file_names)]

# make it into a dataframe
file_df <- data.frame(file_name = csv_files)

# create a column in the dataframe for site ID 
file_df$Site_ID <- substr(file_df$file_name, 1, 4)


Bin15_MAN_df <- data.frame()

# loop each row in the file_df that contains al the names of files of interest
for (i in 1:nrow(file_df)) {
  # set the file name and site ID 
  file_name <- file_df$file_name[i]
  site_id <- file_df$Site_ID[i]
  
  # pull in files from our st pathway 
  file <- read.csv(file.path(Bin15_MAN_path, file_name), na.strings = c("", "NA"))
  
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
  Bin15_MAN_df <- rbind(Bin15_MAN_df, file)
}
## lets add in the BinID 
Bin15_MAN_df$BinID <- paste(Bin15_MAN_df$Site_ID, Bin15_MAN_df$Interval, sep = "_")
Bin15_MAN_df <- Bin15_MAN_df %>%
  rename(Sv_mean_15_MAN = Sv_mean,
         NASC_15_MAN = NASC,
         Depth_mean_15_MAN = Depth_mean,
         Layer_depth_min_15_MAN = Layer_depth_min,
         Layer_depth_max_15_MAN = Layer_depth_max )%>%
  dplyr::select(-c(5,6))


#### Complete 20m dataframe #### 
#bin15_df <- bin15_df %>% select("Site_ID", "BinID", "Sv_mean_15","NASC_15","Depth_mean_15","Layer_depth_min","Layer_depth_max")
#bin10_df <- bin15_df %>% select("Site_ID", "BinID", "Sv_mean_15","NASC_15","Depth_mean_15",

#combine binned data into one dataframe                                
#15m data
bin_df1 <- merge(bin15_df, Bin15_1m_df, by = c("Site_ID", "Interval", "BinID" ))
bin_df2 <- merge(bin_df1, Bin15_MAN_df, by = c("Site_ID", "Interval", "BinID" ))
#10m data
bin_df3 <- merge(bin10_df, Bin10_1m_df, by = c("Site_ID", "Interval", "BinID" ))
bin_df4 <- merge(bin_df3, Bin10_MAN_df, by = c("Site_ID", "Interval", "BinID" ))
#5m data
bin_df5 <- merge(bin5_df, Bin5_1m_df, by = c("Site_ID", "Interval", "BinID" ))
bin_df6 <- merge(bin_df5, Bin5_MAN_df, by = c("Site_ID", "Interval", "BinID" ))

bin_df7 <- merge(bin_df2, bin_df4, by = c("Site_ID", "Interval", "BinID" ))
binNASC <- merge(bin_df7, bin_df6, by = c("Site_ID", "Interval", "BinID" ))

#view(bin_df)

save(binNASC, file = "wdata/binNASC.RData")
write.csv(binNASC, "wdata/binNASC.csv")
save(siteNASC, file = "wdata/siteNASC.RData")
write.csv(siteNASC, "wdata/siteNASc.csv")

#### Transect 3 ####
#### 100x15 ####
Full15_path_t3 <- "odata/Transect 3/100x15"

# get a list of file names 
file_names_t3 <- list.files(Full15_path_t3)

# select only the csv files 
csv_files_t3 <- file_names_t3[grep("\\.csv$", file_names_t3)]

# make it into a dataframe
file_df_t3 <- data.frame(file_name = csv_files_t3)

# create a column in the dataframe for site ID 
file_df_t3$Site_ID <- substr(file_df_t3$file_name, 1, 4)

full15_df_t3 <- data.frame()

# loop each row in the file_df that contains all the names of files of interest
for (i in 1:nrow(file_df_t3)) {
  # set the file name and site ID 
  file_name <- file_df_t3$file_name[i]
  site_id <- file_df_t3$Site_ID[i]
  
  # pull in files from our specified pathway 
  file_t3 <- read.csv(file.path(Full15_path_t3, file_name))
  
  # clean and subset the data to keep only what we need 
  file_t3 <- file_t3 %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs 
  file_t3$Site_ID <- site_id
  
  # create a df that has all the data from each file in it 
  full15_df_t3 <- rbind(full15_df_t3, file_t3)
}

# now lets rename the columns to help us merge them. 
full15_df_t3 <- full15_df_t3 %>%
  rename(Sv_mean_15_t3 = Sv_mean,
         NASC_15_t3 = NASC,
         Depth_mean_15_t3 = Depth_mean)



#### 100x10 #### 
## we want to create a dataframe for all the # specify the path on the computer to the folder with the files you want to run through the code 

Full10_path_t3 <- "odata/Transect 3/100x10"

# get a list of file names 
file_names_t3 <- list.files(Full10_path_t3)

# select only the csv files 
csv_files_t3 <- file_names_t3[grep("\\.csv$", file_names_t3)]

# make it into a dataframe
file_df_t3 <- data.frame(file_name = csv_files_t3)

# create a column in the dataframe for site ID 
file_df_t3$Site_ID <- substr(file_df_t3$file_name, 1, 4)

full10_df_t3 <- data.frame()

# loop each row in the file_df that contains all the names of files of interest
for (i in 1:nrow(file_df_t3)) {
  # set the file name and site ID 
  file_name <- file_df_t3$file_name[i]
  site_id <- file_df_t3$Site_ID[i]
  
  # pull in files from our specified pathway 
  file_t3 <- read.csv(file.path(Full10_path_t3, file_name))
  
  # clean and subset the data to keep only what we need 
  file_t3 <- file_t3 %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs 
  file_t3$Site_ID <- site_id
  
  # create a df that has all the data from each file in it 
  full10_df_t3 <- rbind(full10_df_t3, file_t3)
}

# now lets rename the columns to help us merge them. 
full10_df_t3 <- full10_df_t3 %>%
  rename(Sv_mean_10_t3 = Sv_mean,
         NASC_10_t3 = NASC,
         Depth_mean_10_t3 = Depth_mean)

#### 100x5 #### 
Full5_path_t3 <- "odata/Transect 3/100x5"

# get a list of file names 
file_names_t3 <- list.files(Full5_path_t3)

# select only the csv files 
csv_files_t3 <- file_names_t3[grep("\\.csv$", file_names_t3)]

# make it into a dataframe
file_df_t3 <- data.frame(file_name = csv_files_t3)

# create a column in the dataframe for site ID 
file_df_t3$Site_ID <- substr(file_df_t3$file_name, 1, 4)

full5_df_t3 <- data.frame()

# loop each row in the file_df that contains all the names of files of interest
for (i in 1:nrow(file_df_t3)) {
  # set the file name and site ID 
  file_name <- file_df_t3$file_name[i]
  site_id <- file_df_t3$Site_ID[i]
  
  # pull in files from our specified pathway 
  file_t3 <- read.csv(file.path(Full5_path_t3, file_name))
  
  # clean and subset the data to keep only what we need 
  file_t3 <- file_t3 %>%
    dplyr::select("Sv_mean", "NASC", "Depth_mean", "Layer_depth_min", "Layer_depth_max")
  
  # write column with containing the site_IDs 
  file_t3$Site_ID <- site_id
  
  # create a df that has all the data from each file in it 
  full5_df_t3 <- rbind(full5_df_t3, file_t3)
}

# now lets rename the columns to help us merge them. 
full5_df_t3 <- full5_df_t3 %>%
  rename(Sv_mean_5_t3 = Sv_mean,
         NASC_5_t3 = NASC,
         Depth_mean_5_t3 = Depth_mean)


#### Complete 100m dataframe #### 
full_df_t3 <- merge(full15_df_t3, full10_df_t3, by = c("Layer_depth_min", "Layer_depth_max", "Site_ID" ))
full_df_t3 <- merge(full_df_t3, full5_df_t3, by = c( "Layer_depth_min", "Layer_depth_max", "Site_ID" ))


save(full_df_t3, file = "wdata/full_df_t3.RData")

#proceed to script 7_analysis_GLMs_Spearmans.R

