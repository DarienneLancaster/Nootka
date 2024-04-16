# By: Hutton Noth 
# Date: Fall 2023
## set working directory 
#setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")

## required packages 
lp("tidyverse")
lp("readxl")

## if you read in the file as a excel vs csv it will keep column class's proper for doing time calculations. 
nootkadata <- read_excel("odata/Nootka_Data_2023.xlsx")

head(nootkadata)

str(nootkadata)

## Convert the excel time to a time usable in R ## 
# I struggled getting time calculations because in time columns on excel there was written out NA's this caused the it to be chr not POSIXct 

## T2 and T3 times were loaded in as fractions instead of the format of the last one
## "0.41319444444444442": This is also a representation of time as a fraction of a day.
## RESOLVED: as some of the times are missing for T2 and T3 missing
## there was NA written into the excel file with made the dates show up as fractions and characters 
## removing NA from the excel file solved this issue

 
## Calculate the duration for transect for each transect ## 
nootkadata$T1_duration <- nootkadata$T1_End - nootkadata$T1_Start

nootkadata$T2_duration <- nootkadata$T2_End - nootkadata$T2_Start

nootkadata$T3_duration <- nootkadata$T3_End - nootkadata$T3_Start

## calculate the total time at site ## 
nootkadata$site_duration <- nootkadata$T3_End - nootkadata$T1_Start

## How long between the end of transect 1 to the ROV transect = time for the fish to move 
nootkadata$T2_T1_duration <- nootkadata$T2_Start - nootkadata$T1_End

## Depth differnences ## 
nootkadata$T1_DepthDif <- nootkadata$T1_Edepth - nootkadata$T1_Sdepth

nootkadata$T2_DepthDif <- nootkadata$T2_Edepth - nootkadata$T2_Sdepth

nootkadata$T3_DepthDif <- nootkadata$T3_Edepth - nootkadata$T3_Sdepth

## Lat and long conversion calculations ## 

# Load the required packages 
lp("dplyr")

# Function to convert DMS to decimal degrees
# we are keeping the degrees then pasting the minutes plus decimals of the minutes to the equation that divides them by 60
dm_to_decimal <- function(dm) {
  dm_parts <- strsplit(dm, "\\.")[[1]]
  degrees <- as.numeric(dm_parts[1])
  minutes <- as.numeric(paste0(dm_parts[2], ".", dm_parts[3]))
  
  decimal_degrees <- degrees + minutes/60
  return(decimal_degrees)
}

#'nootkadata' is your dataset, and 'Lat' is the DM column with decimal fraction of minute, this applys the function made above
#'creating a new column with the calculated values 
nootkadata <- nootkadata %>%
  mutate(Lat_Decimal = sapply(Lat, dm_to_decimal))


nootkadata <- nootkadata %>%
  mutate(Long_Decimal = sapply(Long, dm_to_decimal))


## Save Nootka data 
save(nootkadata, file = "nootkadata.Rdata")
write.csv(nootkadata,"wdata/nootkadata.csv")

######################################################################################################################################################

# Calculate summary statistics across specified columns
## identify the class 
class(nootkadata$T2_duration)
## the regular calculation of summary stats and standard deviation can not be conducted on difftime class
# Function to calculate standard deviation for difftime objects
sd_difftime <- function(x) {
  sd_numeric <- sd(as.numeric(x, units = "secs"), na.rm = TRUE)
  as.difftime(sd_numeric, units = "secs")
}

## run the summary stats but sub in the function above to help calculate 
summary_stats_time <- nootkadata %>%
  summarise(
    across(c(T1_duration, T2_duration, T3_duration, site_duration, T2_T1_duration), 
           list(Max = ~max(., na.rm = TRUE), 
                Min = ~min(., na.rm = TRUE), 
                Mean = ~mean(., na.rm = TRUE), 
                SD = ~sd_difftime(.))
    )
  )
## convert the columns into rows to be able to see better 
summary_stats_time %>% pivot_longer(everything(), names_to = "Statistic", values_to = "Value")

## run summary stats for depth calculations 
colnames(nootkadata)

summary_stats_depth <- nootkadata %>%
  summarise(
    across(c(T1_DepthDif,T2_DepthDif, T3_DepthDif), 
           list(Max = ~max(., na.rm = TRUE), 
                Min = ~min(., na.rm = TRUE), 
                Mean = ~mean(., na.rm = TRUE), 
                SD = ~sd(., na.rm = TRUE))
    )
  )

summary_stats_depth
summary_stats_depth %>% pivot_longer(everything(), names_to = "Statistic", values_to = "Value")


## another way to look at the data is to create a box plot for 

# Transform data to long format
nootkadata_long <- nootkadata %>%
  select(T1_duration, T2_duration, T3_duration, site_duration, T2_T1_duration) %>%
  pivot_longer(everything(), names_to = "Duration_Type", values_to = "Value")

# Create boxplot
siteduration <- ggplot(nootkadata_long, aes(x = Duration_Type, y = Value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Duration at sites", y = "Duration (seconds)", x = "Duration Type")

siteduration



