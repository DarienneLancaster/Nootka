## use package to install and 
lp("tidyverse")
lp("readr")
lp("wesanderson")
lp("here")
lp("ggplot2")
lp("dplyr")
lp("readxl")
lp("tidyr")
lp("lubridate")




nootkadata <- read_excel('C:\\Users\\HuttonNoth(HFS)\\OneDrive - Ha’oom Fisheries Society\\Hutton\\Nootka_August2023\\Nootka_data_2023.xlsx')
head(nootkadata)

## Convert the excel time to a time usable in R ## 
## requires the package library(lubridate) ## 
##"1899-12-31 11:36:00": Excel stores date and time values as a numeric value where the whole number 
##part represents the number of days since January 1, 1900, and the decimal part represents the time as a fraction of a day.
##In this format, "1899-12-31 11:36:00" represents a time value with 11 hours and 36 minutes on December 31, 1899. 
##When you import this value into R, it recognizes it as a date-time format.

nootkadata$T1_Start <- as.POSIXct(nootkadata$T1_Start, format = "%H:%M:%S")

nootkadata$T1_End <- as.POSIXct(nootkadata$T1_End, format = "%H:%M:%S")

## T2 and T3 times were loaded in as fractions instead of the format of the last one
## "0.41319444444444442": This is also a representation of time as a fraction of a day.
## RESOLVED: as some of the times are missing for T2 and T3 missing
## there was NA written into the excel file with made the dates show up as fractions and characters 
## removing NA from the excel file solved this issue

## class(nootkadata$T2_Start)

## The time is stored as a charcter and we need to convert it to numeric ## 

# nootkadata$T2_Start < as.numeric(nootkadata$T2_Start)
# nootkadata$T2_End <- as.numeric(nootkadata$T2_End)
# class(nootkadata$T2_Start)
# nootkadata$T2_Start_Seconds <- nootkadata$T2_Start * 86400
# nootkadata$T2_End_Seconds <- nootkadata$T2_End * 86400

# Create a function to format seconds as HH:MM:SS
#format_seconds <- function(seconds) {
#  h <- floor(seconds / 3600)
# m <- floor((seconds %% 3600) / 60)
#  s <- seconds %% 60
#  return(sprintf("%02d:%02d:%02d", h, m, s))
#}
 
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

# Load the required library
library(dplyr)

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

# Assuming 'nootkadata' is your dataset, and 'Lat' is the DM column with decimal fraction of minutes
nootkadata <- nootkadata %>%
  mutate(Lat_Decimal = sapply(Lat, dm_to_decimal))


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
  labs(title = "Duration at sites", y = "Duration (secounds)", x = "Duration Type")

siteduration
