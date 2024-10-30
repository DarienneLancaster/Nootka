#Deadzone height calculator

#### Load Packages #### 
lp<-function(pck){
  if(!require(pck,character.only = TRUE))install.packages(pck);library(pck,character.only = TRUE)
}
lp("dplyr")
lp("flextable")


#Basic formula for calculating Deadzone height (see farther down for full code looping through a dataframe)
#Formula Source: Tuser et al (2011)
#Validation of current acoustic dead-zone estimation methods in lakes with 
#strongly sloped bottoms 
#https://aslopubs.onlinelibrary.wiley.com/doi/pdfdirect/10.4319/lom.2011.9.507

# #formula
# #slope degree
# slope <-40
# #half beam angle in degrees
# hba<- 7
# #depth
# depth<- 6
# #pulse length (seconds)
# pl<-0.0001
# #sound speed in water
# c<-1450
# 
# #slope to radians(beta)
# beta<-slope*pi/180
# 
# #half beam angle (hba) to radians (theta)
# theta<-hba*pi/180
# 
# #theoretical worst case scenario deadzone heigh
# DZ<-(depth*(((1/(cos(beta)))-1))+((c*pl)/2))
# DZ
# 
# #Deadzone (accounting for half beam angle)
# DZ2<-(depth*(((cos(theta-beta))/(cos(beta)))-1))+((c*pl)/2)
# DZ2

####################################################################
###load manual habitat annotations to calculate mean rugosity height

#####load and process format substrate annotations from ROV videos####
ROVsub <-read.csv("odata/Manual_Habitat_Annotations_Nootka_20240708_FINAL.csv",  header = TRUE)
str(ROVsub)

#create new column with substrate height values that correspond to midpoint values from 
#wentworth scale (e.g. Boulders equal 25-50 cm so mid point is 37.5cm)
ROVsub$Rug_height <- ifelse(ROVsub$Rugosity == 1, 12.5,
                            ifelse(ROVsub$Rugosity == 2, 37.5,
                                   ifelse(ROVsub$Rugosity == 3, 100, NA))) #100 picked for large rugosity value as max boulder size is likely ~1.5m high so mid between 50cm and 150 cm is 100

#average rugosity height by site
Rug_height_site <- ROVsub %>%
  group_by(Site) %>%
  summarise(mean_Rug_height = mean(Rug_height, na.rm = TRUE))%>%
  mutate(Rug_height = mean_Rug_height/100)%>%
  dplyr::select(Site, Rug_height)%>%
  rename(Site_ID = Site)

#load site data from echosounder
load("wdata/site_DE.RData")

Echo_depth_slope<-site_DE%>%
  dplyr::select(Site_ID, Average_5m_slope, Average_Depth)

DZdata<-left_join(Echo_depth_slope, Rug_height_site, by="Site_ID")

#create adjusted depth based on rugosity offset
DZdata$depth_adjust<-DZdata$Average_Depth-DZdata$Rug_height

#DZdata is for mean depth and slope information per site


#######################################################################
#calculate DZ height for max and min depth and slopes

#import depth and slopes for 5m increments across all sites
AllDepthSlope<-read.csv("wdata/BEslope.csv")

#calculate max 5m depth bin for each site
MaxDepth<- AllDepthSlope%>%
  filter(!is.na(Slope5m))%>% #remove NAs
  mutate(Slope5m =abs(Slope5m))%>%
  slice(-1)%>% # get rid of weird first row
  group_by(Site_ID)%>%
  slice(c(which.max(Depth)))%>%
  rename(Max_Depth=Depth, Average_5m_slope=Slope5m)%>%
  dplyr::select(Site_ID,Max_Depth,Average_5m_slope)%>%
  ungroup()

#calculate min 5m depth bin for each site
MinDepth<- AllDepthSlope%>%
  filter(!is.na(Slope5m))%>% #remove NAs
  mutate(Slope5m =abs(Slope5m))%>%
  slice(-1)%>% # get rid of weird first row
  group_by(Site_ID)%>%
  slice(c(which.min(Depth)))%>%
  rename(Min_Depth=Depth, Average_5m_slope=Slope5m)%>%
  dplyr::select(Site_ID,Min_Depth,Average_5m_slope)%>%
  ungroup()


##########################################################################
#loop dataframe through formulas to calculate deadzone height for mean depth and mean slope at each site

#input values for analysis
#loop through each of DZdata, MaxDepth, an MinDepth

#dataframe name
data<-DZdata

#half beam angle in degrees (this is half your transducer beam width)
hba<- 5 ###change this value depending on your transducer (for 38kHz transducer with 10 degree = 5degree half beam)

#pulse length (seconds)
pl<-0.0004

#sound speed in water
c<-1450

########loop to calculate beta###########

# Create a column for beta with NA values
data$beta <- NA  
#create list of unique site IDs
unique_sites<-unique(data$Site_ID) 

# Loop through each unique Site_ID
for (i in unique_sites) {
  # Filter the dataframe for the current Site_ID
  site_data <- data[data$Site_ID == i, ]
  
  # Calculate the average slope for this Site_ID
  slope <- site_data$Average_5m_slope
  
  # Calculate beta in radians
  beta <- slope * pi / 180
  
  # Update the beta column for this Site_ID
  data$beta[data$Site_ID == i] <- beta
}

#########loop to calculate theta########
data$theta <- NA  # Create a column for theta with NA values

# Loop through each unique Site_ID
for (i in unique_sites) {
  # Filter the dataframe for the current Site_ID
  site_data <- data[data$Site_ID == i, ]
  
  # Calculate beta in radians
  theta<-hba*pi/180
  
  # Update the beta column for this Site_ID
  data$theta[data$Site_ID == i] <- theta
}

#########loop to calculate deadzone height (worst case)########
#worst case does not account for half beam angle (see Tuser paper)

data$DZworst <- NA  # Create a column for deadzone worst case scenario with NA values

# Loop through each unique Site_ID
for (i in unique(data$Site_ID)) {
  # Filter the dataframe for the current Site_ID
  site_data <- data[data$Site_ID == i, ]
  
  # Check if site_data is not empty
  if (nrow(site_data) > 0) {
    # Theoretical worst case scenario deadzone height
    ####make sure you depth column name is correct to your dataframe here
    DZworst <- (site_data$depth_adjust[1] * (((1 / (cos(site_data$beta[1]))) - 1)) + ((c * pl) / 2))
    
    # Update the DZworst column for this Site_ID
    data$DZworst[data$Site_ID == i] <- DZworst
  }
}

#########loop to calculate deadzone height (best case)########
#this loop accounts for half beam angle and gives more accurate deadzone height estimate

data$DZbest <- NA  # Create a column for beta with NA values

# Loop through each unique Site_ID
for (i in unique(data$Site_ID)) {
  # Filter the dataframe for the current Site_ID
  site_data <- data[data$Site_ID == i, ]
  
  # Check if site_data is not empty
  if (nrow(site_data) > 0) {
    
    # Theoretical worst case scenario deadzone height
    ####make sure you depth column name is correct to your dataframe here
    DZbest<-(site_data$depth_adjust[1]*(((cos(site_data$theta[1]-site_data$beta[1]))/(cos(site_data$beta[1])))-1))+((c*pl)/2)
    
    # Update the DZbest column for this Site_ID
    data$DZbest[data$Site_ID == i] <- DZbest
  }
}

#rename data for specific analysis
DZdataDZ<-data

#export data
# write.csv(DZdataDZ, "wdata/DZdataDZ.csv")
# write.csv(MaxDepthDZ, "wdata/MaxDepthDZ.csv")
# write.csv(MinDepthDZ, "wdata/MinDepthDZ.csv")

##########################################################################
#pull data into a summary table
DZdataDZ <-read.csv("wdata/DZdataDZ.csv",  header = TRUE)
MaxDepthDZ <- read.csv("wdata/MaxDepthDZ.csv",  header = TRUE)
MinDepthDZ <- read.csv("wdata/MinDepthDZ.csv",  header = TRUE)

#pull max deadzone calculated from deepest depths
MaxDeets<-MaxDepthDZ%>%
  slice(c(which.max(DZbest)))%>%
  rename(Depth = Max_Depth, Slope = Average_5m_slope)%>%
  dplyr::select(DZbest, Depth, Slope)

#pull min deadzone calculated from shallowest depths
MinDeets<-MinDepthDZ%>%
  slice(c(which.min(DZbest)))%>%
  rename(Depth = Min_Depth, Slope = Average_5m_slope)%>%
  dplyr::select(DZbest, Depth, Slope)

#calculate mean deadzone, depth, and slope across all sites (site data already averaged across transect)
MeanDeets<-DZdataDZ%>%
  summarise(
    DZbest = mean(DZbest, na.rm = TRUE),
    Depth = mean(depth_adjust, na.rm = TRUE),
    Slope = mean(Average_5m_slope, na.rm = TRUE)
  )

#calculate standard deviation of deadzone, depth, and slope across all sites (site data already averaged across transect)
SDDeets<-DZdataDZ%>%
  summarise(
    DZbest = sd(DZbest, na.rm = TRUE),
    Depth = sd(depth_adjust, na.rm = TRUE),
    Slope = sd(Average_5m_slope, na.rm = TRUE)
  )

# Combine the four data frames
CombinedDeets <- bind_rows(
  MinDeets %>% mutate(Type = "Minimum"),
  MaxDeets %>% mutate(Type = "Maximum"),
  MeanDeets %>% mutate(Type = "Mean"),
  SDDeets %>% mutate(Type = "SD"),
)%>%
  rename(Transect_Depth = Type, Deadzone_Height = DZbest)%>%
  mutate(across(where(is.numeric), ~ round(., 2)))

#rearrange columns
CombinedDeets <- CombinedDeets[, c(4, 1, 2, 3)]
flextable(CombinedDeets)

#export flextable into word for formatting

lp("officer")
help(package="officer")

# Create a new Word document
doc <- read_docx()

# Add the flextable
doc <- doc %>%
  body_add(CombinedDeets)

# Save the Word document
print(doc, target = "figures/deadzone_height.docx")

###summarize data for Min, Max, and Mean deadzone height calculations
# MinSum <- MinDepth %>%
#   summarize(
#     Mean_DZbest = mean(DZbest, na.rm = TRUE),
#     Max_DZbest = max(DZbest, na.rm = TRUE),
#     Min_DZbest = min(DZbest, na.rm = TRUE),
#     SD_DZbest = sd(DZbest, na.rm = TRUE),
#     .groups = 'drop'  # Optional: Ungroup after summarizing
#   )
# 
# MaxSum <- MaxDepth %>%
#   summarize(
#     Mean_DZbest = mean(DZbest, na.rm = TRUE),
#     Max_DZbest = max(DZbest, na.rm = TRUE),
#     Min_DZbest = min(DZbest, na.rm = TRUE),
#     SD_DZbest = sd(DZbest, na.rm = TRUE),
#     .groups = 'drop'  # Optional: Ungroup after summarizing
#   )
# 
# MeanSum <- DZdata %>%
#   summarize(
#     Mean_DZbest = mean(DZbest, na.rm = TRUE),
#     Max_DZbest = max(DZbest, na.rm = TRUE),
#     Min_DZbest = min(DZbest, na.rm = TRUE),
#     SD_DZbest = sd(DZbest, na.rm = TRUE),
#     .groups = 'drop'  # Optional: Ungroup after summarizing
#   )
# 
# # Add a new column indicating the source data frame
# MaxSum <- MaxSum %>% mutate(Source = "MaxDepth")
# MinSum <- MinSum %>% mutate(Source = "MinDepth")
# MeanSum <- MeanSum %>% mutate(Source = "MeanDepth")
# 
# Sum<-bind_rows(MaxSum, MinSum, MeanSum)
# flextable(Sum)
###this table is a bit over elaborate, make simpler one


