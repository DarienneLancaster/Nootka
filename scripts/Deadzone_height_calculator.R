#Deadzone height calculator

#load packages
lp("dplyr")


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

DZdata$depth_adjust<-DZdata$Average_Depth-DZdata$Rug_height

mean(DZdata$Average_5m_slope)
#16.68587
sd(DZdata$Average_5m_slope)
#6.479068

mean(DZdata$Average_Depth)
#38.04964
sd(DZdata$Average_Depth)
#9.544151


##########################################################################
#loop dataframe through formulas to calculate deadzone height for mean depth and mean slope at each site

########loop to calculate beta###########
DZdata$beta <- NA  # Create a column for beta with NA values
unique_sites<-unique(DZdata$Site_ID) 


# Loop through each unique Site_ID
for (i in unique_sites) {
  # Filter the dataframe for the current Site_ID
  site_data <- DZdata[DZdata$Site_ID == i, ]
  
  # Calculate the average slope for this Site_ID
  slope <- site_data$Average_5m_slope
  
  # Calculate beta in radians
  beta <- slope * pi / 180
  
  # Update the beta column for this Site_ID
  DZdata$beta[DZdata$Site_ID == i] <- beta
}

#########loop to calculate theta########
DZdata$theta <- NA  # Create a column for beta with NA values
unique_sites<-unique(DZdata$Site_ID)
#half beam angle in degrees (this is half your transducer beam width)
hba<- 5 ###change this value depending on your transducer (for 38kHz transducer with 10 degree = 5degree half beam)


# Loop through each unique Site_ID
for (i in unique_sites) {
  # Filter the dataframe for the current Site_ID
  site_data <- DZdata[DZdata$Site_ID == i, ]
  
  # Calculate beta in radians
  theta<-hba*pi/180
  
  # Update the beta column for this Site_ID
  DZdata$theta[DZdata$Site_ID == i] <- theta
}

#########loop to calculate deadzone height (worst case)########
DZdata$DZworst <- NA  # Create a column for beta with NA values

#pulse length (seconds)
pl<-0.0001
#sound speed in water
c<-1450

# Loop through each unique Site_ID
for (i in unique(DZdata$Site_ID)) {
  # Filter the dataframe for the current Site_ID
  site_data <- DZdata[DZdata$Site_ID == i, ]
  
  # Check if site_data is not empty
  if (nrow(site_data) > 0) {
    # Theoretical worst case scenario deadzone height
    DZworst <- (site_data$depth_adjust[1] * (((1 / (cos(site_data$beta[1]))) - 1)) + ((c * pl) / 2))
    
    # Update the DZworst column for this Site_ID
    DZdata$DZworst[DZdata$Site_ID == i] <- DZworst
  }
}

#########loop to calculate deadzone height (best case)########
DZdata$DZbest <- NA  # Create a column for beta with NA values

# Loop through each unique Site_ID
for (i in unique(DZdata$Site_ID)) {
  # Filter the dataframe for the current Site_ID
  site_data <- DZdata[DZdata$Site_ID == i, ]
  
  # Check if site_data is not empty
  if (nrow(site_data) > 0) {
    
    # Theoretical worst case scenario deadzone height
    DZbest<-(site_data$depth_adjust[1]*(((cos(site_data$theta[1]-site_data$beta[1]))/(cos(site_data$beta[1])))-1))+((c*pl)/2)
    
    # Update the DZbest column for this Site_ID
    DZdata$DZbest[DZdata$Site_ID == i] <- DZbest
  }
}

##########################
#calculate deadzone height accounting for height of boulders, cobble, etc... off bottom

DZdata$TotDZworst<-DZdata$DZworst+DZdata$Rug_height
mean(DZdata$TotDZworst)
mean(MaxDepth$Average_5m_slope)
sd(DZdata$TotDZworst)
DZdata$TotDZbest<-DZdata$DZbest+DZdata$Rug_height
mean(DZdata$TotDZbest)
sd(DZdata$TotDZbest)

#######################################################################
#calculate DZ height for max and min depth and slopes

AllDepthSlope<-read.csv("wdata/BEslope.csv")

#calculate max depth for each site
MaxDepth<- AllDepthSlope%>%
  filter(!is.na(Slope5m))%>% #remove NAs
  mutate(Slope5m =abs(Slope5m))%>%
  slice(-1)%>% # get rid of weird first row
  group_by(Site_ID)%>%
  slice(c(which.max(Depth)))%>%
  rename(Max_Depth=Depth, Average_5m_slope=Slope5m)%>%
  dplyr::select(Site_ID,Max_Depth,Average_5m_slope)%>%
  ungroup()

#calculate min depth for each site
MinDepth<- AllDepthSlope%>%
  filter(!is.na(Slope5m))%>% #remove NAs
  mutate(Slope5m =abs(Slope5m))%>%
  slice(-1)%>% # get rid of weird first row
  group_by(Site_ID)%>%
  slice(c(which.min(Depth)))%>%
  rename(Min_Depth=Depth, Average_5m_slope=Slope5m)%>%
  dplyr::select(Site_ID,Min_Depth,Average_5m_slope)%>%
  ungroup()


##########run max/min depths through DZ height loop#############

####MAX DEPTH DEADZONE####
########loop to calculate beta###########
MaxDepth$beta <- NA  # Create a column for beta with NA values
unique_sites<-unique(MaxDepth$Site_ID) 


# Loop through each unique Site_ID
for (i in unique_sites) {
  # Filter the dataframe for the current Site_ID
  site_data <- MaxDepth[MaxDepth$Site_ID == i, ]
  
  # Calculate the average slope for this Site_ID
  slope <- site_data$Average_5m_slope
  
  # Calculate beta in radians
  beta <- slope * pi / 180
  
  # Update the beta column for this Site_ID
  MaxDepth$beta[MaxDepth$Site_ID == i] <- beta
}

#########loop to calculate theta########
MaxDepth$theta <- NA  # Create a column for beta with NA values
unique_sites<-unique(MaxDepth$Site_ID)
#half beam angle in degrees (this is half your transducer beam width)
hba<- 5 ###change this value depending on your transducer (for 38kHz transducer with 10 degree = 5degree half beam)


# Loop through each unique Site_ID
for (i in unique_sites) {
  # Filter the dataframe for the current Site_ID
  site_data <- MaxDepth[MaxDepth$Site_ID == i, ]
  
  # Calculate beta in radians
  theta<-hba*pi/180
  
  # Update the beta column for this Site_ID
  MaxDepth$theta[MaxDepth$Site_ID == i] <- theta
}

#########loop to calculate deadzone height (worst case)########
MaxDepth$DZworst <- NA  # Create a column for beta with NA values

#pulse length (seconds)
pl<-0.0001
#sound speed in water
c<-1450

# Loop through each unique Site_ID
for (i in unique(MaxDepth$Site_ID)) {
  # Filter the dataframe for the current Site_ID
  site_data <- MaxDepth[MaxDepth$Site_ID == i, ]
  
  # Check if site_data is not empty
  if (nrow(site_data) > 0) {
    # Theoretical worst case scenario deadzone height
    DZworst <- (site_data$Max_Depth[1] * (((1 / (cos(site_data$beta[1]))) - 1)) + ((c * pl) / 2))
    
    # Update the DZworst column for this Site_ID
    MaxDepth$DZworst[MaxDepth$Site_ID == i] <- DZworst
  }
}

#########loop to calculate deadzone height (best case)########
MaxDepth$DZbest <- NA  # Create a column for beta with NA values

# Loop through each unique Site_ID
for (i in unique(MaxDepth$Site_ID)) {
  # Filter the dataframe for the current Site_ID
  site_data <- MaxDepth[MaxDepth$Site_ID == i, ]
  
  # Check if site_data is not empty
  if (nrow(site_data) > 0) {
    
    # Theoretical worst case scenario deadzone height
    DZbest<-(site_data$Max_Depth[1]*(((cos(site_data$theta[1]-site_data$beta[1]))/(cos(site_data$beta[1])))-1))+((c*pl)/2)
    
    # Update the DZbest column for this Site_ID
    MaxDepth$DZbest[MaxDepth$Site_ID == i] <- DZbest
  }
}


####MIN DEPTH DEADZONE####
########loop to calculate beta###########
MinDepth$beta <- NA  # Create a column for beta with NA values
unique_sites<-unique(MinDepth$Site_ID) 


# Loop through each unique Site_ID
for (i in unique_sites) {
  # Filter the dataframe for the current Site_ID
  site_data <- MinDepth[MinDepth$Site_ID == i, ]
  
  # Calculate the average slope for this Site_ID
  slope <- site_data$Average_5m_slope
  
  # Calculate beta in radians
  beta <- slope * pi / 180
  
  # Update the beta column for this Site_ID
  MinDepth$beta[MinDepth$Site_ID == i] <- beta
}

#########loop to calculate theta########
MinDepth$theta <- NA  # Create a column for beta with NA values
unique_sites<-unique(MinDepth$Site_ID)
#half beam angle in degrees (this is half your transducer beam width)
hba<- 5 ###change this value depending on your transducer (for 38kHz transducer with 10 degree = 5degree half beam)


# Loop through each unique Site_ID
for (i in unique_sites) {
  # Filter the dataframe for the current Site_ID
  site_data <- MinDepth[MinDepth$Site_ID == i, ]
  
  # Calculate beta in radians
  theta<-hba*pi/180
  
  # Update the beta column for this Site_ID
  MinDepth$theta[MinDepth$Site_ID == i] <- theta
}

#########loop to calculate deadzone height (worst case)########
MinDepth$DZworst <- NA  # Create a column for beta with NA values

#pulse length (seconds)
pl<-0.0001
#sound speed in water
c<-1450

# Loop through each unique Site_ID
for (i in unique(MinDepth$Site_ID)) {
  # Filter the dataframe for the current Site_ID
  site_data <- MinDepth[MinDepth$Site_ID == i, ]
  
  # Check if site_data is not empty
  if (nrow(site_data) > 0) {
    # Theoretical worst case scenario deadzone height
    DZworst <- (site_data$Min_Depth[1] * (((1 / (cos(site_data$beta[1]))) - 1)) + ((c * pl) / 2))
    
    # Update the DZworst column for this Site_ID
    MinDepth$DZworst[MinDepth$Site_ID == i] <- DZworst
  }
}

#########loop to calculate deadzone height (best case)########
MinDepth$DZbest <- NA  # Create a column for beta with NA values

# Loop through each unique Site_ID
for (i in unique(MinDepth$Site_ID)) {
  # Filter the dataframe for the current Site_ID
  site_data <- MinDepth[MinDepth$Site_ID == i, ]
  
  # Check if site_data is not empty
  if (nrow(site_data) > 0) {
    
    # Theoretical worst case scenario deadzone height
    DZbest<-(site_data$Min_Depth[1]*(((cos(site_data$theta[1]-site_data$beta[1]))/(cos(site_data$beta[1])))-1))+((c*pl)/2)
    
    # Update the DZbest column for this Site_ID
    MinDepth$DZbest[MinDepth$Site_ID == i] <- DZbest
  }
}

MinSum <- MinDepth %>%
  summarize(
    Mean_DZbest = mean(DZbest, na.rm = TRUE),
    Max_DZbest = max(DZbest, na.rm = TRUE),
    Min_DZbest = min(DZbest, na.rm = TRUE),
    SD_DZbest = sd(DZbest, na.rm = TRUE),
    .groups = 'drop'  # Optional: Ungroup after summarizing
  )

MaxSum <- MaxDepth %>%
  summarize(
    Mean_DZbest = mean(DZbest, na.rm = TRUE),
    Max_DZbest = max(DZbest, na.rm = TRUE),
    Min_DZbest = min(DZbest, na.rm = TRUE),
    SD_DZbest = sd(DZbest, na.rm = TRUE),
    .groups = 'drop'  # Optional: Ungroup after summarizing
  )

MeanSum <- DZdata %>%
  summarize(
    Mean_DZbest = mean(DZbest, na.rm = TRUE),
    Max_DZbest = max(DZbest, na.rm = TRUE),
    Min_DZbest = min(DZbest, na.rm = TRUE),
    SD_DZbest = sd(DZbest, na.rm = TRUE),
    .groups = 'drop'  # Optional: Ungroup after summarizing
  )

# Add a new column indicating the source data frame
MaxSum <- MaxSum %>% mutate(Source = "MaxDepth")
MinSum <- MinSum %>% mutate(Source = "MinDepth")
MeanSum <- MeanSum %>% mutate(Source = "MeanDepth")

Sum<-bind_rows(MaxSum, MinSum, MeanSum)
flextable(Sum)

MaxDeets<-MaxDepth%>%
  slice(c(which.max(DZbest), which.min(DZbest)))

MinDeets<-MinDepth%>%
  slice(c(which.max(DZbest), which.min(DZbest)))


