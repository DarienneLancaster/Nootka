## Hutton Noth ##
# June 4th # 

lp<-function(pck){
  if(!require(pck,character.only = TRUE))install.packages(pck);library(pck,character.only = TRUE)
}


## Testing to see if we can compare t1 and t3

setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")
lp("geosphere")
lp("sp")
lp("spdep")

# Set pathways for the for loop to pull from for file name 
T1_path <- "odata/Transect 1/100mLines/Bottom"
T3_path <- "odata/Transect 3/Bottom Lines"

# list csv only but with the full pathways 
csv_files_T1 <- list.files(T1_path, pattern = "\\.csv$", full.names = TRUE)
csv_files_T3 <- list.files(T3_path, pattern = "\\.csv$", full.names = TRUE)

# dataframe with the pathways, Site_ID 
# Set pathways for the for loop to pull from for file name 
T1_path <- "odata/Transect 1/100mLines/Bottom"
T3_path <- "odata/Transect 3/Bottom Lines"

# list csv only but with the full pathways 
csv_files_T1 <- list.files(T1_path, pattern = "\\.csv$", full.names = TRUE)
csv_files_T3 <- list.files(T3_path, pattern = "\\.csv$", full.names = TRUE)

# dataframe with the pathways, Site_ID 
files_T1 <- data.frame(
  file_path = csv_files_T1,
  Site_ID = substr(basename(csv_files_T1), 1, 4),
  stringsAsFactors = FALSE
)
files_T3 <- data.frame(
  file_path = csv_files_T3,
  Site_ID = substr(basename(csv_files_T3), 1, 4),
  stringsAsFactors = FALSE
)

# combine the new dataframes by site_ID for both T1 and T3
merged_files <- merge(files_T1, files_T3, by = "Site_ID", suffixes = c("_T1", "_T3"))

# create a new dataframe for the results 
t1_t3_distance_results <- data.frame(Site_ID = character(), T1T3_mean_dist = numeric(), T1T3_SD_Dist = numeric(), stringsAsFactors = FALSE)

# change the projection from lat and long
crs_original <- CRS("+proj=longlat +datum=WGS84")
# define the zone we are in for utm (nootka is predominately 9 but kind of on the border of 10)
utm_zone <- CRS("+proj=utm +zone=9 +datum=WGS84")

# combine the new dataframes by site_ID for both T1 and T3
merged_files <- merge(files_T1, files_T3, by = "Site_ID", suffixes = c("_T1", "_T3"))

# create a new dataframe for the results 
t1_t3_distance_results <- data.frame(Site_ID = character(), T1T3_mean_dist = numeric(), T1T3_SD_Dist = numeric(), stringsAsFactors = FALSE)

# change the projection from lat and long
crs_original <- CRS("+proj=longlat +datum=WGS84")
# define the zone we are in for utm (nootka is predominately 9 but kind of on the border of 10)
utm_zone <- CRS("+proj=utm +zone=9 +datum=WGS84")

# add in these files to th efor loop
for (i in 1:nrow(merged_files)) {
  # Load the CSV files
  file_T1 <- read.csv(merged_files$file_path_T1[i])
  file_T3 <- read.csv(merged_files$file_path_T3[i])
  
  site_id <- merged_files$Site_ID[i]
  
  # covert the lat and long into coordiates for both T1 and T3
  coordinates(file_T1) <- c("Longitude", "Latitude")
  coordinates(file_T3) <- c("Longitude", "Latitude")
  
  # again change the projection of the coordinates so that our output is in meters not degrees 
  proj4string(file_T1) <- crs_original
  proj4string(file_T3) <- crs_original
  
  # change the projection 
  file_T1_proj <- spTransform(file_T1, utm_zone)
  file_T3_proj <- spTransform(file_T3, utm_zone)
  
  # now we have out projected spatial coordinates that we need to pull from 
  coords_T1 <- coordinates(file_T1_proj)
  coords_T3 <- coordinates(file_T3_proj)
  
  # now because one of the dataframe will have more rows we need to make them both equal to the minimum 
  n_points_T1 <- nrow(coords_T1)
  n_points_T3 <- nrow(coords_T3)
  
  # set both to the minimum 
  n_points <- min(n_points_T1, n_points_T3)
  
  # calc the euclidean distance between the points in T1 and T3
  distances <- sqrt((coords_T1[1:n_points, 1] - coords_T3[1:n_points, 1])^2 +
                      (coords_T1[1:n_points, 2] - coords_T3[1:n_points, 2])^2)
  
  # now lets export the mean and standard deviation ?? any other variables? 
  average_distance <- mean(distances)
  sd_distance <- sd(distances)
  # Store the results in the data frame
  t1_t3_distance_results <- rbind(t1_t3_distance_results, data.frame(Site_ID = site_id, T1T3_mean_dist = average_distance, T1T3_SD_Dist = sd_distance))
 
  
  # let create a dataframe that we can plot 
  plot_data <- data.frame(
    Longitude = c(file_T1$Longitude[1:n_points], file_T3$Longitude[1:n_points]),
    Latitude = c(file_T1$Latitude[1:n_points], file_T3$Latitude[1:n_points]),
    Transect = rep(c("T1", "T3"), each = n_points))
  
  plot <- ggplot(plot_data, aes(x = Longitude, y = Latitude, color = Transect)) +
    geom_path() +
    ggtitle(paste("Site ID:", site_id)) +
    theme_classic()
  # Save tthe plots to our working directory 
  ggsave(filename = paste0("plot_", site_id, ".png"), plot = plot)
  
}

view(t1_t3_distance_results)



