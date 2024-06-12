## Hutton Noth ##
# June 4th # 

lp<-function(pck){
  if(!require(pck,character.only = TRUE))install.packages(pck);library(pck,character.only = TRUE)
}


#### Testing to see if we can compare t1 and t3 #### 
setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")

lp("geosphere")
lp("sp")
lp("spdep")
lp("ggplot2")
lp("dplyr")
lp("irr")

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
# ggsave(filename = paste0("plot_", site_id, ".png"), plot = plot)
  
}

View(t1_t3_distance_results)


#### Lets test if NASC values significantly vary across our study site ####
## lets load in neccessary data 
load("wdata/full_df_t3.RData")
load("wdata/full10_1m_df.RData")
load("wdata/full15_1m_df.RData")

## lets pull only the data we need from the larger data frames
nasct3 <- full_df_t3 %>% select("Site_ID", "NASC_10_t3")
nasct1 <- full10_1m_df %>% select("Site_ID", "NASC_10_1m")

## merge these data frames 
nasccomparison <- merge(nasct1, nasct3, by = "Site_ID")

## plot this data
ggplot(nasccomparison, aes(x = NASC_10_1m, y = NASC_10_t3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  geom_text(aes(label =Site_ID)) +
  labs(x = "T1 nasc 10m", y = "T3 nasc 10m") +
  theme_minimal()
## we can see some serious outliers, lets see what the t-test says then remove them
## t-Test - nasc values 
t_test_result <- t.test(nasccomparison$NASC_10_1m, nasccomparison$NASC_10_t3, paired = TRUE)

print(t_test_result)
# t-value = -1.5582 this means that the average nasc of t1 is less than t3
# the p-value is 0.1263 - we can not reject the null hypothese that there is no difference between the two values 
# the alternative hypothesis states that the true mean difference is not equal to zero 
# the 95% confidence interval lies between  -4689.6061 and 599.9221 since this interval includes zero it supports the conculsion theres no signficant difference 
# mean difference is -2044.842 which suggests that NASC_10_1m is lower then NASC_10_t3  

## box plot the data to make sense of the value distribution 
nasccomparison_long <- nasccomparison %>%
  pivot_longer(cols = c(NASC_10_1m, NASC_10_t3),
               names_to = "Transect",
               values_to = "NASC")

ggplot(nasccomparison_long, aes(x = Transect, y = NASC)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "NASC comparison (outliers removed)",
       y = "NASC Value")
## this is super skewed by the outliers 

### the Outliers are causing issues, lets retry after removing the noticable ones 
nascomparison_clean <- nasccomparison %>% 
  filter(Site_ID != "NS39", Site_ID != "NS44",Site_ID != "NS27", Site_ID != "NS15")

## plot this filtered data 
ggplot(nascomparison_clean, aes(x = NASC_10_1m, y = NASC_10_t3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  geom_text(aes(label =Site_ID)) +
  labs(x = "T1 nasc 10m", y = "T3 nasc 10m") +
  theme_minimal()
## we can see its more negativel 
t_test_Clean_result <- t.test(nascomparison_clean$NASC_10_1m, nascomparison_clean$NASC_10_t3, paired = TRUE)

print(t_test_Clean_result)
# t-value = -0.17099  indicating the size of the difference relative to the variation in the sample data, so negative means that t1 is slightly less than t3, but very miminally 
# p-value is 0.8651 we can not reject the null hypothesis that there is any difference between these, removing the outliers even made this weaker
# alternative hypothesis states that the mean diff is not zero 
# mean difference is -42.61309

nascomparison_clean_long <- nascomparison_clean %>%
  pivot_longer(cols = c(NASC_10_1m, NASC_10_t3),
               names_to = "Transect",
               values_to = "NASC")

ggplot(nascomparison_clean_long, aes(x = Transect, y = NASC)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "NASC comparison (outliers removed)",
       y = "NASC Value")

### lets try for NASC 15 

nasct3_15 <- full_df_t3 %>% select("Site_ID", "NASC_15_t3")
nasct1_15 <- full15_1m_df %>% select("Site_ID", "NASC_15_1m")

# merge the dataframes 
nasccomparison_15 <- merge(nasct1_15, nasct3_15, by = "Site_ID")

# plot this 
ggplot(nasccomparison_15, aes(x = NASC_15_1m, y = NASC_15_t3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  geom_text(aes(label =Site_ID)) +
  labs(x = "T1 NASC 15m", y = "T3 NASC 15m") +
  theme_minimal()

## the same outliers as before as causing issues
nascomparison_clean_15 <- nasccomparison_15 %>% 
  filter(Site_ID != "NS39", Site_ID != "NS44",Site_ID != "NS27", Site_ID != "NS15")

ggplot(nascomparison_clean_15, aes(x = NASC_15_1m, y = NASC_15_t3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  geom_text(aes(label =Site_ID)) +
  labs(x = "T1 NASC 15m", y = "T3 NASC 15m", title = "Outliers removed") +
  theme_classic()


## t-Test - nasc values 
t_test_result_15 <- t.test(nascomparison_clean_15$NASC_15_1m, nascomparison_clean_15$NASC_15_t3, paired = TRUE)

print(t_test_result_15)

nascomparison_clean_15long <-nascomparison_clean_15 %>%
  pivot_longer(cols = c(NASC_15_1m, NASC_15_t3),
               names_to = "Transect",
               values_to = "NASC")

ggplot(nascomparison_clean_15long, aes(x = Transect, y = NASC)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "NASC comparison (outliers removed)",
       y = "NASC Value")

# lets now filter to test the same thing on sites that are only less than 10m away from eachother 

close_sites <- t1_t3_distance_results %>% 
  filter(T1T3_mean_dist < 10)

# let combine the NASC values to this site just for close site ID 
nasc_close <- merge(close_sites, nasccomparison, by = "Site_ID")

## now lets run a t-test 
t_test_nasc_close_lessthan10 <- t.test(nasc_close$NASC_10_1m, nasc_close$NASC_10_t3, paired = TRUE)
# Results still show no significance, t value = -1.028, p-value - 0.3308 
# CI overlapping 0

### lets test the relationship between rugosity and standard deviation of the slope 
# we assume that as the value of rugosity increases that the standard deviation the slope does too 
load("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/wdata/sitelines.RData")


sitelines$rugosity <- sitelines$chainlength/ sitelines$profilelength
## lets graph this 
ggplot(sitelines, aes(x = Std_Dev_Slope, y = Ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  geom_text(aes(label =Site_ID)) +
  labs(x = "SD of slope", y = "rugosity") +
  theme_minimal()
ggplot(sitelines, aes(x = Ratio, y = Std_Dev_Slope)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
 # geom_text(aes(label =Site_ID)) +
  labs(x = "SD of slope", y = "rugosity") +
  theme_minimal()

## there doesn't appear to be any relationsip 
t_test_rug_SDslope <- t.test(sitelines$Ratio, sitelines$Std_Dev_Slope, paired = TRUE)
print(t_test_rug_SDslope)
# the P-value is significant but this is because the values aren't standardized so
# there obviously will be a difference between these values. 

# lets try another measure of testing linearity - pearson correlation coefficent 
pearson <- cor(sitelines$Ratio, sitelines$Std_Dev_Slope, method = "pearson", use = "complete.obs")
# correlation coefficent is 0.866312 which means very high correlation coefficent 

spearman_correlation <- cor(sitelines$Ratio, sitelines$Std_Dev_Slope, method = "spearman", use = "complete.obs")
# spearman value of 0.8945 also a very strong monotonic relationship 

## lets standardize the variables and these things again. 
sitelines$standardized_Ratio <- scale(sitelines$Ratio)
sitelines$standardized_Std_Dev_Slope <- scale(sitelines$Std_Dev_Slope)

## lets look at the standardized variables
ggplot(sitelines, aes(x = standardized_Std_Dev_Slope, y = standardized_Ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  geom_text(aes(label =Site_ID)) +
  labs(x = "SD of slope", y = "rugosity") +
  theme_minimal()
## there still doesn't appear to be a linear relationship 

t_test_standardized_surface <- t.test(sitelines$standardized_Std_Dev_Slope, sitelines$standardized_Ratio, paired = TRUE)
# so there is no significant difference between the standard deviation of the slope and the ratio when standardized 

# lets try the other measures of relationships 
standardized_spearman <- cor(sitelines$standardized_Std_Dev_Slope, sitelines$standardized_Ratio, method = "spearman", use = "complete.obs")
standardized_pearson <- cor(sitelines$standardized_Std_Dev_Slope, sitelines$standardized_Ratio, method = "pearson", use = "complete.obs")
## same result with standardized values showing a very strong relationship 

## lets try an icc test 
icc_result_standardized <- icc(data.frame(sitelines$standardized_Ratio, sitelines$standardized_Std_Dev_Slope), model = "twoway", type = "agreement", unit = "single")
## the standardized ICC shows a strong relationship 
icc_result <- icc(data.frame(sitelines$Ratio, sitelines$Std_Dev_Slope), model = "twoway", type = "agreement", unit = "single")
