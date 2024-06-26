## MMFN Report - Site level variables ## 
setwd("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka")
ROV <-read.csv("odata/NootkaROV_20240426_finalDL_NS42noGPSused.csv",  skip=4, stringsAsFactors = FALSE)
lp<-function(pck){
  if(!require(pck,character.only = TRUE))install.packages(pck);library(pck,character.only = TRUE)
}
lp("tidyverse")
lp("lubridate")
lp("dplyr")
lp("ggplot2")
lp("flextable")
lp("xlsx")
lp("showtext")
lp("sysfonts")

## fix up dataframe 

ROV$Species[ROV$Species== ""] <- "unknown" 
ROV$Genus[ROV$Genus== ""] <- "unknown"
ROV$Family[ROV$Family== ""] <- "unknown"
ROV$Site_ID <- substr(ROV$Filename, 1, 4)
ROV <- ROV %>% select("Site_ID","Family","Genus","Species","Number","Depth.1")
colnames(ROV)[which(names(ROV) == "Depth.1")] <- "Depth"
rockfishonly <- ROV %>% filter(Genus == "Sebastes" | Family == "Hexagrammidae")


## we have all the rockfish and the species
  rockfishonly$CommonName<- ifelse(rockfishonly$Species == "caurinus", "Copper rockfish",
                       ifelse(rockfishonly$Species == "maliger", "Quillback rockfish",
                              ifelse(rockfishonly$Species == "pinniger", "Canary rockfish",
                                     ifelse(rockfishonly$Species == "miniatus", "Vermillion rockfish",
                                            ifelse(rockfishonly$Species == "melanops", "Black rockfish",
                                                   ifelse(rockfishonly$Species == "elongatus", "Lingcod",
                                                          ifelse(rockfishonly$Species == "decagrammus", "Kelp greenling",
                                                                 ifelse(rockfishonly$Species == "emphaeus", "Puget Sound rockfish",
                                                                        ifelse(rockfishonly$Species == "flavidus", "Yellowtail rockfish",
                                                                               ifelse(rockfishonly$Species == "pictus", "Painted greenling",
                                                                                             ifelse(rockfishonly$Species == "entomelas", "Widow rockfish", "Unknown")))))))))))
  
  
  unique(rockfishonly$CommonName)
  
RockFish <- rockfishonly %>% select("Site_ID", "CommonName", "Depth", "Number")
RockFish$DepthM <- RockFish$Depth * 0.3048
RockFish$DepthMNeg <- RockFish$DepthM*-1

# perform a wilcox test tp see of depth is significantly different for different species 
pairwise_wilcox <- pairwise.wilcox.test(RockFish$DepthM, RockFish$CommonName, p.adjust.method = "BH")
print(pairwise_wilcox)

#### lets plot 
# Lets add in the fira sans to match haoom style guides 
font_add_google("Fira Sans", "Fira Sans Light")
showtext_auto()

# Lets calculat the mean depth and set the counts for each species 
species_stats <- RockFish %>%
  group_by(CommonName) %>%
  summarise(mean_depth = mean(DepthMNeg, na.rm = TRUE),
            count = sum(Number)) %>%
  arrange(desc(mean_depth))

# we are creating a new column that includes the common name and the count
species_stats <- species_stats %>%
  mutate(CommonNameLabel = paste0(CommonName, " (n = ", count, ")"))

# merge with the RockFish dataframe 
RockFish <- RockFish %>%
  left_join(species_stats, by = "CommonName")

# Now I want to organize the data by the to have the RockFish by depth 
RockFish$CommonNameLabel <- factor(RockFish$CommonNameLabel, levels = species_stats$CommonNameLabel)

# Determine the range for the y-axis, ensuring it includes 0
depth_range <- range(RockFish$DepthMNeg, na.rm = TRUE)
y_limits <- c(min(0, depth_range[1]), depth_range[2])


ggplot(RockFish, aes(x = CommonNameLabel, y = DepthMNeg)) +
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  theme_classic() +
  labs(title = "Depth Distribution by Species", x = "", y = "Depth (m)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Fira Sans Light", size = 10),
    plot.title = element_text(hjust = 0.5, family = "Fira Sans Light", size = 16),
    text = element_text(family = "Fira Sans Light")
  ) +
  scale_y_continuous(limits = y_limits) + # Set the y-axis limits to include 0
  geom_text(data = species_stats, aes(x = CommonNameLabel, y = y_limits[2], 
                                      label = paste0(round(mean_depth, 1))),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 4, color = "red")
unique(RockFish$Site_ID)

## lets make this for a certain site_ID 
RockFish47 <- RockFish %>% filter(Site_ID == "NS47")
ggplot(RockFish47, aes(x = CommonNameLabel, y = DepthMNeg)) +
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  theme_classic() +
  labs(title = "Depth Distribution by Species", x = "", y = "Depth (m)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Fira Sans Light"),
    plot.title = element_text(hjust = 0.5, family = "Fira Sans Light", size = 14),
    text = element_text(family = "Fira Sans Light")
  ) +
#  scale_y_continuous(limits = y_limits) + # Set the y-axis limits to include 0
  geom_text(data = species_stats, aes(x = CommonNameLabel, y = y_limits[2], 
                                      label = paste0(round(mean_depth, 1))),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3, color = "red")


rockfishonly <- RockFish

### create a for loop that plots this 
# create a unique site ID 
rockfishonly <- rockfishonly %>% 
  filter(CommonName != "Unknown")


unique_sites <- unique(rockfishonly$Site_ID)
## lets create a for-loop at each site to plot this 
for (site in unique_sites) {
  # set the filter for the particular site 
  site_data <- rockfishonly %>% filter(Site_ID == site)
  
  # also calculate the mean depth and number for each site 
  summary_data <- site_data %>%
    group_by(CommonName) %>%
    summarise(
      TotalNumber = sum(Number),
      MeanDepth = mean(DepthMNeg)
    )
  
  # merge the data 
  site_data <- merge(site_data, summary_data, by = "CommonName")
  
  # create the plot 
  p <- ggplot(site_data, aes(x = reorder(CommonName, DepthMNeg), y = DepthMNeg)) +
    geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
    theme_classic() +
    geom_text(aes(label = round(mean_depth, 2), y = max(DepthMNeg) + 1),
      vjust = -0.5, size = 4, color = "Red")+
   scale_x_discrete(labels = function(x) paste0(x, " (n=", summary_data$TotalNumber[match(x, summary_data$CommonName)], ")")) +
    labs(title = paste("Depth Distribution by Species at", site),
         x = "", y = "Depth (m)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Fira Sans Light"),
      plot.title = element_text(hjust = 0.5, family = "Fira Sans Light", size = 16),
      text = element_text(family = "Fira Sans Light"))
  # print and save the plots
  #print(p)
  ggsave(filename = paste0("plots/Depth_Distribution_", site, ".png"), plot = p, width = 7, height = 6.2, units = "in", dpi = 300)
}




