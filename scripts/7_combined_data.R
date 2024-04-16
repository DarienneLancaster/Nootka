# By Hutton Noth 
# March 14th, 2024
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

# load in dataframes 
# load("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/full_lines.RData")
# load("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/bin_lines.RData")
# load("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/bin_df.RData")
# load("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/full_df.RData")
# load("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/bininfo.RData")
# load("C:/Users/HuttonNoth(HFS)/OneDrive - Ha’oom Fisheries Society/Nootka Rockfish Paper/Nootka_Aug2023/R/Nootka/siteinfo.RData")

load("wdata/full_lines.RData")
load("wdata/bin_lines.RData")
load("wdata/bin_df.RData")
load("wdata/full_df.RData")
load("wdata/bininfo.RData")
load("wdata/siteinfo.RData")

# merge the site dataframes 
merged_site <- left_join(siteinfo, sitelines, by = "Site_ID")
site_complete <- left_join(merged_site, full_df, by = "Site_ID")
site_complete <- site_complete %>% filter(!is.na(TopSub))
site_complete <- site_complete %>% filter(!is.na(Layer_depth_max))
site_complete <- site_complete %>% mutate(Average_Slope = abs(Average_Slope))
site_complete <- site_complete %>% mutate(Layer_depth_min = abs(Layer_depth_min))

## Abundance across sites

ggplot(site_complete, aes(x = Site_ID)) +
  geom_col(aes(y = TotalAbundance, fill = "Total Abundance"), position = position_nudge(x = -0.2), width = 0.4, color = "black") +
  geom_col(aes(y = RFAbundance, fill = "Rockfish Abundance"), position = position_nudge(x = 0.2), width = 0.4, color = "black") +
  scale_fill_manual(values = c("Total Abundance" = "gray70", "Rockfish Abundance" = "gray30")) +
  labs(y = "Abundance", x = "", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

siteabundance <- site_complete %>% select(Site_ID, TotalAbundance, RFAbundance)
siteabundance$percent <- 100*(siteabundance$TotalAbundance / sum(siteabundance$TotalAbundance))

## site species richness distribution 
ggplot(site_complete, aes(x = Site_ID)) +
  geom_col(aes(y = SpeciesRichness, fill = "Total Species Richness"), position = position_nudge(x = -0.2), width = 0.4, color = "black") +
  geom_col(aes(y = RFSpeciesRichness, fill = "Rockfish Species Richness"), position = position_nudge(x = 0.2), width = 0.4, color = "black") +
  scale_fill_manual(values = c("Total Species Richness" = "gray70", "Rockfish Species Richness" = "gray30")) +
  labs(y = "Species Richness", x = "", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

siteSR <- site_complete %>% select(Site_ID, SpeciesRichness, RFSpeciesRichness)
## substrate vs RF abundance 

ggplot(site_complete, aes(x = TopSub, y = RFAbundance, fill = TopSub)) +
  geom_boxplot() +
  labs(x = "Substrate Type", y = "Rockfish Abundance") +
  theme_minimal()

# Substrate classification vs RF species Richness 
ggplot(site_complete, aes(x = TopSub, y = RFSpeciesRichness, fill = TopSub)) +
  geom_boxplot() +
  labs(x = "Substrate Type", y = "Rockfish Species richness") +
  scale_fill_manual(values = c("hard" = "gray80", "soft" = "gray50")) +
  labs(y = "Rockfish Species Richness", x = "Substrate Classification", fill = "") +
  theme_minimal()

ggplot(site_complete, aes(x = TopSub, y = SpeciesRichness, fill = TopSub)) +
  geom_boxplot() +
#  labs(x = "Substrate Type", y = "Species richness") +
  scale_fill_manual(values = c("hard" = "gray80", "soft" = "gray50")) +
  labs(y = "Species Richness", x = "Substrate Classification", fill = "") +
  theme_minimal()


##'# deadzonedepth vs substrate type is significant '
ggplot(site_complete, aes(x = TopSub, y = Average_DZDiff, fill = TopSub)) +
  geom_boxplot() +
  labs(x = "Substrate Type", y = "Deadzone Depth") +
  scale_fill_manual(values = c("hard" = "gray80", "soft" = "gray50")) +
  labs(y = "Deadzone Depth", x = "Substrate Type", fill = "") +
  theme_minimal()

ggplot(site_complete, aes(x = Average_Slope, y = RFAbundance)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Add a smoothed line
  labs(x = "Average Slope", y = "Rockfish Abundance") +
  theme_minimal()




ggplot(site_complete, aes(x = Average_Slope, y = RFAbundance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Average Slope", y = "Rockfish Abundance") +
  theme_minimal()


## could be significant 
ggplot(site_complete, aes(x = Average_DZDiff, y = RFAbundance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Deadzone Depth", y = "Rockfish Abundance") +
  theme_minimal()




# Average Slope vs Rock Fish Abundance 
ggplot(site_complete, aes(x = Average_Slope, y = RFAbundance, color = TopSub)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(x = "Average Slope", y = "Rock Fish Abundance", color = "TopSub") +
  theme_minimal()

# Rugosity vs Rockfish Species Richness 
ggplot(site_complete, aes(x = Ratio, y = RFSpeciesRichness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Rugosity", y = "Rock Fish Species Richness") +
  theme_minimal()

# Average Slope vs Rockfish Species Richness 
ggplot(site_complete, aes(x = Average_Slope, y = RFSpeciesRichness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Average Slope", y = "Rock Fish Species Richness") +
  theme_minimal()

# Rugosity vs RF Abundance 
ggplot(site_complete, aes(x = Ratio, y = RFAbundance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Rugosity", y = "Rock Fish Abundance") +
  theme_minimal()

# Rugosity vs Total Abundance
ggplot(site_complete, aes(x = Ratio, y = TotalAbundance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Rugosity", y = "Total Abundance") +
  theme_minimal()

# Rugosity vs Abundance 
ggplot(site_complete, aes(x = Ratio, y = SpeciesRichness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Rugosity", y = "Species Richness") +
  theme_minimal()


# number of fishschool vs abundance of non-schoolingfish
ggplot(site_complete, aes(x = number_FS, y = AbundanceNonSchooling)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "number of fish schools", y = "abundance of non-schooling fish") +
  theme_minimal()

# deadzone vs rugosity 
ggplot(site_complete, aes(x = Ratio, y = CumulativeArea)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Rugosity", y = "Cumulative Area of deadzone") +
  theme_minimal()

# deadzone vs rockfish abundance (looks like there's a big outlier here)
ggplot(site_complete, aes(x = RFAbundance, y = CumulativeArea )) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "abundance rockfish", y = "deadzone area") +
  theme_minimal()

# deadzone vs non schooling fish abundance
ggplot(site_complete, aes(x = AbundanceNonSchooling, y = CumulativeArea )) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "abundance of non-schooling fish", y = "deadzone area") +
  theme_minimal()

# STD slope vs non schooling fish abundance
ggplot(site_complete, aes(x = AbundanceNonSchooling, y = Std_Dev_Slope )) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "abundance of non-schooling fish", y = "Std_Dev_Slope") +
  theme_minimal()

# Chain ratio vs non schooling fish abundance
ggplot(site_complete, aes(x = AbundanceNonSchooling, y = Ratio )) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "abundance of non-schooling fish", y = "ChainRatio") +
  theme_minimal()

# Chain length difference vs non schooling fish abundance
ggplot(site_complete, aes(x = AbundanceNonSchooling, y = ChainDiff )) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "abundance of non-schooling fish", y = "ChainDiff") +
  theme_minimal()

# Chain length difference vs non schooling fish abundance
ggplot(site_complete, aes(x = AbundanceNonSchooling, y = NASC_15 )) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "abundance of non-schooling fish", y = "NASC_15") +
  theme_minimal()

site_complete <- site_complete %>%
  filter(Sv_mean_15 >= -110)

ggplot(site_complete, aes(x = Sv_mean_15, y = TotalAbundance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Sv_mean_15", y = "Total Abundance") +
  theme_minimal()

lm_rug_Bfish<-lm(AbundanceNonSchooling ~ Std_Dev_Slope, site_complete)
summary(lm_rug_Bfish)

lm_dz_Bfish<-lm(AbundanceNonSchooling ~ CumulativeArea, site_complete)
summary(lm_dz_Bfish)

# the Standard deviation of slope does impact the number of schooling fish 
ggplot(site_complete, aes(x = Std_Dev_Slope, y = total_number_schoolingfish)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Standard deviation of slope", y = "Number of schooling fish") +
  theme_minimal()
## Standard deviation of the slope shows surface roughness 
# explains the expected tends better 
ggplot(site_complete, aes(x = Std_Dev_Slope, y = RFSpeciesRichness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Standard deviation of slope", y = "Rockfish species richness") +
  theme_minimal()
ggplot(site_complete, aes(x = Std_Dev_Slope, y = SpeciesRichness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Standard deviation of slope", y = "Total Species Richness") +
  theme_minimal()
ggplot(site_complete, aes(x = Std_Dev_Slope, y = TotalAbundance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Standard deviation of slope", y = "Total Abundance") +
  theme_minimal()


## this shows that they do not have a linear relationship as expected. I don't really think chain length ratio is very accurate in this context

ggplot(site_complete, aes(x = Std_Dev_Slope, y = Ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Standard deviation of slope", y = "ratio") +
  theme_minimal()

# ratio has a negative measure 

ggplot(site_complete, aes(x = Ratio, y = total_number_schoolingfish)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Standard deviation of slope", y = "Number of schooling fish") +
  theme_minimal()

# Depth vs RF abudance

ggplot(site_complete, aes(x = Layer_depth_min, y = RFAbundance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Depth", y = "Rock Fish Abundance") +
  theme_minimal()

# Depth vs Total Abundance
ggplot(site_complete, aes(x = Layer_depth_min, y = TotalAbundance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Depth", y = "Abundance") +
  theme_minimal()

# Depth vs RF Species Richness 
ggplot(site_complete, aes(x = Layer_depth_min, y = RFSpeciesRichness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Depth", y = "Rock species richness") +
  theme_minimal()

# Depth vs total Species Richness 
ggplot(site_complete, aes(x = Layer_depth_min, y = SpeciesRichness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Depth", y = "Species Richness") +
  theme_minimal()



