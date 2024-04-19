# By Hutton Noth 
# March 14th, 2024
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

# merge the bin dataframes 
merged_bin <- left_join(bininfo, binlines, by = "BinID")
bin_complete <- left_join(merged_bin, bin_df, by = "BinID")

#remove unwanted sites and make nas 0
bin_complete<- bin_complete%>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  filter(Site_ID.x !="NS01", Site_ID.x != "NS02", Site_ID.x != "NS03", Site_ID.x != "NS04",
         Site_ID.x != "NS05", Site_ID.x != "NS06", Site_ID.x != "NS08", Site_ID.x != "NS18")


#create column for bin number
bin_complete<- bin_complete%>% separate(BinID, into = c("site","bin_num"), sep = "_", remove = FALSE )%>%
  dplyr::select(-c(site))

# adjust these for bin column names
# bin_complete <- bin_complete %>% filter(!is.na(TopSub))
# bin_complete <- bin_complete %>% filter(!is.na(Layer_depth_max))
# bin_complete <- bin_complete %>% mutate(Average_Slope = abs(Average_Slope))
# bin_complete <- bin_complete %>% mutate(Layer_depth_min = abs(Layer_depth_min))


# Bin Average Slope vs nonschooling fish Abundance 
ggplot(bin_complete, aes(x = Average_5m_slope, y = AbundanceNonSchooling, color = bin_num)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(x = "Average Slope", y = "AbundanceNonSchooling") +
  theme_minimal()


# Bin Average Slope vs nonschooling fish Abundance 
ggplot(bin_complete, aes(x = Average_5m_slope, y = total_number_schoolingfish)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(x = "Average Slope", y = "AbundanceNonSchooling") +
  theme_minimal()

# Bin Average Slope vs nonschooling fish Abundance 
ggplot(bin_complete, aes(x = Average_5m_slope, y = total_number_schoolingfish)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(x = "Average Slope", y = "AbundanceNonSchooling") +
  theme_minimal()

# Average Slope vs Rock Fish Abundance 
ggplot(site_complete, aes(x = Average_5m_slope, y = AbundanceNonSchooling, color = TopSub)) +
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
ggplot(site_complete, aes(x = Average_5m_slope, y = RFSpeciesRichness)) +
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
ggplot(site_complete, aes(x = SpeciesRichness, y = Ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Species Richness", y = "Rugosity") +
  theme_minimal()


# number of fishschool vs abundance of non-schoolingfish
ggplot(site_complete, aes(x = number_FS, y = AbundanceNonSchooling)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "number of fish schools", y = "abundance of non-schooling fish") +
  theme_minimal()

###this is the best example of how the Ratio vs PaperRatio works (they show complete inverse relationships
### we definitely want the ratio (chain length/profile length). I think there's an issue with how Average slope is being
### calculated as well as it shows an unexpected inverse relationship as well - maybe lots of up and downs cancel out the
# actual true slope we're seeing?)

# deadzone vs rugosity 
ggplot(site_complete, aes(x = Ratio, y = CumulativeArea)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Rugosity", y = "Cumulative Area") +
  theme_minimal()


# deadzone vs average Slope
ggplot(site_complete, aes(x = Average_5m_slope, y = CumulativeArea, label= Site_ID)) +
  geom_point() +
  geom_text(hjust=0, vjust=0)+
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Average Slope", y = "Cumulative Area") +
  theme_minimal()

# Average slope vs non-schooling fish
ggplot(site_complete, aes(x = AbundanceNonSchooling, y = Average_5m_slope)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "AbundanceNonSchooling", y = "Average_Slope") +
  theme_minimal()

# Average slope vs non-schooling fish
ggplot(bin_complete, aes(x = AbundanceNonSchooling, y = Average_5m_slope, color=bin_num)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "AbundanceNonSchooling", y = "Average_Slope") +
  theme_minimal()

bin_complete$Depth_mean_5

ggplot(bin_complete, aes(x = AbundanceNonSchooling, y = Depth_mean_5, color = bin_num)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "nonschoolin", y = "avg_depth") +
  theme_minimal()

# deadzone vs rockfish abundance (looks like there's a big outlier here)
ggplot(bin_complete, aes(x = AbundanceNonSchooling, y = CumulativeArea )) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "abundance non schooling", y = "deadzone area") +
  theme_minimal()

bin_complete$total_number_schoolingfish
# deadzone vs rockfish abundance (looks like there's a big outlier here)
ggplot(bin_complete, aes(x = AbundanceNonSchooling, y = number_FS, color = bin_num )) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "abundance non schooling", y = "total schooling fish") +
  theme_minimal()

# deadzone vs non schooling fish abundance
ggplot(site_complete, aes(x = AbundanceNonSchooling, y = CumulativeArea, label= Site_ID )) +
  geom_point() +
  geom_text(hjust=0, vjust=0)+
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

ggplot(site_complete, aes(x = Std_Dev_Slope, y = ChainDiff)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Standard deviation of slope", y = "ChainDiff") +
  theme_minimal()

# ratio has a negative measure 

ggplot(site_complete, aes(x = Ratio, y = total_number_schoolingfish)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "ratio", y = "Number of schooling fish") +
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


