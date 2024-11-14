#full statistical analyses - GLMs for fish density, habitat metric spearmans correlation plots, etc...

####  Load in Packages ####
lp<-function(pck){
  if(!require(pck,character.only = TRUE))install.packages(pck);library(pck,character.only = TRUE)
}
lp("tidyverse")
lp("lubridate")
lp("dplyr")
lp("ggplot2")
lp("flextable")
lp("xlsx")
lp("patchwork")
lp("nlme")
lp("MASS")
lp("mgcv")
lp("pscl")
lp("lmtest")
lp("lme4")
lp("broom")
lp("mosaic")
lp("car")
lp("performance")
lp("DHARMa")
lp("smplot2")  #package that allows you to add correlation stats to graphs
lp("gridExtra")
lp("reporter")
lp("magrittr")

#####load in additional habitat annotations for ROV video that include rugosity metric######
#####load and process format substrate annotations from ROV videos####
Srov <-read.csv("odata/Manual_Habitat_Annotations_Nootka_20240708_FINAL.csv",  header = TRUE)
str(Srov)

#add bin numbers to substrate annotations
# Measure the total duration for each of the sites
trandur <- Srov %>%
  group_by(Site) %>%
  mutate(Total_Duration = max(Seconds))

# the length of the bin will be the total duration / 5
trandur <- trandur %>%
  mutate(Bin_length = Total_Duration / 5)

## place each observation within a bin based on time
create_bins <- function(data) {
  data %>%
    group_by(Site) %>%
    mutate(
      Min_Time = min(Seconds),
      Bin = case_when(
        between(Seconds, Min_Time, Min_Time + Bin_length) ~ 1,
        between(Seconds, Min_Time + Bin_length, Min_Time + 2 * Bin_length) ~ 2,
        between(Seconds, Min_Time + 2 * Bin_length, Min_Time + 3 * Bin_length) ~ 3,
        between(Seconds, Min_Time + 3 * Bin_length, Min_Time + 4 * Bin_length) ~ 4,
        TRUE ~ 5
      )
    ) %>%
    dplyr::select(-Min_Time)
}

# Apply the function (add bin ID numbers to each substrate annotation based on time along Pass)
trandur <- create_bins(trandur)

#summarize substrate data by site
trandur1<- trandur%>%
  mutate_at(vars(2:4,7:10, 12:14), as.numeric)


site_Srov<- trandur1%>%
  group_by(Site)%>%
  summarize(
    Percent_unobstructed = mean(Percent_unobstructed),
    Distance = mean(Distance),
    Visibility = mean(Visibility),
    Slope = mean(Slope),
    Rugosity = mean(Rugosity),
    Depth = mean(Depth),
    FOV = mean(FOV),
    FOVnVis = mean(FOVnVis))

#count number of observations at each
trandur4 <- trandur1 %>%
  group_by(Site) %>%
  summarize(count = n()) 

#remove extra spaces before or after categorical variable

trandur1$Rock.Soft<-trimws(trandur1$Rock.Soft, "both")

#count number of times each categorical variable occurs by site
counts <- trandur1 %>%
  group_by(Site, Rock.Soft) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Rock.Soft, values_from = count, values_fill = 0)%>%
  rename(Rock=R, Soft = S)

counts1 <- trandur1 %>%
  group_by(Site, Substrate) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Substrate, values_from = count, values_fill = 0)

counts2 <- trandur1 %>%
  group_by(Site, Water_Column) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Water_Column, values_from = count, values_fill = 0)

#join all dataframes

twocounts<-left_join(counts, counts1, by="Site")
allcounts<-left_join(twocounts, counts2, by="Site")
#add total number of observations per site to dataframe
sitecount<-left_join(allcounts, trandur4, by="Site")
#divide all categorical variables by total observations
sitecountD<-sitecount%>%
  mutate(across(Rock:Y, ~./count))
#multiply all vars by 100
sitecountT<-sitecountD%>%
           mutate(across(Rock:Y, ~.*100))

ROV_Sub_Site<-left_join(sitecountT, site_Srov, by="Site")



#summarize substrate data by bin
bin_Srov<- trandur1%>%
  group_by(Site,Bin)%>%
  summarize(
    Percent_unobstructed = mean(Percent_unobstructed),
    Distance = mean(Distance),
    Visibility = mean(Visibility),
    Slope = mean(Slope),
    Rugosity = mean(Rugosity),
    Depth = mean(Depth),
    FOV = mean(FOV),
    FOVnVis = mean(FOVnVis))

#count number of observations in each bin at each site
numbinobs <- trandur1 %>%
  group_by(Site, Bin) %>%
  summarize(count = n()) 

#count number of times each categorical variable occurs by site
bcounts <- trandur1 %>%
  group_by(Site, Bin, Rock.Soft) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Rock.Soft, values_from = count, values_fill = 0)%>%
  rename(Rock=R, Soft = S)

bcounts1 <- trandur1 %>%
  group_by(Site, Bin, Substrate) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Substrate, values_from = count, values_fill = 0)

bcounts2 <- trandur1 %>%
  group_by(Site, Bin, Water_Column) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Water_Column, values_from = count, values_fill = 0)

#join all dataframes

twobcounts<-left_join(bcounts, bcounts1, by=c("Site", "Bin"))
allbcounts<-left_join(twobcounts, bcounts2, by=c("Site", "Bin"))
#add total number of observations per site to dataframe
bincount<-left_join(allbcounts, numbinobs, by=c("Site", "Bin"))
#divide all categorical variables by total observations
bincountD<-bincount%>%
  mutate(across(Rock:Y, ~./count))

bincountT<-bincountD%>%
  mutate(across(Rock:Y, ~.*100))

ROV_Sub_Bin<-left_join(bincountT, bin_Srov, by=c("Site", "Bin"))



#################################################################################################################
####edit dataframe with fish information and NASC information to add to substrate information####

load("wdata/sitelines20241104.RData")
load("wdata/binlines20241104.RData")
load("wdata/binNASC.RData")
load("wdata/siteNASC.RData")
load("wdata/bininfo20241104.RData")
load("wdata/siteinfo20241104.RData")

################### merge the site dataframes############ 
merged_site <- left_join(siteinfo, sitelines, by = "Site_ID")
site_complete <- left_join(merged_site, siteNASC, by = "Site_ID")
site_complete <- site_complete %>% filter(!is.na(TopSub))
site_complete <- site_complete %>% mutate(Average_Slope = abs(Average_Slope))

#remove unwanted sites and make nas 0
site_complete<- site_complete%>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  mutate(Average_Depth = ifelse(Average_Depth == 0, NA, Average_Depth))%>%
  filter(Site_ID !="NS01", Site_ID != "NS02", Site_ID != "NS03", Site_ID != "NS04",
         Site_ID != "NS05", Site_ID != "NS06", Site_ID != "NS08", Site_ID != "NS18")

####subset dataframe to only include variables for GLMMs or GAMS####
str(site_complete)

#create some new variables for fish abundance
site_complete$TotPel<-site_complete$`sum_SF--BP`+site_complete$`sum_SF--SP` #combine big and small pelagics sum total
site_complete$TotBen<-site_complete$`sum_SF--SB`+site_complete$AbundanceNonSchooling #combine solo benthics with benthic schools

site_DE<- site_complete%>% 
  # dplyr::select(c(BinID,bin_num, Site_ID.x,total_number_schoolingfish, number_FS,
  #                 AbundanceNonSchooling,Average_5m_slope,Std_Dev_Slope, CumulativeArea, Sv_mean_15))
  #dplyr::select(c(AbundanceNonSchooling,total_number_schoolingfish, number_FS,Average_5m_slope,Std_Dev_Slope, CumulativeArea, NASC_15, NASC_10, NASC_5, Average_Depth))
  dplyr::select(c(Site_ID,TotalAbundance,AbundanceNonSchooling,number_FS, total_number_schoolingfish, 
                  total_number_NoHschoolingfish,TotPel,TotBen, RFSpeciesRichness,SpeciesRichness,
                  Average_5m_slope, Average_20m_slope,Std_Dev_Slope, Ratio, 
                  Cumulative_LG_DZ_Area, 
                  NASC_5_MAN,NASC_10_MAN,NASC_15_MAN,
                  NASC_5_1m,NASC_10_1m,NASC_15_1m, 
                  NASC_5_LG,NASC_10_LG,NASC_15LG, Average_Depth,Volume,
                  TopSub, SubPercent,TopSlope, SlopePercent))


#### need to factor in volume surveyed by ROV FOV (prelim linear model tests seem to show stronger correlations using volume adjusted values than non adjusted)
site_DE$BenthicbyVol<-site_DE$AbundanceNonSchooling/site_DE$Volume
site_DE$SchoolbyVol<-site_DE$total_number_schoolingfish/site_DE$Volume
site_DE$AllFish<-site_DE$total_number_schoolingfish+site_DE$AbundanceNonSchooling #create column with schooling fish and benthics combined (no mud fish)
site_DE$AllFishbyVol<-site_DE$AllFish/site_DE$Volume
site_DE$TotPelbyVol<-site_DE$TotPel/site_DE$Volume
site_DE$TotBenbyVol<-site_DE$TotBen/site_DE$Volume

#make all vars numeric
site_DE<- site_DE%>%
  mutate_at(vars(2:25,27:31), as.numeric)

str(site_DE)

mean(site_DE$BenthicbyVol)
var(site_DE$BenthicbyVol)

mean(site_DE$AbundanceNonSchooling)
var(site_DE$AbundanceNonSchooling)

str(site_DE)
save(site_DE, file = "wdata/site_DE20241104.RData")

######################################################################################################################

#### attach fish and echosounder data to ROV substrate annotation data####

load("wdata/site_DE20241104.RData")

#rename site to match site_ID
#ROV_Sub_Site<-ROV_Sub_Site%>%
#  rename(Site_ID=Site)


#join dataframes together
ROV_Sub_Site<-ROV_Sub_Site%>%
  rename(Site_ID = Site)
sitefull<-left_join(ROV_Sub_Site, site_DE, by="Site_ID")

sitefull<-sitefull%>%
  ungroup()

#remove site NS05
sitefull<-sitefull%>%
   filter( Site_ID !="NS05")

#scale variables for comparison

sitefull_s<-sitefull%>%
  mutate(Average_5m_slope_s=scale(Average_5m_slope),
         Average_20m_slope_s=scale(Average_20m_slope),
         Std_Dev_Slope_s=scale(Std_Dev_Slope),
         Ratio_s=scale(Ratio),
         Cumulative_LG_DZ_Area_s=scale(Cumulative_LG_DZ_Area),
         NASC_10_1m_s=scale(NASC_10_1m), 
         Rugosity_s=scale(Rugosity),
         Slope_s=scale(Slope),
         Depth_s=scale(Depth),
         Rock_s=scale(Rock),
         Average_Depth_s=scale(Average_Depth))%>%
  mutate_at(vars(56:66), as.numeric)


####calculate mean and sd for habitat variables across sites####

summary_mean_sd <- sitefull %>%
  dplyr::select(Average_5m_slope,
         Average_20m_slope,
         Slope,
         Std_Dev_Slope,
         Ratio,
         Rugosity,
         Rock,
         Cumulative_LG_DZ_Area,
         NASC_15_1m,
         NASC_10_1m,
         NASC_5_1m,
         Depth,
         Average_Depth) %>%
  reframe(
    Mean = round(colMeans(.), 2),
    SD = round(sapply(., sd), 2)
  ) %>%
  t() %>%               # Transpose to make column names accessible
  as.data.frame() %>%   # Convert to data frame
 rename(
   "Slope 5m (°)" = V1,
   "Slope 20m (°)" = V2,
   "Slope ROV (°)" = V3,
   "Rugosity SD_Slope (°)" = V4,
   "Rugosity CL_Ratio (unitless)" = V5,
   "Rugosity ROV (unitless)" = V6,
   "Rock ROV (%)" = V7,
   "Deadzone Area (m^2)" = V8,
   "NASC_15_1m" = V9,
   "NASC_10_1m" = V10,
   "NASC_5_1m" = V11,
   "Depth ROV (m)" = V12,
   "Depth Echosounder(m))" = V13)%>%
 mutate(`Depth ROV (m)` = `Depth ROV (m)` / 3.281)%>% # convert to meters
  rownames_to_column(var = "Summary") 


lp("flextable")

summary_mean_sd_table <-flextable(summary_mean_sd)
summary_mean_sd_table <- set_table_properties(summary_mean_sd_table, layout = "autofit")%>%
  fontsize(size = 12) %>%  # Adjust font size for the entire table
  theme_booktabs() %>%    # Optional: Apply a booktabs theme for nicer table borders
  bold(part = "header") %>% # Make headers (part = "header") bold
  fontsize(size = 12, part = "header")  
summary_mean_sd_table <- align(summary_mean_sd_table, align = c("left"), part = "all")

print(summary_mean_sd_table)
save_as_image(summary_mean_sd_table, path='figures/HabVars_summary_mean_sd.png')



####spearman correlation and wilcoxon signed rank tests####
#test rugosity from ROV and Echosounder for significant difference (Spearman rank correlation)

#SD SLOPE vs ROV
rugSTDROV_spear<-cor.test(sitefull$Rugosity, sitefull$Std_Dev_Slope,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugSTDROV_spear)
#rho = 0.38, p = 0.01 (weak significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

#test for significant difference with wilcoxon signed-rank test for paired/dependent samples
rugSTDROV_wilcox<-wilcox.test(sitefull_s$Rugosity_s, sitefull_s$Std_Dev_Slope_s, exact= FALSE, paired=TRUE) #exact = FALSE gets rid of impact of tied values on ranking, paired=TRUE makes it a paired test for dependent samples
print(rugSTDROV_wilcox)
#p=0.65 (not significant so no sign. difference)

#Chain Length Ratio vs ROV
rugRatioROV_spear<-cor.test(sitefull$Rugosity, sitefull$Ratio,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugRatioROV_spear)
#rho = 0.48, p = 0.002 (moderate significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

#test for significant difference with wilcoxon signed-rank test for paired/dependent samples
rugRatioROV_wilcox<-wilcox.test(sitefull_s$Rugosity_s, sitefull_s$Ratio_s, exact= FALSE, paired=TRUE) #exact = FALSE gets rid of impact of tied values on ranking, paired=TRUE makes it a paired test for dependent samples
print(rugRatioROV_wilcox)
#p=0.76 (not significant so no sign. difference)

#SD SLOPE vs CHain Length Ratio
rugRatioSTD_spear<-cor.test(sitefull$Std_Dev_Slope, sitefull$Ratio,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugRatioSTD_spear)
#rho = 0.898, p = 8.08e-15 (very strong significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

#test for significant difference with wilcoxon signed-rank test for paired/dependent samples
rugRatioSTD_wilcox<-wilcox.test(sitefull_s$Std_Dev_Slope_s, sitefull_s$Ratio_s, exact= FALSE, paired=TRUE) #exact = FALSE gets rid of impact of tied values on ranking, paired=TRUE makes it a paired test for dependent samples
print(rugRatioSTD_wilcox)
#p=0.93 (not significant so no sign. difference)

# Create ggplot plots of rugosity metrics
rugSTDROV <- ggplot(sitefull_s, aes(x = Rugosity_s, y = Std_Dev_Slope_s)) +
  geom_point(color = "deepskyblue") +
  labs(title = "a)") +
  sm_statCorr(corr_method="spearman", color = "deepskyblue3", linetype = "dashed")+
  #geom_smooth(method = "loess", color = "deepskyblue4", linetype = "dashed", se=FALSE)+
  labs(x = "ROV", y = "SD Slope")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugSTDROV  

rugRatioROV <- ggplot(sitefull_s, aes(x = Rugosity_s, y = Ratio_s)) +
  geom_point(color = "deepskyblue") +
  labs(title = "b)") +
  sm_statCorr(corr_method="spearman", color = "deepskyblue3", linetype = "dashed")+
  labs(x = "ROV", y = "Ratio")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugRatioROV  

rugRatioSTD <- ggplot(sitefull_s, aes(x = Ratio_s, y = Std_Dev_Slope_s)) +
  geom_point(color = "deepskyblue") +
  labs(title = "c)") +
  sm_statCorr(corr_method="spearman", color = "deepskyblue3", linetype = "dashed")+
  labs(x = "Ratio", y = "SD Slope")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugRatioSTD

## box plot of RUGOSITY data 
sitefull_s_subR <- sitefull_s %>%
  dplyr::select("Site_ID", "Rugosity_s", "Std_Dev_Slope_s", "Ratio_s")

sitefull_s_longR <- sitefull_s_subR %>%
  rename(
    "ROV" = Rugosity_s,
    "SD Slope" = Std_Dev_Slope_s,
    "Ratio" = Ratio_s
  ) %>%
  pivot_longer(cols = c(ROV, `SD Slope`, `Ratio`),
               names_to = "Method",
               values_to = "Rugosity")

# Define the desired order of Method levels
method_order <- c("SD Slope","Ratio", "ROV")  # Replace with your actual method names and desired order

# Convert Method to a factor with specified levels and order
sitefull_s_longR$Method <- factor(sitefull_s_longR$Method, levels = method_order)

rug_box<-ggplot(sitefull_s_longR, aes(x = Method, y = Rugosity )) +
  geom_boxplot(color = "deepskyblue4") +
  labs(title = "d)") +
  geom_jitter(width = 0.2, alpha = 0.6, color = "deepskyblue") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
rug_box


lp("grid")
###put all rugosity plots together
allrug<-grid.arrange(rugSTDROV, rugRatioROV, rugRatioSTD, rug_box, nrow = 1, respect=TRUE)
grid.text(
  label = "Rugosity",
  x = unit(0.5, "npc"),  # Horizontal center
  y = unit(1, "npc") + unit(0.5, "lines"),  # Slightly above the grid
  gp = gpar(fontsize = 14, fontface = "bold")  # Customize font size and style
)
allrug
ggsave("figures/ROV_Echo_Rugosity_spearmancor.png", plot = allrug, width = 25, height = 25, units = "cm")


#######################################################################################################  

####ROV rock percentage vs SD Slope and Chain Length Ratio metrics####

#test relationship between rock percentage and STD slope
rugSTDrock_spear<-cor.test(sitefull$Std_Dev_Slope, sitefull$Rock,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugSTDrock_spear)
#rho = 0.44, p = 0.005 (moderate significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

#test for significant difference with wilcoxon signed-rank test for paired/dependent samples
rugSTDrock_wilcox<-wilcox.test(sitefull_s$Std_Dev_Slope_s, sitefull_s$Rock_s, exact= FALSE, paired=TRUE) #exact = FALSE gets rid of impact of tied values on ranking, paired=TRUE makes it a paired test for dependent samples
print(rugSTDrock_wilcox)
#p=0.81 (not significant so no sign. difference)

#test relationship between rock percentage and Ratio
rugRATIOrock_spear<-cor.test(sitefull$Ratio, sitefull$Rock,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugRATIOrock_spear)
#rho = 0.52, p = 0.0006 (moderate significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

#test for significant difference with wilcoxon signed-rank test for paired/dependent samples
rugRATIOrock_wilcox<-wilcox.test(sitefull_s$Ratio_s, sitefull_s$Rock_s, exact= FALSE, paired=TRUE) #exact = FALSE gets rid of impact of tied values on ranking, paired=TRUE makes it a paired test for dependent samples
print(rugRATIOrock_wilcox)
#p=0.81 (not significant so no sign. difference)
colors()

####plot rock vs rugosity metrics
rugSTDrock <- ggplot(sitefull_s, aes(x = Rock, y = Std_Dev_Slope_s)) +
  geom_point(color = "orange1") +
  labs(title = "e)") +
  sm_statCorr(corr_method="spearman", color = "orange3", linetype = "dashed")+
  labs(x = "Rock (%)", y = "SD Slope")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugSTDrock

rugRATIOrock <- ggplot(sitefull_s, aes(x = Rock, y = Ratio_s)) +
  geom_point(color = "orange1") +
  labs(title = "f)") +
  sm_statCorr(corr_method="spearman", color = "orange3", linetype = "dashed")+
  labs(x = "Rock (%)", y = "Ratio")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugRATIOrock

# ROV_Rock_Slope <- ggplot(sitefull_s, aes(x = Slope_s, y = Rock_s)) +
#   geom_point(color = "orange1") +
#   sm_statCorr(corr_method="spearman", color = "orange3")+
#   labs(x = "ROV Slope scaled", y = "Rock scaled")+
#   theme_bw()+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# ROV_Rock_Slope

ROV_Rock_Rugosity <- ggplot(sitefull_s, aes(x = Rock, y = Rugosity_s )) +
  geom_point(color = "orange1") +
  labs(title = "g)") +
  sm_statCorr(corr_method="spearman", color = "orange3", linetype = "dashed")+
  labs(x = "Rock (%)", y = "ROV")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ROV_Rock_Rugosity

## box plot the ROCK data to make sense of the value distribution 
sitefull_s_subRock <- sitefull_s %>%
  dplyr::select("Site_ID", "Rock_s", "Std_Dev_Slope_s", "Ratio_s", "Rugosity_s")

sitefull_s_longRock <- sitefull_s_subRock %>%
  rename(
    "ROV" = Rugosity_s,
    "SD Slope" = Std_Dev_Slope_s,
    "Ratio" = Ratio_s,
    "Rock (%)" = Rock_s
  ) %>%
  pivot_longer(cols = c(ROV, `SD Slope`, `Ratio`, 'Rock (%)'),
               names_to = "Method",
               values_to = "Rock")

# Define the desired order of Method levels
method_order <- c("SD Slope","Ratio", "ROV","Rock (%)")  # Replace with your actual method names and desired order

# Convert Method to a factor with specified levels and order
sitefull_s_longRock$Method <- factor(sitefull_s_longRock$Method, levels = method_order)

rock_box<-ggplot(sitefull_s_longRock, aes(x = Method, y = Rock )) +
  geom_boxplot(color = "orange3") +
  geom_jitter(width = 0.2, alpha = 0.6, color= "orange1") +
  theme_bw()+
  labs(title = "h)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
rock_box

allrug_rock<-grid.arrange(rugSTDROV, rugRatioROV, rugRatioSTD, rug_box,
                      rugSTDrock,rugRATIOrock,ROV_Rock_Rugosity, rock_box, nrow = 2, respect=TRUE)


ggsave("figures/Rug_Rock_spearmancor.png", plot = allrug_rock, width = 25, height = 25, units = "cm")


#############################################

####test slope from ROV and Echosounder for significant difference (Spearman rank correlation)####
slope5_spear<-cor.test(sitefull$Slope, sitefull$Average_5m_slope,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(slope5_spear)
#rho = 0.69, p = 0.000001 (moderate significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

#test for significant difference with wilcoxon signed-rank test for paired/dependent samples
slope5_wilcox<-wilcox.test(sitefull_s$Slope_s, sitefull_s$Average_5m_slope_s, exact= FALSE, paired=TRUE) #exact = FALSE gets rid of impact of tied values on ranking, paired=TRUE makes it a paired test for dependent samples
print(slope5_wilcox)
#p=0.4 (not significant so no sign. difference)


#test 20m slope from ROV and Echosounder for significant difference (Spearman rank correlation)
slope20_spear<-cor.test(sitefull$Slope, sitefull$Average_20m_slope,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(slope20_spear)
#rho = 0.65, p = 0.000006 (moderate significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

#test for significant difference with wilcoxon signed-rank test for paired/dependent samples
slope20_wilcox<-wilcox.test(sitefull_s$Slope_s, sitefull_s$Average_20m_slope_s, exact= FALSE, paired=TRUE) #exact = FALSE gets rid of impact of tied values on ranking, paired=TRUE makes it a paired test for dependent samples
print(slope20_wilcox)
#p=0.3 (not significant so no sign. difference)
slope20_s <- ggplot(sitefull_s, aes(x = Slope_s, y = Average_20m_slope_s)) +
  geom_point(color = "indianred2") +
  labs(title = "i)") +
  sm_statCorr(corr_method="spearman", color = "indianred3", linetype = "dashed")+
  labs(x = "ROV", y = "20m")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope20_s

slope5_s <- ggplot(sitefull_s, aes(x = Slope_s, y = Average_5m_slope_s)) +
  geom_point(color = "indianred2") +
  labs(title = "j)") +
  sm_statCorr(corr_method="spearman", color = "indianred3", linetype = "dashed")+
  labs(x = "ROV", y = "5m")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope5_s

colors()

slope20_5_s <- ggplot(sitefull_s, aes(x = Average_5m_slope_s, y = Average_20m_slope_s)) +
  geom_point(color = "indianred2") +
  labs(title = "k)") +
  sm_statCorr(corr_method="spearman", color = "indianred3", linetype = "dashed")+
  labs(x = "5m", y = "20m")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope20_5_s



## box plot the SLOPE data to make sense of the value distribution 
sitefull_s_sub <- sitefull_s %>%
  dplyr::select("Site_ID", "Slope_s", "Average_5m_slope_s", "Average_20m_slope_s")
str(sitefull_s_sub)

sitefull_s_long <- sitefull_s_sub %>%
  rename(
    "ROV" = Slope_s,
    "5m" = Average_5m_slope_s,
    "20m" = Average_20m_slope_s
  ) %>%
  pivot_longer(cols = c('ROV', `5m`,`20m` ),
               names_to = "Method",
               values_to = "Slope")

slopebox_s<- ggplot(sitefull_s_long, aes(x = Method, y = Slope )) +
  geom_boxplot(color = "indianred4") +
  labs(title = "l)") +
  geom_jitter(width = 0.2, alpha = 0.6, color = "indianred2") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
slopebox_s

lp("gtable")
lp("grid")

row1 <- arrangeGrob(
  rugSTDROV, rugRatioROV, rugRatioSTD, rug_box,
  ncol = 4, 
  top = textGrob("Echosounder vs. ROV rugosity metrics", gp = gpar(fontsize = 12, fontface = "bold"))
)

row2 <- arrangeGrob(
  rugSTDrock, rugRATIOrock, ROV_Rock_Rugosity, rock_box,
  ncol = 4, 
  top = textGrob("All rugosity metrics vs. rock cover", gp = gpar(fontsize = 12, fontface = "bold"))
)

row3 <- arrangeGrob(
  slope20_s, slope5_s, slope20_5_s, slopebox_s,
  ncol = 4, 
  top = textGrob("Echosounder vs. ROV slope metrics", gp = gpar(fontsize = 12, fontface = "bold"))
)

# Combine all rows into one final layout
allrug_rock_slope <- arrangeGrob(
  row1, row2, row3,
  nrow = 3
)
grid.arrange(allrug_rock_slope)
ggsave("figures/allrug_rock_slope.png", plot = allrug_rock_slope, width = 25, height = 25, units = "cm")

#### try not scaled ####

slope5 <- ggplot(sitefull, aes(x = Slope, y = Average_5m_slope)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Slope", y = "Echosounder Slope (5m Average)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope5

slope20 <- ggplot(sitefull, aes(x = Slope, y = Average_20m_slope)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Slope", y = "Echosounder Slope (20m Average)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope20

sitefull_sub <- sitefull_s %>%
  dplyr::select("Site_ID", "Slope", "Average_5m_slope", "Average_20m_slope")
str(sitefull_s_sub)

sitefull_long <- sitefull_sub %>%
  rename(
    "ROV" = Slope,
    "Echosounder (5m)" = Average_5m_slope,
    "Echosounder (20m)" = Average_20m_slope
  ) %>%
  pivot_longer(cols = c(ROV, `Echosounder (5m)`,`Echosounder (20m)` ),
               names_to = "Method",
               values_to = "Slope")

slopebox<- ggplot(sitefull_long, aes(x = Method, y = Slope )) +
  geom_boxplot(color = "gray40") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
slopebox

allslope<-grid.arrange(slope5, slope20,slopebox,  ncol = 2, respect=TRUE)



ggsave("figures/ROV_Echo_Slope_spearmancor.png", plot = allslope, width = 25, height = 25, units = "cm")



###############################################################

####boxplots for NASC for each RFZ and each Deadzone buffer type####

## box plot for NASC in each RFZ
sitefull_NASC_RFZ <- sitefull %>%
  dplyr::select("Site_ID", "NASC_15_1m", "NASC_10_1m", "NASC_5_1m")
str(sitefull_NASC_RFZ)

sitefull_NASC_RFZlong <- sitefull_NASC_RFZ %>%
  rename(
    "15m RFZ" = NASC_15_1m,
    "10m RFZ" = NASC_10_1m,
    "5m RFZ" = NASC_5_1m
  ) %>%
  pivot_longer(cols = c(`15m RFZ`, `10m RFZ`,`5m RFZ` ),
               names_to = "Method",
               values_to = "NASC")

# Define the desired order of Method levels
method_order <- c("15m RFZ", "10m RFZ","5m RFZ")  # Replace with your actual method names and desired order

# Convert Method to a factor with specified levels and order
sitefull_NASC_RFZlong$Method <- factor(sitefull_NASC_RFZlong$Method, levels = method_order)


NASC_RFZ_box<- ggplot(sitefull_NASC_RFZlong, aes(x = Method, y = NASC )) +
  geom_boxplot(color = "gray40") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(title = "a)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
NASC_RFZ_box

### all values of NASC are significantly different for different Rockfish Zones
t_test_15_10 <- t.test(sitefull$NASC_15_1m, sitefull$NASC_10_1m, paired = TRUE)
t_test_15_10 #signficantly different t = 3.3079, df = 38, p-value = 0.002063

t_test_15_5 <- t.test(sitefull$NASC_15_1m, sitefull$NASC_5_1m, paired = TRUE)
t_test_15_5 #signficantly different t = 4.5484, df = 38, p-value = 5.382e-05

t_test_10_5 <- t.test(sitefull$NASC_10_1m, sitefull$NASC_5_1m, paired = TRUE)
t_test_10_5 #signficantly different t = 3.961, df = 38, p-value = 0.000317

#double check with non-parametric test
wilcox15_10<-wilcox.test(sitefull$NASC_10_1m, sitefull$NASC_5_1m, exact= FALSE, paired=TRUE) #exact = FALSE gets rid of impact of tied values on ranking, paired=TRUE makes it a paired test for dependent samples
print(wilcox15_10)


## box plot for NASC at 10m for each DEADZONE buffer
sitefull_NASC_DZ <- sitefull %>%
  dplyr::select("Site_ID", "NASC_10_LG", "NASC_10_MAN", "NASC_10_1m")
str(sitefull_NASC_DZ)

sitefull_NASC_DZlong <- sitefull_NASC_DZ %>%
  rename(
    "Large Buffer" = NASC_10_LG,
    "Manual Buffer" = NASC_10_MAN,
    "1m Buffer" = NASC_10_1m
  ) %>%
  pivot_longer(cols = c(`Large Buffer`, `Manual Buffer`,`1m Buffer` ),
               names_to = "Method",
               values_to = "NASC")

# Define the desired order of Method levels
method_order <- c("Large Buffer", "Manual Buffer","1m Buffer")  # Replace with your actual method names and desired order

# Convert Method to a factor with specified levels and order
sitefull_NASC_DZlong$Method <- factor(sitefull_NASC_DZlong$Method, levels = method_order)


### if we want to include this plot will need to go back and make sure cleaned data is what is loaded in here (e.g. are herring schools removed from LG and Manual
# deadzone files?)
NASC_DZ_box<- ggplot(sitefull_NASC_DZlong, aes(x = Method, y = NASC )) +
  geom_boxplot(color = "gray40") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(title = "b)") +
  theme_bw()+
 # geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
#+geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASC_DZ_box

### all values of NASC are significantly different for different Rockfish Zones
t_test_LG_MAN <- t.test(sitefull$NASC_10_LG, sitefull$NASC_10_MAN, paired = TRUE)
t_test_LG_MAN #signficantly different t = -2.4931, df = 38, p-value = 0.01713

t_test_LG_1m <- t.test(sitefull$NASC_10_LG, sitefull$NASC_10_1m, paired = TRUE)
t_test_LG_1m #signficantly different t = -4.1356, df = 38, p-value = 0.0001885

t_test_1m_MAN <- t.test(sitefull$NASC_10_1m, sitefull$NASC_10_MAN, paired = TRUE)
t_test_1m_MAN #NOT signficantly different t = -1.4666, df = 38, p-value = 0.1507


NASC_all<-grid.arrange(NASC_RFZ_box, NASC_DZ_box,  ncol = 2, respect=TRUE)
ggsave("figures/NASC_RFZ_DZ_boxes.png", plot = NASC_all, width = 25, height = 25, units = "cm")








#############################################################

####GLM ANALYSES####

#convert benthic count to density
sitefull$Ben<-sitefull$AbundanceNonSchooling/sitefull$Volume

####examining quadratic in relationship
#Echo slope vs fish density with quadratic line
BvS<-ggplot(sitefull, aes(x=Average_5m_slope, y=Ben))+
  geom_point()+
  geom_smooth(method="lm", formula = y ~ poly(x, 2, raw = TRUE),se=FALSE) + #formula = y ~ poly(x, 2, raw = TRUE) makes line a quadratic
labs(x = "Slope", y = "Benthic Fish Density")
#  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS

  par(mfrow = c(1, 1))
#slope with echoslope and fish count
ggplot(sitefull, aes(x = NASC_10_1m, y = Ben, color = Average_5m_slope)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +# You can adjust colors as per your preference
  geom_smooth(method="lm")+
  labs(x = "NASC", y = "Benthic Fish Density", color = "Slope")
  #geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)

###########################################

###echosounder density model (10m) #####

##############################################
##AIC selection on full model##
#steps - remove Rugosity (AIC 217, 55%), NASC_10_1m:Cumulative_LG_DZ_Area (AIC 219, 55%), Average_Depth (AIC 219 - 53%), 
#Cumulative_LG_DZ_Area (AIC 220, 52%)

###AIC model selection

#Full model with all vars and no quadratic for slope
NASC_Slope_Rugosity_Depth_DZ_DZNASC_NASCSLOPE <- glm(Ben ~ NASC_10_1m + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_10_1m:Average_5m_slope + NASC_10_1m:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(NASC_Slope_Rugosity_Depth_DZ_DZNASC_NASCSLOPE)

#remove rugosity
NASC_Slope_Depth_DZ_DZNASC_NASCSLOPE <- glm(Ben ~ NASC_10_1m + Average_5m_slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_10_1m:Average_5m_slope + NASC_10_1m:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(NASC_Slope_Depth_DZ_DZNASC_NASCSLOPE)

#remove NASC/Deadzone Interaction
NASC_Slope_Depth_DZ_NASCSLOPE <- glm(Ben ~ NASC_10_1m + Average_5m_slope + Average_Depth + Cumulative_LG_DZ_Area +
               NASC_10_1m:Average_5m_slope, 
             family = gaussian,  data = sitefull)
summary(NASC_Slope_Depth_DZ_NASCSLOPE)

#remove Depth
NASC_Slope_DZ_NASCSLOPE <- glm(Ben ~ NASC_10_1m + Average_5m_slope + Cumulative_LG_DZ_Area +
                       NASC_10_1m:Average_5m_slope, 
                     family = gaussian,  data = sitefull)
summary(NASC_Slope_DZ_NASCSLOPE)



#remove Deadzone
NASC_Slope_NASCSLOPE <- glm(Ben ~ NASC_10_1m + Average_5m_slope + 
                          NASC_10_1m:Average_5m_slope, 
                        family = gaussian,  data = sitefull)
summary(NASC_Slope_NASCSLOPE)
AIC(NASC_Slope_NASCSLOPE)

#remove Slope and slope/NASC Interaction
NASC <- glm(Ben ~ NASC_10_1m , 
                           family = gaussian,  data = sitefull)
summary(NASC)
AIC(NASC)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((NASC_Slope_NASCSLOPE)$null.deviance-(NASC_Slope_NASCSLOPE)$deviance)/(NASC_Slope_NASCSLOPE)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

lp("AICcmodavg")

###put all models in a list to compare AIC weights
models <- list(NASC_Slope_Rugosity_Depth_DZ_DZNASC_NASCSLOPE, NASC_Slope_Depth_DZ_DZNASC_NASCSLOPE, NASC_Slope_Depth_DZ_NASCSLOPE, NASC_Slope_DZ_NASCSLOPE, NASC_Slope_NASCSLOPE, NASC)

model.names <- c('NASC_Slope_Rugosity_Depth_DZ_DZNASC_NASCSLOPE', 'NASC_Slope_Depth_DZ_DZNASC_NASCSLOPE', 'NASC_Slope_Depth_DZ_NASCSLOPE', 'NASC_Slope_DZ_NASCSLOPE', 'NASC_Slope_NASCSLOPE', 'NASC')

AIC_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_results)

###model validation plots

par(mfrow = c(2, 2))
plot(NASC_Slope_NASCSLOPE)

check_overdispersion(NASC_Slope_NASCSLOPE)


#####calculate residuals and add to dataframe
residuals <- residuals(NASC_Slope_NASCSLOPE)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

E1<-ggplot(TF2) +
  geom_point(aes(x = Average_5m_slope,
                 y = resid)) +
  geom_smooth(aes(x = Average_5m_slope,
                  y = resid),
              method = "lm")

E2<-ggplot(TF2) +
  geom_point(aes(x = Std_Dev_Slope,
                 y = resid)) +
  geom_smooth(aes(x = Std_Dev_Slope,
                  y = resid),
              method = "lm")

E3<-ggplot(TF2) +
  geom_point(aes(x = Cumulative_LG_DZ_Area,
                 y = resid)) +
  geom_smooth(aes(x = Cumulative_LG_DZ_Area,
                  y = resid),
              method = "lm")

E4<-ggplot(TF2) +
  geom_point(aes(x = NASC_10_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_10_1m,
                  y = resid),
              method = "lm")

E5<-ggplot(TF2) +
  geom_point(aes(x = Average_Depth,
                 y = resid)) +
  geom_smooth(aes(x = Average_Depth,
                  y = resid),
              method = "lm")

resVpred<-grid.arrange(E1, E2, E3, E4, E5, nrow = 2, respect=TRUE)

#check model fit with DHARMa tests
r <- simulateResiduals(NASC_Slope_NASCSLOPE, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(NASC_Slope_NASCSLOPE)


#######################################
#5m model
#### Echo model with 5m RFZ NASC######

#Full model with all vars and no quadratic for slope (problems with residuals)
NASC5_Slope_Rugosity_Depth_DZ_DZNASC5_NASC5SLOPE <- glm(Ben ~ NASC_5_1m + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_5_1m:Average_5m_slope + NASC_5_1m:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(NASC5_Slope_Rugosity_Depth_DZ_DZNASC5_NASC5SLOPE)
AIC(NASC5_Slope_Rugosity_Depth_DZ_DZNASC5_NASC5SLOPE) #-205 (42% exp dev)

#remove rugosity
NASC5_Slope_Depth_DZ_DZNASC5_NASC5SLOPE <- glm(Ben ~ NASC_5_1m + Average_5m_slope + Average_Depth + Cumulative_LG_DZ_Area +
                                                          NASC_5_1m:Average_5m_slope + NASC_5_1m:Cumulative_LG_DZ_Area, 
                                                        family = gaussian,  data = sitefull)
summary(NASC5_Slope_Depth_DZ_DZNASC5_NASC5SLOPE)
AIC(NASC5_Slope_Depth_DZ_DZNASC5_NASC5SLOPE) #-205 (42% exp dev)

#remove DZ/NASC
NASC5_Slope_Depth_DZ_NASC5SLOPE <- glm(Ben ~ NASC_5_1m + Average_5m_slope + Average_Depth + Cumulative_LG_DZ_Area +
                                                 NASC_5_1m:Average_5m_slope, 
                                               family = gaussian,  data = sitefull)
summary(NASC5_Slope_Depth_DZ_NASC5SLOPE)
AIC(NASC5_Slope_Depth_DZ_NASC5SLOPE) #-205 (42% exp dev)

#remove DZ
NASC5_Slope_Depth_NASC5SLOPE <- glm(Ben ~ NASC_5_1m + Average_5m_slope + Average_Depth +
                                         NASC_5_1m:Average_5m_slope, 
                                       family = gaussian,  data = sitefull)
summary(NASC5_Slope_Depth_NASC5SLOPE)
AIC(NASC5_Slope_Depth_NASC5SLOPE) #-205 (42% exp dev)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((NASC5_Slope_Depth_NASC5SLOPE)$null.deviance-(NASC5_Slope_Depth_NASC5SLOPE)$deviance)/(NASC5_Slope_Depth_NASC5SLOPE)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#remove Depth
NASC5_Slope_NASC5SLOPE <- glm(Ben ~ NASC_5_1m + Average_5m_slope +
                                NASC_5_1m:Average_5m_slope, 
                              family = gaussian,  data = sitefull)
summary(NASC5_Slope_NASC5SLOPE)
AIC(NASC5_Slope_NASC5SLOPE) #-205 (42% exp dev)

#remove slope and slope/nasc
NASC5 <- glm(Ben ~ NASC_5_1m, 
                                    family = gaussian,  data = sitefull)
summary(NASC5)
AIC(NASC5) #-205 (42% exp dev)


###put all models in a list to compare AIC weights
models <- list(NASC5_Slope_Rugosity_Depth_DZ_DZNASC5_NASC5SLOPE, NASC5_Slope_Depth_DZ_DZNASC5_NASC5SLOPE, NASC5_Slope_Depth_DZ_NASC5SLOPE, NASC5_Slope_Depth_NASC5SLOPE, NASC5_Slope_NASC5SLOPE, NASC5)

model.names <- c('NASC5_Slope_Rugosity_Depth_DZ_DZNASC5_NASC5SLOPE', 'NASC5_Slope_Depth_DZ_DZNASC5_NASC5SLOPE', 'NASC5_Slope_Depth_DZ_NASC5SLOPE', 'NASC5_Slope_Depth_NASC5SLOPE', 'NASC5_Slope_NASC5SLOPE', 'NASC5')

AIC_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_results)

###model validation 5m RFZ model

par(mfrow = c(2, 2))
plot(NASC5_Slope_NASC5SLOPE)

check_overdispersion(NASC5_Slope_NASC5SLOPE)


#####calculate residuals and add to dataframe
residuals <- residuals(NASC5_Slope_NASC5SLOPE)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

E1<-ggplot(TF2) +
  geom_point(aes(x = Average_5m_slope,
                 y = resid)) +
  geom_smooth(aes(x = Average_5m_slope,
                  y = resid),
              method = "lm")

E2<-ggplot(TF2) +
  geom_point(aes(x = Std_Dev_Slope,
                 y = resid)) +
  geom_smooth(aes(x = Std_Dev_Slope,
                  y = resid),
              method = "lm")

E3<-ggplot(TF2) +
  geom_point(aes(x = Cumulative_LG_DZ_Area,
                 y = resid)) +
  geom_smooth(aes(x = Cumulative_LG_DZ_Area,
                  y = resid),
              method = "lm")

E4<-ggplot(TF2) +
  geom_point(aes(x = NASC_5_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_5_1m,
                  y = resid),
              method = "lm")

E5<-ggplot(TF2) +
  geom_point(aes(x = Average_Depth,
                 y = resid)) +
  geom_smooth(aes(x = Average_Depth,
                  y = resid),
              method = "lm")

resVpred<-grid.arrange(E1, E2, E3, E4, E5, nrow = 2, respect=TRUE)

#check model fit with DHARMa tests
r <- simulateResiduals(NASC5_Slope_NASC5SLOPE, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(NASC5_Slope_NASC5SLOPE)

#######################################################
#15m model
#### Echo model with 15m RFZ NASC######

#Full model with all vars and no quadratic for slope (problems with residuals)
NASC15_Slope_Rugosity_Depth_DZ_DZNASC15_NASC15SLOPE <- glm(Ben ~ NASC_15_1m + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
                                                          NASC_15_1m:Average_5m_slope + NASC_15_1m:Cumulative_LG_DZ_Area, 
                                                        family = gaussian,  data = sitefull)
summary(NASC15_Slope_Rugosity_Depth_DZ_DZNASC15_NASC15SLOPE)
AIC(NASC15_Slope_Rugosity_Depth_DZ_DZNASC15_NASC15SLOPE) #-205 (42% exp dev)

#remove rugosity
NASC15_Slope_Depth_DZ_DZNASC15_NASC15SLOPE <- glm(Ben ~ NASC_15_1m + Average_5m_slope + Average_Depth + Cumulative_LG_DZ_Area +
                                                             NASC_15_1m:Average_5m_slope + NASC_15_1m:Cumulative_LG_DZ_Area, 
                                                           family = gaussian,  data = sitefull)
summary(NASC15_Slope_Depth_DZ_DZNASC15_NASC15SLOPE)
AIC(NASC15_Slope_Depth_DZ_DZNASC15_NASC15SLOPE) #-205 (42% exp dev)

#remove DZ/NASC
NASC15_Slope_Depth_DZ_NASC15SLOPE <- glm(Ben ~ NASC_15_1m + Average_5m_slope + Average_Depth + Cumulative_LG_DZ_Area +
                                                    NASC_15_1m:Average_5m_slope, 
                                                  family = gaussian,  data = sitefull)
summary(NASC15_Slope_Depth_DZ_NASC15SLOPE)
AIC(NASC15_Slope_Depth_DZ_NASC15SLOPE) #-205 (42% exp dev)

#remove DZ
NASC15_Slope_Depth_NASC15SLOPE <- glm(Ben ~ NASC_15_1m + Average_5m_slope + Average_Depth + 
                                        NASC_15_1m:Average_5m_slope, 
                                      family = gaussian,  data = sitefull)
summary(NASC15_Slope_Depth_NASC15SLOPE)
AIC(NASC15_Slope_Depth_NASC15SLOPE) #-205 (42% exp dev)

#remove Depth
NASC15_Slope_NASC15SLOPE <- glm(Ben ~ NASC_15_1m + Average_5m_slope + 
                                        NASC_15_1m:Average_5m_slope, 
                                      family = gaussian,  data = sitefull)
summary(NASC15_Slope_NASC15SLOPE)
AIC(NASC15_Slope_NASC15SLOPE) #-205 (42% exp dev)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((NASC15_Slope_NASC15SLOPE)$null.deviance-(NASC15_Slope_NASC15SLOPE)$deviance)/(NASC15_Slope_NASC15SLOPE)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#remove slope and slope/nasc
NASC15 <- glm(Ben ~ NASC_15_1m, 
                                family = gaussian,  data = sitefull)
summary(NASC15)
AIC(NASC15) #-205 (42% exp dev)


###put all models in a list to compare AIC weights
models <- list(NASC15_Slope_Rugosity_Depth_DZ_DZNASC15_NASC15SLOPE, NASC15_Slope_Depth_DZ_DZNASC15_NASC15SLOPE, NASC15_Slope_Depth_DZ_NASC15SLOPE, NASC15_Slope_Depth_NASC15SLOPE, NASC15_Slope_NASC15SLOPE, NASC15)

model.names <- c('NASC15_Slope_Rugosity_Depth_DZ_DZNASC15_NASC15SLOPE', 'NASC15_Slope_Depth_DZ_DZNASC15_NASC15SLOPE', 'NASC15_Slope_Depth_DZ_NASC15SLOPE', 'NASC15_Slope_Depth_NASC15SLOPE', 'NASC15_Slope_NASC15SLOPE', 'NASC15')

AIC_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_results)

###model validation 15m RFZ model

par(mfrow = c(2, 2))
plot(NASC15_Slope_NASC15SLOPE)

check_overdispersion(NASC15_Slope_NASC15SLOPE)


#####calculate residuals and add to dataframe
residuals <- residuals(NASC15_Slope_NASC15SLOPE)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

E1<-ggplot(TF2) +
  geom_point(aes(x = Average_5m_slope,
                 y = resid)) +
  geom_smooth(aes(x = Average_5m_slope,
                  y = resid),
              method = "lm")

E2<-ggplot(TF2) +
  geom_point(aes(x = Std_Dev_Slope,
                 y = resid)) +
  geom_smooth(aes(x = Std_Dev_Slope,
                  y = resid),
              method = "lm")

E3<-ggplot(TF2) +
  geom_point(aes(x = Cumulative_LG_DZ_Area,
                 y = resid)) +
  geom_smooth(aes(x = Cumulative_LG_DZ_Area,
                  y = resid),
              method = "lm")

E4<-ggplot(TF2) +
  geom_point(aes(x = NASC_15_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_15_1m,
                  y = resid),
              method = "lm")

E5<-ggplot(TF2) +
  geom_point(aes(x = Average_Depth,
                 y = resid)) +
  geom_smooth(aes(x = Average_Depth,
                  y = resid),
              method = "lm")

resVpred<-grid.arrange(E1, E2, E3, E4, E5, nrow = 2, respect=TRUE)

#check model fit with DHARMa tests
r <- simulateResiduals(NASC15_Slope_NASC15SLOPE, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(NASC15_Slope_NASC15SLOPE)

#################################################

######density model with ROV data#####

#### ROV model - Slope, Rug, Depth only####
#model using only variables also available from echosounder (best model - 18% dev explained)
###full model
Slope_Rugosity_Rock_ROCKSLOPE <- glm(Ben ~  Slope + Rugosity + Rock +Rock:Slope,
                                 family = gaussian,  data = sitefull)
summary(Slope_Rugosity_Rock_ROCKSLOPE)
AIC(Slope_Rugosity_Rock_ROCKSLOPE)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((Slope_Rugosity_Rock_ROCKSLOPE)$null.deviance-(Slope_Rugosity_Rock_ROCKSLOPE)$deviance)/(Slope_Rugosity_Rock_ROCKSLOPE)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

Slope_Rugosity_Depth_Rock <- glm(Ben ~  Slope + Rugosity + Depth+Rock,
          family = gaussian,  data = sitefull)
summary(Slope_Rugosity_Depth_Rock)
AIC(Slope_Rugosity_Depth_Rock) #-199 (18% Dev Exp)

Slope_Rugosity_Rock <- glm(Ben ~  Slope + Rugosity + Rock,
                                 family = gaussian,  data = sitefull)
summary(Slope_Rugosity_Rock)
AIC(Slope_Rugosity_Rock) #-199 (18% Dev Exp)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((Slope_Rugosity_Rock)$null.deviance-(Slope_Rugosity_Rock)$deviance)/(Slope_Rugosity_Rock)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

Rugosity_Rock <- glm(Ben ~ Rugosity + Rock,
                           family = gaussian,  data = sitefull)
summary(Rugosity_Rock)
AIC(Rugosity_Rock) #-199 (18% Dev Exp)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((Rugosity_Rock)$null.deviance-(Rugosity_Rock)$deviance)/(Rugosity_Rock)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

Slope_Rock <- glm(Ben ~ Slope + Rock,
                     family = gaussian,  data = sitefull)
summary(Slope_Rock)
AIC(Slope_Rock) #-199 (18% Dev Exp)

###AIC selection BEST MODEL (remove Depth)
Slope_Rugosity <- glm(Ben ~  Slope + Rugosity,
          family = gaussian,  data = sitefull)
summary(Slope_Rugosity)
AIC(Slope_Rugosity) #-201 (17% Dev Exp)


###put all models in a list to compare AIC weights
models <- list(Slope_Rugosity_Depth_Rock, Slope_Rugosity_Rock,Rugosity_Rock,Slope_Rock, Slope_Rugosity,Slope_Rugosity_Rock_ROCKSLOPE)

model.names <- c('Slope_Rugosity_Depth_Rock', 'Slope_Rugosity_Rock','Rugosity_Rock','Slope_Rock', 'Slope_Rugosity', 'Slope_Rugosity_Rock_ROCKSLOPE')

AIC_ROV_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_ROV_results)

### model validation for ROV model

par(mfrow = c(2, 2))
plot(Slope_Rugosity_Rock)

check_overdispersion(Slope_Rugosity_Rock)


#####calculate residuals and add to dataframe
residuals <- residuals(Slope_Rugosity_Rock)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)


E1<-ggplot(TF2) +
  geom_point(aes(x = Slope,
                 y = resid)) +
  geom_smooth(aes(x = Slope,
                  y = resid),
              method = "lm")

E2<-ggplot(TF2) +
  geom_point(aes(x = Rugosity,
                 y = resid)) +
  geom_smooth(aes(x = Rugosity,
                  y = resid),
              method = "lm")

E3<-ggplot(TF2) +
  geom_point(aes(x = Rock,
                 y = resid)) +
  geom_smooth(aes(x = Rock,
                  y = resid),
              method = "lm")

E4<-ggplot(TF2) +
  geom_point(aes(x = Depth,
                 y = resid)) +
  geom_smooth(aes(x = Depth,
                  y = resid),
              method = "lm")


resVpred<-grid.arrange(E1, E2, E3, E4,  nrow = 2, respect=TRUE)

#check model fit with DHARMa tests
r <- simulateResiduals(Slope_Rugosity_Rock, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(D1)





##### ROV/ECHO hybrid model with rock and NASC included ####
## Full model
Slope_Rug_Dep_Rock_NASC_NASCSlope <- glm(Ben ~  Slope + Rugosity + Depth + Rock +NASC_10_1m +
            NASC_10_1m:Slope,
          family = gaussian,  data = sitefull)
summary(Slope_Rug_Dep_Rock_NASC_NASCSlope)
AIC(Slope_Rug_Dep_Rock_NASC_NASCSlope) #-220 (59% Dev Exp)

Slope_Rug_Rock_NASC_NASCSlope <- glm(Ben ~  Slope + Rugosity  + Rock +NASC_10_1m +
                                           NASC_10_1m:Slope,
                                         family = gaussian,  data = sitefull)
summary(Slope_Rug_Rock_NASC_NASCSlope)
AIC(Slope_Rug_Rock_NASC_NASCSlope) #-220 (59% Dev Exp)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((Slope_Rug_Rock_NASC_NASCSlope)$null.deviance-(Slope_Rug_Rock_NASC_NASCSlope)$deviance)/(Slope_Rug_Rock_NASC_NASCSlope)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

Slope_Rug_NASC_NASCSlope <- glm(Ben ~  Slope + Rugosity +NASC_10_1m +
                                       NASC_10_1m:Slope,
                                     family = gaussian,  data = sitefull)
summary(Slope_Rug_NASC_NASCSlope)
AIC(Slope_Rug_NASC_NASCSlope) #-220 (59% Dev Exp)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((Slope_Rug_NASC_NASCSlope)$null.deviance-(Slope_Rug_NASC_NASCSlope)$deviance)/(Slope_Rug_NASC_NASCSlope)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

Slope_NASC_NASCSlope <- glm(Ben ~  Slope  +NASC_10_1m +
                                  NASC_10_1m:Slope,
                                family = gaussian,  data = sitefull)
summary(Slope_NASC_NASCSlope)
AIC(Slope_NASC_NASCSlope) #-220 (59% Dev Exp)

###put all models in a list to compare AIC weights
models <- list(Slope_Rug_Dep_Rock_NASC_NASCSlope, Slope_Rug_Rock_NASC_NASCSlope,Slope_Rug_NASC_NASCSlope, Slope_NASC_NASCSlope)

model.names <- c('Slope_Rug_Dep_Rock_NASC_NASCSlope', 'Slope_Rug_Rock_NASC_NASCSlope','Slope_Rug_NASC_NASCSlope', 'Slope_NASC_NASCSlope')

AIC_hybrid_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_hybrid_results)


### model validation for hybrid model

par(mfrow = c(2, 2))
plot(Slope_Rug_NASC_NASCSlope)

check_overdispersion(Slope_Rug_NASC_NASCSlope)


#####calculate residuals and add to dataframe
residuals <- residuals(Slope_Rug_NASC_NASCSlope)
TF2 <- sitefull %>%
  mutate(resid = residuals)

hist(sitefull$Rock)

sitefullT<-sitefull
sitefullT$RockT<-sitefullT$Rock/100

sitefullT$RockTlogit<-logit(sitefullT$RockT)
hist(sitefullT$RockTlogit)

## plot residuals vs predictors (looking for flat line with lm)


E1<-ggplot(TF2) +
  geom_point(aes(x = Slope,
                 y = resid)) +
  geom_smooth(aes(x = Slope,
                  y = resid),
              method = "lm")

E2<-ggplot(TF2) +
  geom_point(aes(x = Rugosity,
                 y = resid)) +
  geom_smooth(aes(x = Rugosity,
                  y = resid),
              method = "lm")

E3<-ggplot(TF2) +
  geom_point(aes(x = Rock,
                 y = resid)) +
  geom_smooth(aes(x = Rock,
                  y = resid),
              method = "lm")

E4<-ggplot(TF2) +
  geom_point(aes(x = Depth,
                 y = resid)) +
  geom_smooth(aes(x = Depth,
                  y = resid),
              method = "lm")

E5<-ggplot(TF2) +
  geom_point(aes(x = NASC_10_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_10_1m,
                  y = resid),
              method = "lm")


resVpred<-grid.arrange(E1, E2, E3, E4, E5,  nrow = 2, respect=TRUE)

#check model fit with DHARMa tests
r <- simulateResiduals(Slope_Rug_NASC_NASCSlope, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)


#######################################################
####different deadzone in echo models####
##########################################
#10m model w Manual deadzone
####AIC selection on full echo model 10m RFZ WITH quadratic####
#steps - remove Rugosity (AIC 217), NASC_10_1m:Cumulative_LG_DZ_Area (AIC 218), Average_Depth (AIC 219 - 52%), 
#Cumulative_LG_DZ_Area (AIC 220, 52%)


###10m MAN DZ##### - not significant with any vars removed
#Full model with all vars and no quadratic for slope
MANNASC_Slope_Rug_Depth_DZ_NASCSlope_NASCDZ <- glm(Ben ~ NASC_10_MAN + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_10_MAN:Average_5m_slope + NASC_10_MAN:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(MANNASC_Slope_Rug_Depth_DZ_NASCSlope_NASCDZ)

#remove Depth
MANNASC_Slope_Rug_DZ_NASCSlope_NASCDZ <- glm(Ben ~ NASC_10_MAN + Average_5m_slope + Std_Dev_Slope + Cumulative_LG_DZ_Area +
                                                     NASC_10_MAN:Average_5m_slope + NASC_10_MAN:Cumulative_LG_DZ_Area, 
                                                   family = gaussian,  data = sitefull)
summary(MANNASC_Slope_Rug_DZ_NASCSlope_NASCDZ)

#remove Rugosity
MANNASC_Slope_DZ_NASCSlope_NASCDZ <- glm(Ben ~ NASC_10_MAN + Average_5m_slope + Cumulative_LG_DZ_Area +
                                               NASC_10_MAN:Average_5m_slope + NASC_10_MAN:Cumulative_LG_DZ_Area, 
                                             family = gaussian,  data = sitefull)
summary(MANNASC_Slope_DZ_NASCSlope_NASCDZ)

#remove NASCDZ
MANNASC_Slope_DZ_NASCSlope <- glm(Ben ~ NASC_10_MAN + Average_5m_slope + Cumulative_LG_DZ_Area +
                                    NASC_10_MAN:Average_5m_slope, 
                                  family = gaussian,  data = sitefull)
summary(MANNASC_Slope_DZ_NASCSlope)

#remove DZ
MANNASC_Slope_NASCSlope <- glm(Ben ~ NASC_10_MAN + Average_5m_slope + 
                                           NASC_10_MAN:Average_5m_slope, 
                                         family = gaussian,  data = sitefull)
summary(MANNASC_Slope_NASCSlope)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((MANNASC_Slope_NASCSlope)$null.deviance-(MANNASC_Slope_NASCSlope)$deviance)/(MANNASC_Slope_NASCSlope)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#remove Slope
MANNASC <- glm(Ben ~ NASC_10_MAN , 
                               family = gaussian,  data = sitefull)
summary(MANNASC)


###put all models in a list to compare AIC weights
models <- list(MANNASC_Slope_Rug_Depth_DZ_NASCSlope_NASCDZ, MANNASC_Slope_Rug_DZ_NASCSlope_NASCDZ, MANNASC_Slope_DZ_NASCSlope_NASCDZ, MANNASC_Slope_DZ_NASCSlope,
               MANNASC_Slope_NASCSlope, MANNASC)

model.names <- c('MANNASC_Slope_Rug_Depth_DZ_NASCSlope_NASCDZ', 'MANNASC_Slope_Rug_DZ_NASCSlope_NASCDZ', 'MANNASC_Slope_DZ_NASCSlope_NASCDZ', 'MANNASC_Slope_DZ_NASCSlope',
                 'MANNASC_Slope_NASCSlope', 'MANNASC')

AIC_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_results)

###model validation 5m RFZ model

par(mfrow = c(2, 2))
plot(MANNASC_Slope_NASCSlope)

check_overdispersion(MANNASC_Slope_NASCSlope)


#####calculate residuals and add to dataframe
residuals <- residuals(MANNASC_Slope_NASCSlope)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

E1<-ggplot(TF2) +
  geom_point(aes(x = Average_5m_slope,
                 y = resid)) +
  geom_smooth(aes(x = Average_5m_slope,
                  y = resid),
              method = "lm")

E2<-ggplot(TF2) +
  geom_point(aes(x = Std_Dev_Slope,
                 y = resid)) +
  geom_smooth(aes(x = Std_Dev_Slope,
                  y = resid),
              method = "lm")

E3<-ggplot(TF2) +
  geom_point(aes(x = Cumulative_LG_DZ_Area,
                 y = resid)) +
  geom_smooth(aes(x = Cumulative_LG_DZ_Area,
                  y = resid),
              method = "lm")

E4<-ggplot(TF2) +
  geom_point(aes(x = NASC_10_MAN,
                 y = resid)) +
  geom_smooth(aes(x = NASC_10_MAN,
                  y = resid),
              method = "lm")

E5<-ggplot(TF2) +
  geom_point(aes(x = Average_Depth,
                 y = resid)) +
  geom_smooth(aes(x = Average_Depth,
                  y = resid),
              method = "lm")

resVpred<-grid.arrange(E1, E2, E3, E4, E5, nrow = 2, respect=TRUE)

#check model fit with DHARMa tests
r <- simulateResiduals(MANNASC_Slope_NASCSlope, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(MANNASC_Slope_NASCSlope)

##########################################

####10m LG DZ##### 
#Full model with all vars and no quadratic for slope
LGNASC_Slope_Rug_Depth_DZ_NASCSlope_NASCDZ <- glm(Ben ~ NASC_10_LG + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
                                                     NASC_10_LG:Average_5m_slope + NASC_10_LG:Cumulative_LG_DZ_Area, 
                                                   family = gaussian,  data = sitefull)
summary(LGNASC_Slope_Rug_Depth_DZ_NASCSlope_NASCDZ)

#remove Depth
LGNASC_Slope_Rug_DZ_NASCSlope_NASCDZ <- glm(Ben ~ NASC_10_LG + Average_5m_slope + Std_Dev_Slope  + Cumulative_LG_DZ_Area +
                                              NASC_10_LG:Average_5m_slope + NASC_10_LG:Cumulative_LG_DZ_Area, 
                                            family = gaussian,  data = sitefull)
summary(LGNASC_Slope_Rug_DZ_NASCSlope_NASCDZ)

#remove Rugosity
LGNASC_Slope_DZ_NASCSlope_NASCDZ <- glm(Ben ~ NASC_10_LG + Average_5m_slope + Cumulative_LG_DZ_Area +
                                          NASC_10_LG:Average_5m_slope + NASC_10_LG:Cumulative_LG_DZ_Area, 
                                        family = gaussian,  data = sitefull)
summary(LGNASC_Slope_DZ_NASCSlope_NASCDZ)

#remove NASCDZ
LGNASC_Slope_NASCSlope <- glm(Ben ~ NASC_10_LG + Average_5m_slope + 
                                   NASC_10_LG:Average_5m_slope , 
                                 family = gaussian,  data = sitefull)
summary(LGNASC_Slope_NASCSlope)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((LGNASC_Slope_NASCSlope)$null.deviance-(LGNASC_Slope_NASCSlope)$deviance)/(LGNASC_Slope_NASCSlope)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#remove Slope
LGNASC <- glm(Ben ~ NASC_10_LG , 
               family = gaussian,  data = sitefull)
summary(LGNASC)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((LGNASC)$null.deviance-(LGNASC)$deviance)/(LGNASC)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance


###put all models in a list to compare AIC weights
models <- list(LGNASC_Slope_Rug_Depth_DZ_NASCSlope_NASCDZ, LGNASC_Slope_Rug_DZ_NASCSlope_NASCDZ, LGNASC_Slope_DZ_NASCSlope_NASCDZ,
               LGNASC_Slope_NASCSlope, LGNASC)

model.names <- c('LGNASC_Slope_Rug_Depth_DZ_NASCSlope_NASCDZ', 'LGNASC_Slope_Rug_DZ_NASCSlope_NASCDZ', 'LGNASC_Slope_DZ_NASCSlope_NASCDZ',
                 'LGNASC_Slope_NASCSlope', 'LGNASC')

AIC_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_results)

####Do not proceed with model validation because deviance explained is too low. 

###model validation 5m RFZ model

par(mfrow = c(2, 2))
plot(MANNASC_Slope_NASCSlope)

check_overdispersion(MANNASC_Slope_NASCSlope)


#####calculate residuals and add to dataframe
residuals <- residuals(MANNASC_Slope_NASCSlope)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

E1<-ggplot(TF2) +
  geom_point(aes(x = Average_5m_slope,
                 y = resid)) +
  geom_smooth(aes(x = Average_5m_slope,
                  y = resid),
              method = "lm")

E2<-ggplot(TF2) +
  geom_point(aes(x = Std_Dev_Slope,
                 y = resid)) +
  geom_smooth(aes(x = Std_Dev_Slope,
                  y = resid),
              method = "lm")

E3<-ggplot(TF2) +
  geom_point(aes(x = Cumulative_LG_DZ_Area,
                 y = resid)) +
  geom_smooth(aes(x = Cumulative_LG_DZ_Area,
                  y = resid),
              method = "lm")

E4<-ggplot(TF2) +
  geom_point(aes(x = NASC_10_MAN,
                 y = resid)) +
  geom_smooth(aes(x = NASC_10_MAN,
                  y = resid),
              method = "lm")

E5<-ggplot(TF2) +
  geom_point(aes(x = Average_Depth,
                 y = resid)) +
  geom_smooth(aes(x = Average_Depth,
                  y = resid),
              method = "lm")

resVpred<-grid.arrange(E1, E2, E3, E4, E5, nrow = 2, respect=TRUE)

#check model fit with DHARMa tests
r <- simulateResiduals(MANNASC_Slope_NASCSlope, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(MANNASC_Slope_NASCSlope)

#################################################################################
#########Transect 1 (Pass 1) vs. Transect 3 (Pass 3) (10m RFZ NASC sign. difference tests)#########

load("wdata/full_df_t3.RData")

T3_10m<-full_df_t3%>%
  dplyr::select(Site_ID, NASC_10_t3)

T1_10m<-sitefull%>%
  dplyr::select(Site_ID, NASC_10_1m)

T1_T3_NASC<- left_join(T1_10m, T3_10m, by = "Site_ID")


### t test to check for significant difference between T1 and T3
t_test_T1_T3 <- t.test(T1_T3_NASC$NASC_10_1m, T1_T3_NASC$NASC_10_t3, paired = TRUE)
t_test_T1_T3 #not significantly different t = -1.707, df = 38, p-value = 0.09599

##check pearson correlation
pearson <- cor(T1_T3_NASC$NASC_10_1m, T1_T3_NASC$NASC_10_t3, method = "pearson")
pearson


##plot correlation
Cor_T1T3 <- ggplot(T1_T3_NASC, aes(x = NASC_10_1m, y = NASC_10_t3)) +
  geom_point(color = "gray40") +
 # labs(title = "b)") +
  sm_statCorr(corr_method="pearson", color = "gray40")+
  labs(x = "Pass 1 NASC", y = "Pass 3 NASC")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Cor_T1T3

ggsave("figures/T1_T3_pearsonscor_plot.png", plot = Cor_T1T3, width = 25, height = 25, units = "cm")


## box plot for NASC at 10m for each DEADZONE
T1_T3 <- T1_T3_NASC %>%
  rename(
    "Pass 1" = NASC_10_1m,
    "Pass 3" = NASC_10_t3
  ) %>%
  pivot_longer(cols = c(`Pass 1`, `Pass 3`),
               names_to = "Method",
               values_to = "NASC")

# Define the desired order of Method levels
method_order <- c("Pass 1", "Pass 3")  # Replace with your actual method names and desired order

# Convert Method to a factor with specified levels and order
T1_T3$Method <- factor(T1_T3$Method, levels = method_order)
str(T1_T3)


### if we want to include this plot will need to go back and make sure cleaned data is what is loaded in here (e.g. are herring schools removed from LG and Manual
# deadzone files?)
T1_T3_NASC_plot<- ggplot(T1_T3, aes(x = Method, y = NASC )) +
  geom_boxplot(color = "gray40") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_bw()+
  #labs(title = "a)") +
  #geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
#+geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
T1_T3_NASC_plot

#T1T3_plots_all<-grid.arrange(T1_T3_NASC_plot, Cor_T1T3,  ncol = 2, respect=TRUE)
ggsave("figures/T1_T3_NASC_boxplot.png", plot = T1_T3_NASC_plot, width = 25, height = 25, units = "cm")


