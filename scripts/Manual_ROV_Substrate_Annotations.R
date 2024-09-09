# By: Darienne Lancaster
# Date: July 2024

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

# Apply the function (add bin ID numbers to each substrate annotation based on time along transect)
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


### attach fish and echosounder data to ROV substrate annotation data###

load("wdata/site_DE.RData")

#rename site to match site_ID
#ROV_Sub_Site<-ROV_Sub_Site%>%
#  rename(Site_ID=Site)


#join dataframes together
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

lp("reporter")
lp("magrittr")

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
 


#####site level comparison of habitat variables####
####prelim plots and tests####
#test relationship between rugosity values
rug1<-ggplot(sitefull_s, aes(x=Rugosity_s, y=Std_Dev_Slope_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
rug1

correlation_coefficient <- cor(sitefull_s$Rugosity_s, sitefull_s$Std_Dev_Slope_s)
correlation_coefficient

rug2<-ggplot(sitefull_s, aes(x=Rugosity_s, y=Ratio_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
rug2

correlation_coefficient <- cor(sitefull_s$Rugosity_s, sitefull_s$Ratio_s)
correlation_coefficient

#test relationship between slope values
slo<-ggplot(sitefull_s, aes(x=Slope_s, y=Average_5m_slope_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
slo

correlation_coefficient <- cor(sitefull_s$Slope_s, sitefull_s$Average_5m_slope_s)
correlation_coefficient


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
  sm_statCorr(corr_method="spearman", color = "deepskyblue4")+
  labs(x = "ROV", y = "SD Slope")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugSTDROV  

rugRatioROV <- ggplot(sitefull_s, aes(x = Rugosity_s, y = Ratio_s)) +
  geom_point(color = "deepskyblue") +
  labs(title = "b)") +
  sm_statCorr(corr_method="spearman", color = "deepskyblue4")+
  labs(x = "ROV", y = "Ratio")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugRatioROV  

rugRatioSTD <- ggplot(sitefull_s, aes(x = Ratio_s, y = Std_Dev_Slope_s)) +
  geom_point(color = "deepskyblue") +
  labs(title = "c)") +
  sm_statCorr(corr_method="spearman", color = "deepskyblue4")+
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
  sm_statCorr(corr_method="spearman", color = "orange3")+
  labs(x = "Rock (%)", y = "SD Slope")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugSTDrock

rugRATIOrock <- ggplot(sitefull_s, aes(x = Rock, y = Ratio_s)) +
  geom_point(color = "orange1") +
  labs(title = "f)") +
  sm_statCorr(corr_method="spearman", color = "orange3")+
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
  sm_statCorr(corr_method="spearman", color = "orange3")+
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
  sm_statCorr(corr_method="spearman", color = "indianred4")+
  labs(x = "ROV", y = "20m")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope20_s

slope5_s <- ggplot(sitefull_s, aes(x = Slope_s, y = Average_5m_slope_s)) +
  geom_point(color = "indianred2") +
  labs(title = "j)") +
  sm_statCorr(corr_method="spearman", color = "indianred4")+
  labs(x = "ROV", y = "5m")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope5_s

colors()

slope20_5_s <- ggplot(sitefull_s, aes(x = Average_5m_slope_s, y = Average_20m_slope_s)) +
  geom_point(color = "indianred2") +
  labs(title = "k)") +
  sm_statCorr(corr_method="spearman", color = "indianred4")+
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
####boxplots for NASC for each RFZ and each Deadzone type####

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


## box plot for NASC at 10m for each DEADZONE
sitefull_NASC_DZ <- sitefull %>%
  dplyr::select("Site_ID", "NASC_10_LG", "NASC_10_MAN", "NASC_10_1m")
str(sitefull_NASC_DZ)

sitefull_NASC_DZlong <- sitefull_NASC_DZ %>%
  rename(
    "Large Deadzone" = NASC_10_LG,
    "Manual Deadzone" = NASC_10_MAN,
    "1m Deadzone" = NASC_10_1m
  ) %>%
  pivot_longer(cols = c(`Large Deadzone`, `Manual Deadzone`,`1m Deadzone` ),
               names_to = "Method",
               values_to = "NASC")

# Define the desired order of Method levels
method_order <- c("Large Deadzone", "Manual Deadzone","1m Deadzone")  # Replace with your actual method names and desired order

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



####check relationship between slope and benthic fish abundance####
#decent linear relationship between NASC and benthic fish counts
BvS<-ggplot(sitefull, aes(x=FOVnVis, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(sitefull$FOVnVis, sitefull$AbundanceNonSchooling)
correlation_coefficient

BvS<-ggplot(sitefull, aes(x=Slope, y=Rock))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(sitefull$Slope, sitefull$Rock)
correlation_coefficient

#####binned data comparisons####
#check same relationships with binned data and then run ttests
load("wdata/bin_DE.RData")


#rename site to match site_ID
ROV_Sub_Bin<-ROV_Sub_Bin%>%
  rename(Site_ID=Site)
#rename interval to match Bin
bin_DE<- bin_DE%>%
  rename(Bin=Interval)

str(bin_DE)
str(ROV_Sub_Bin)
ROV_Sub_Bin$Bin<- as.character(ROV_Sub_Bin$Bin)


#join dataframes together
binfull<-left_join(ROV_Sub_Bin, bin_DE, by=c("Site_ID", "Bin"))

binfull<-binfull%>%
  ungroup()

#remove site NS05
binfull<-binfull%>%
  filter( Site_ID !="NS05")

binfull$Bin_ID <- paste0(binfull$Site_ID, "_", binfull_s$Bin)

#remove bin NS32_5 (strange outlier)
binfull<-binfull%>%
  filter( Bin_ID !="NS32_5")



#scale variables for comparison
str(binfull)
str(sitefull_s)

binfull_s<- binfull%>%
  mutate(Average_5m_slope_s=scale(Average_5m_slope),
         Std_Dev_Slope_s=scale(Std_Dev_Slope),
         Ratio_s=scale(Ratio),
         Cumulative_LG_DZ_Area_s=scale(Cumulative_LG_DZ_Area),
         NASC_10_1m_s=scale(NASC_10_1m), 
         Rugosity_s=scale(Rugosity),
         Slope_s=scale(Slope),
         Depth_s=scale(Depth),
         Average_Depth_s=scale(Average_Depth))%>%
  mutate_at(vars(47:54), as.numeric)
str(binfull_s)

#####Binned data relationships with habitat variables#####
#test relationship between rugosity values
rug1<-ggplot(binfull_s, aes(x=Rugosity_s, y=Std_Dev_Slope_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
rug1

correlation_coefficient <- cor(binfull_s$Rugosity_s, binfull_s$Std_Dev_Slope_s)
correlation_coefficient

rug2<-ggplot(binfull_s, aes(x=Rugosity_s, y=Ratio_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
rug2

correlation_coefficient <- cor(binfull_s$Rugosity_s, binfull_s$Ratio_s)
correlation_coefficient

#test relationship between slope values
slo<-ggplot(binfull_s, aes(x=Slope_s, y=Average_5m_slope_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
slo

correlation_coefficient <- cor(binfull_s$Slope_s, binfull_s$Average_5m_slope_s)
correlation_coefficient

#test relationship between rock percentage and Ratio


rugROVratio_bin_spear<-cor.test(binfull$Ratio, binfull$Rugosity,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugROVratio_bin_spear)
#rho = 0.30, p = 1.556e-05 (weak significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

rugROVstd_bin_spear<-cor.test(binfull$Std_Dev_Slope, binfull$Rugosity,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugROVstd_bin_spear)
#rho = 0.21, p = 0.003 (weak significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

rugRatio_std_bin_spear<-cor.test(binfull$Std_Dev_Slope, binfull$Ratio,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugRatio_std_bin_spear)
#rho = 0.81, p = 2.2e-16 (weak significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

Slope_bin_spear<-cor.test(binfull$Slope, binfull$Average_5m_slope,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(Slope_bin_spear)
#rho = 0.41, p = 3.123e-09 (moderate significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”
  

## box plot the data to make sense of the value distribution 


binfull_s_sub <- binfull_s %>%
  dplyr::select("Bin_ID", "Slope_s", "Average_5m_slope_s")

binfull_s_long <- binfull_s_sub %>%
  pivot_longer(cols = c(Slope_s, Average_5m_slope_s),
               names_to = "Method",
               values_to = "Slope")

ggplot(binfull_s_long, aes(x = Method, y = Slope )) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "ROV vs Echosounder Slope",
       y = "Slope Scaled")

#test binned rugosity from ROV and Echosounder for significant difference
#Stand dev vs RugosityROV
t_test_result <- t.test(binfull$Rugosity, binfull$Std_Dev_Slope, paired = TRUE)

print(t_test_result)

#Ratio vs RugosityROV
t_test_result <- t.test(binfull$Rugosity, binfull$Ratio, paired = TRUE)

print(t_test_result)


## box plot the data to make sense of the value distribution 
binfull_s_subR <- binfull_s %>%
  dplyr::select("Bin_ID", "Rugosity_s", "Std_Dev_Slope_s", "Ratio_s")

binfull_s_longR <- binfull_s_subR %>%
  pivot_longer(cols = c(Rugosity_s, Std_Dev_Slope_s, Ratio_s),
               names_to = "Method",
               values_to = "Rugosity")

ggplot(binfull_s_longR, aes(x = Method, y = Rugosity )) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "ROV vs Echosounder Rugosity",
       y = "Rugosity Scaled")

#test relationships between slope, rugosity, etc.. and benthic fish with binned data
BvS<-ggplot(binfull, aes(x=Std_Dev_Slope, y=Rock))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(binfull$Rock, binfull$Std_Dev_Slope)
correlation_coefficient

#ROV rock vs rugosity metric from Echosounder (Std_Dev_Slope)
#takeaway- higher Std_Dev_Slope indicates higher rock percentages on transects (rock is positively associated with benthic rockfish abundance)
BvS<-ggplot(sitefull, aes(x=FOVnVis, y=Volume))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(sitefull$FOVnVis, sitefull$Volume)
correlation_coefficient

#############################################################

##neg binomial count models have similar results to density models but they have more issues with the residuals (stick with density models)
#### run neg binom glm on site data from ROV####
str(sitefull)

sitefull$LFOVnVis<-log(sitefull$FOVnVis)
sitefull$LVol<-log(sitefull$Volume)

#model with only variables also included in echosounder model
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LVol) + Rugosity + Depth+ Slope, #remove
             data = sitefull)
summary(M1)
AIC(M1) #265 (22% Dev Exp)

M1 <- glm.nb(AbundanceNonSchooling ~ offset(LVol) + Rugosity + Slope, #remove
             data = sitefull)
summary(M1)
AIC(M1) #263 (21% Dev Exp)

M1 <- glm.nb(AbundanceNonSchooling ~ offset(LVol) + Rugosity + Slope +  NASC_10_1m, #remove
             data = sitefull)
summary(M1)
AIC(M1) #260 (30% Dev Exp)

#model with rock (explains more variance - better model)
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LVol) + Rugosity + Slope +Rock, #remove
             data = sitefull)
summary(M1)
AIC(M1) #257 (34% Dev Exp)

#model with rock (explains more variance - better model)
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LVol) + Rugosity + Slope +Rock +  NASC_10_1m, #remove
             data = sitefull)
summary(M1)
AIC(M1) #257 (39% Dev Exp)



#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(M1)

check_overdispersion(M1)


#####calculate residuals and add to dataframe
residuals <- residuals(M1)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

ggplot(TF2) +
  geom_point(aes(x = Slope,
                 y = resid)) +
  geom_smooth(aes(x = Slope,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Rugosity,
                 y = resid)) +
  geom_smooth(aes(x = Rugosity,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Depth,
                 y = resid)) +
  geom_smooth(aes(x = Depth,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Rock,
                 y = resid)) +
  geom_smooth(aes(x = Rock,
                  y = resid),
              method = "lm")

#check model fit with DHARMa tests
r <- simulateResiduals(M1, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(M1)

### compare ROV model to Echosounder model

#echosounder model

BvS<-ggplot(sitefull, aes(x=Average_5m_slope, y=NASC_10_1m))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS

#model copying ROV model (no NASC) - nothing significant
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) + Average_Depth+
                Std_Dev_Slope+ Average_5m_slope , #remove
             data = sitefull)

#full echosounder model
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) +NASC_10_1m + Average_Depth+
               Cumulative_LG_DZ_Area + Std_Dev_Slope+ Average_5m_slope+
               NASC_10_1m:Average_5m_slope , #remove
             data = sitefull)
summary(M1)
AIC(M1) #255

M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) +NASC_10_1m + Average_Depth+
               Cumulative_LG_DZ_Area + Std_Dev_Slope+ Average_5m_slope+
               NASC_10_1m:I(Average_5m_slope^2) , #remove
             data = sitefull)
summary(M1)
AIC(M1) #254

#remove least sign term
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) +NASC_10_1m + Average_Depth+
                Std_Dev_Slope+ Average_5m_slope+
               NASC_10_1m:I(Average_5m_slope^2) , #remove
             data = sitefull)
summary(M1)
AIC(M1) #252


#BEST MODEL - remove least sign term (35% dev explained) NOTE: model explains more deviance with rock included but not echosounder variable 
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) +NASC_10_1m + Average_Depth+
               Average_5m_slope +
               NASC_10_1m:I(Average_5m_slope^2) , #remove
             data = sitefull)
summary(M1)
AIC(M1) #250

#remove least sign term (only drops by 1 AIC point and explained deviance drops so keep model before with Slope included)
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) +NASC_10_1m + Average_Depth+
               NASC_10_1m:I(Average_5m_slope^2) , #remove
             data = sitefull)
summary(M1)
AIC(M1) #249


#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(M1)

check_overdispersion(M1)


#####calculate residuals and add to dataframe
residuals <- residuals(M1)
TF2 <- TF %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

ggplot(TF2) +
  geom_point(aes(x = Average_5m_slope,
                 y = resid)) +
  geom_smooth(aes(x = Average_5m_slope,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Std_Dev_Slope,
                 y = resid)) +
  geom_smooth(aes(x = Std_Dev_Slope,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Cumulative_LG_DZ_Area,
                 y = resid)) +
  geom_smooth(aes(x = Cumulative_LG_DZ_Area,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = NASC_10_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_10_1m,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Average_Depth,
                 y = resid)) +
  geom_smooth(aes(x = Average_Depth,
                  y = resid),
              method = "lm")

#check model fit with DHARMa tests
r <- simulateResiduals(M1, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(M1)
  
###############################################################
####try density gaussian glm####

#convert benthic count to density
sitefull$Ben<-sitefull$AbundanceNonSchooling/sitefull$Volume

#Echo slope vs fish density with quadratic line
BvS<-ggplot(sitefull, aes(x=Average_5m_slope, y=Ben))+
  geom_point()+
  geom_smooth(method="lm", formula = y ~ poly(x, 2, raw = TRUE),se=FALSE) + #formula = y ~ poly(x, 2, raw = TRUE) makes line a quadratic
labs(x = "Slope", y = "Benthic Fish Density")
#  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS

BvS<-ggplot(sitefull, aes(x=Average_5m_slope, y=Ben))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE) #formula = y ~ poly(x, 2, raw = TRUE) makes line a quadratic
  #geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS

#slope with echoslope and fish count
ggplot(sitefull, aes(x = NASC_10_1m, y = Average_5m_slope, color = Ben)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +# You can adjust colors as per your preference
  geom_smooth(method="lm")+
  labs(x = "NASC", y = "SlopeEcho", color = "AbundanceNonSchooling")+
  #geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)

  par(mfrow = c(1, 1))
#slope with echoslope and fish count
ggplot(sitefull, aes(x = NASC_10_1m, y = Ben, color = Average_5m_slope)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +# You can adjust colors as per your preference
  geom_smooth(method="lm")+
  labs(x = "NASC", y = "Benthic Fish Density", color = "Slope")
  #geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)



#####Some random tests ####

# L1<-lm(Ben~Average_5m_slope + I(Average_5m_slope^2), data = sitefull)
# summary(L1)
# 
# L2<-lm(Ben~Average_5m_slope , data = sitefull)
# summary(L2)
# 
# L3<-lm(Ben~ I(Average_5m_slope^2), data = sitefull)
# summary(L3)
# 
# L4 <- glm(Ben ~ NASC_10_1m + Average_5m_slope + I(Average_5m_slope^2) + 
#             NASC_10_1m:Average_5m_slope , 
#           family = gaussian,  data = sitefull)
# summary(L4)
# 
# 
# L4 <- glm(Ben ~ NASC_10_1m + Average_5m_slope + I(Average_5m_slope^2) + 
#             NASC_10_1m:I(Average_5m_slope^2) , 
#           family = gaussian,  data = sitefull)
# summary(L4)
# 
# L4 <- glm(Ben ~ NASC_10_1m + I(Average_5m_slope^2) + 
#             NASC_10_1m:I(Average_5m_slope^2) , 
#           family = gaussian,  data = sitefull)
# summary(L4)
# 
# L4 <- glm(Ben ~ NASC_10_1m + Average_5m_slope + 
#             NASC_10_1m:Average_5m_slope , 
#           family = gaussian,  data = sitefull)
# summary(L4)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((L4)$null.deviance-(L4)$deviance)/(L4)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# 
# # Install and load the visreg package
# install.packages("visreg")
# library(visreg)
# 
# ###plot model vs observed values for ben and NASC
# par(mfrow = c(1, 1))
# # Create visreg plot
# vis <- visreg(L4, "NASC_10_1m", scale = "response", main = "Effect Plot of NASC_10_1m on Ben")
# 
# # Extract predicted values from the visreg object
# predicted <- vis$fit
# 
# # Plot observed points
# points(sitefull$NASC_10_1m, sitefull$Ben, col = "blue", pch = 16)
# 
# ###plot model vs observed values for ben and slope
# par(mfrow = c(1, 1))
# # Create visreg plot
# vis <- visreg(L4, "NASC_10_1m:Average_5m_slope", scale = "response", main = "Effect Plot of Slope on Ben")
# 
# # Extract predicted values from the visreg object
# predicted <- vis$fit
# 
# # Plot observed points
# points(sitefull$Average_5m_slope, sitefull$Ben, col = "blue", pch = 16)
# 
# par(mfrow = c(2, 2))
# plot(L4)
# 
# check_overdispersion(D1)
# 
# 
# #####calculate residuals and add to dataframe
# residuals <- residuals(L4)
# TF2 <- sitefull %>%
#   mutate(resid = residuals)
# 
# ## plot residuals vs predictors (looking for flat line with lm)
# 
# ggplot(TF2) +
#   geom_point(aes(x = Average_5m_slope,
#                  y = resid)) +
#   geom_smooth(aes(x = Average_5m_slope,
#                   y = resid),
#               method = "lm")
# 
# ggplot(TF2) +
#   geom_point(aes(x = Std_Dev_Slope,
#                  y = resid)) +
#   geom_smooth(aes(x = Std_Dev_Slope,
#                   y = resid),
#               method = "lm")
# 
# ggplot(TF2) +
#   geom_point(aes(x = Cumulative_LG_DZ_Area,
#                  y = resid)) +
#   geom_smooth(aes(x = Cumulative_LG_DZ_Area,
#                   y = resid),
#               method = "lm")
# 
# ggplot(TF2) +
#   geom_point(aes(x = NASC_10_1m,
#                  y = resid)) +
#   geom_smooth(aes(x = NASC_10_1m,
#                   y = resid),
#               method = "lm")
# 
# ggplot(TF2) +
#   geom_point(aes(x = Average_Depth,
#                  y = resid)) +
#   geom_smooth(aes(x = Average_Depth,
#                   y = resid),
#               method = "lm")
# 
# #check model fit with DHARMa tests
# r <- simulateResiduals(L4, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
# #check dispersion
# testDispersion(D1)


###########################################
###echosounder density model#####

#thoughts:
#interaction between NASC and slope (NASC has a positive relationships with fish density, NASC has a positive relationship with slope, 
#Slope has slightly parabolic relationship with fish density- increase then decrease)

#deadzone and nasc positive relationship, but deadzone and fish density negative relationship

#
#Full model with all vars and quadratic for slope
###this model has best residual plots and lowest AIC w 54% dev explained
D1 <- glm(Ben ~ NASC_10_1m + I(Average_5m_slope^2) + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_10_1m:I(Average_5m_slope^2) + NASC_10_1m:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-215 (54% exp dev)

#Full model with all vars and no quadratic for slope
D1 <- glm(Ben ~ NASC_10_1m + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_10_1m:Average_5m_slope + NASC_10_1m:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-215 (55% exp dev)



##########################################
#10m model
####AIC selection on full echo model 10m RFZ WITH quadratic####
#steps - remove Rugosity (AIC 217), NASC_10_1m:Cumulative_LG_DZ_Area (AIC 218), Average_Depth (AIC 219 - 52%), 
#Cumulative_LG_DZ_Area (AIC 220, 52%)

#best model based on AIC and keeping all terms included in interaction (slope not significant but must be retained)
D1 <- glm(Ben ~ NASC_10_1m + I(Average_5m_slope^2) +
            NASC_10_1m:I(Average_5m_slope^2), 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-220 (52% exp dev)

##############################################
##AIC selection on full model WITHOUT quadratic##
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

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((NoRug_NASCDZ_Dep)$null.deviance-(NoRug_NASCDZ_Dep)$deviance)/(NoRug_NASCDZ_Dep)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

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

lp("AICcmodavg")

###put all models in a list to compare AIC weights
models <- list(NASC_Slope_Rugosity_Depth_DZ_DZNASC_NASCSLOPE, NASC_Slope_Depth_DZ_DZNASC_NASCSLOPE, NASC_Slope_Depth_DZ_NASCSLOPE, NASC_Slope_DZ_NASCSLOPE, NASC_Slope_NASCSLOPE, NASC)

model.names <- c('NASC_Slope_Rugosity_Depth_DZ_DZNASC_NASCSLOPE', 'NASC_Slope_Depth_DZ_DZNASC_NASCSLOPE', 'NASC_Slope_Depth_DZ_NASCSLOPE', 'NASC_Slope_DZ_NASCSLOPE', 'NASC_Slope_NASCSLOPE', 'NASC')

AIC_results<-aictab(cand.set = models, modnames = model.names)
flextable(AIC_results)


####*FINAL 10m ECHO MODEL* - based on AIC####
####Do not include slope as quadratic as it does not significantly improve model from slope as simple linear term - they are almost identical####
D1 <- glm(Ben ~ NASC_10_1m + Average_5m_slope +
            NASC_10_1m:Average_5m_slope, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-220 (52% exp dev)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((D1)$null.deviance-(D1)$deviance)/(D1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance


par(mfrow = c(2, 2))
plot(D1)

check_overdispersion(D1)


#####calculate residuals and add to dataframe
residuals <- residuals(D1)
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
r <- simulateResiduals(D1, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(D1)


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

###################################################################
####Echo model using only ROV available habitat variables####
#model copying variables from ROV (no significant predictors and explains 7% of deviation only (ROV model explains 18% - still low))
Slope_Depth_Rugosity <- glm(Ben ~  Average_5m_slope + Std_Dev_Slope + Average_Depth, 
          family = gaussian,  data = sitefull)
summary(Slope_Depth_Rugosity)
AIC(Slope_Depth_Rugosity) #-193 (4% Dev exp)

Slope_Depth_Rugosity<- glm(Ben ~  Average_5m_slope  + Average_Depth, 
          family = gaussian,  data = sitefull)
summary(Slope_Depth_Rugosity)
AIC(Slope_Depth_Rugosity) #-195 (4% Dev exp)

Slope <- glm(Ben ~  Average_5m_slope, 
          family = gaussian,  data = sitefull)
summary(Slope)
AIC(Slope) #-197 (4% Dev exp)


##THOUGHTS:  when sampling in pre selected rockfish habitat echosounder derived habitat variables
#are unable to predict benthic rockfish densities without the addition of NASC values. It is likely that if an 
#echosounder survey was performed across a wider array of habitats without preselecting for rocky reef areas
# echosounder derived habitat variables would be able to help predict areas of high rockfish density. 

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((D1)$null.deviance-(D1)$deviance)/(D1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(D1)

check_overdispersion(D1)


#####calculate residuals and add to dataframe
residuals <- residuals(D1)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

ggplot(TF2) +
  geom_point(aes(x = Average_5m_slope,
                 y = resid)) +
  geom_smooth(aes(x = Average_5m_slope,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Std_Dev_Slope,
                 y = resid)) +
  geom_smooth(aes(x = Std_Dev_Slope,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Cumulative_LG_DZ_Area,
                 y = resid)) +
  geom_smooth(aes(x = Cumulative_LG_DZ_Area,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = NASC_10_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_10_1m,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Average_Depth,
                 y = resid)) +
  geom_smooth(aes(x = Average_Depth,
                  y = resid),
              method = "lm")

#check model fit with DHARMa tests
r <- simulateResiduals(D1, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(D1)

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
#check dispersion
testDispersion(D1)



#BEST MODEL (best residuals and simplest - no major diff in AIC) - AIC selection (remove depth - AIC -221 (58%), remove ROck AIC - 220 (54%))  
D1 <- glm(Ben ~  Slope + Rugosity +NASC_10_1m +
            NASC_10_1m:Slope,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-220 (54% Dev Exp)

#echosounder slope model hybrid model (best model - 61% dev exp)
D1 <- glm(Ben ~  Average_5m_slope + Rugosity + Depth + Rock +NASC_10_1m +
            NASC_10_1m:Average_5m_slope,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-223 (61% Dev Exp)

D1 <- glm(Ben ~  Average_5m_slope + Rugosity + Rock +NASC_10_1m +
            NASC_10_1m:Average_5m_slope,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-224 (61% Dev Exp)


#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((D1)$null.deviance-(D1)$deviance)/(D1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(D1)

check_overdispersion(D1)


#####calculate residuals and add to dataframe
residuals <- residuals(D1)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

ggplot(TF2) +
  geom_point(aes(x = Slope,
                 y = resid)) +
  geom_smooth(aes(x = Slope,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Rugosity,
                 y = resid)) +
  geom_smooth(aes(x = Rugosity,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Depth,
                 y = resid)) +
  geom_smooth(aes(x = Depth,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = NASC_10_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_10_1m,
                  y = resid),
              method = "lm")



#check model fit with DHARMa tests
r <- simulateResiduals(D1, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(D1)



#######################################################
####different deadzone in echo models####

str(sitefull)
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

###10m LG DZ##### - not significant with any vars removed
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

########################


#AIC selection
D1 <- glm(Ben ~ NASC_10_MAN + Average_5m_slope + Std_Dev_Slope+ Cumulative_LG_DZ_Area +
            NASC_10_MAN:Average_5m_slope + NASC_10_MAN:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-212 (49% exp dev)


D1 <- glm(Ben ~ NASC_10_MAN + Average_5m_slope+ Cumulative_LG_DZ_Area +
            NASC_10_MAN:Average_5m_slope + NASC_10_MAN:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-212 (46% exp dev)


### best model - 40% dev explained (AIC 212 - same as all others)
D1 <- glm(Ben ~ NASC_10_MAN + Average_5m_slope +
            NASC_10_MAN:Average_5m_slope, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-212 (40% exp dev)


####10m LG DZ### 
D1 <- glm(Ben ~ NASC_10_LG + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_10_LG:Average_5m_slope + NASC_10_LG:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-189 (13% exp dev)

D1 <- glm(Ben ~ NASC_10_LG , 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-189 (13% exp dev)


#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((D1)$null.deviance-(D1)$deviance)/(D1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance


par(mfrow = c(2, 2))
plot(D1)

check_overdispersion(D1)


#####calculate residuals and add to dataframe
residuals <- residuals(D1)
TF2 <- sitefull %>%
  mutate(resid = residuals)

## plot residuals vs predictors (looking for flat line with lm)

ggplot(TF2) +
  geom_point(aes(x = Average_5m_slope,
                 y = resid)) +
  geom_smooth(aes(x = Average_5m_slope,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Std_Dev_Slope,
                 y = resid)) +
  geom_smooth(aes(x = Std_Dev_Slope,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Cumulative_LG_DZ_Area,
                 y = resid)) +
  geom_smooth(aes(x = Cumulative_LG_DZ_Area,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = NASC_10_MAN,
                 y = resid)) +
  geom_smooth(aes(x = NASC_10_MAN,
                  y = resid),
              method = "lm")

ggplot(TF2) +
  geom_point(aes(x = Average_Depth,
                 y = resid)) +
  geom_smooth(aes(x = Average_Depth,
                  y = resid),
              method = "lm")

#check model fit with DHARMa tests
r <- simulateResiduals(D1, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(D1)

#########################################################
####density vs predictor variable plots#####

#density vs ROV rugosity
ROV_DR <- ggplot(sitefull, aes(x = Ben, y = Rugosity)) +
  geom_point(color = "gray40") +
  labs(x = "ROV Rugosity", y = "Benthic Fish Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_smooth(method = "lm", se = TRUE, color = "black")
ROV_DR 

#density vs Echo rugosity
Echo_DR_SD <- ggplot(sitefull, aes(x = Ben, y = Std_Dev_Slope)) +
  geom_point(color = "gray40") +
  labs(x = "Echogram Rugosity (SD Slope)", y = "Benthic Fish Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_smooth(method = "lm", se = TRUE, color = "black")
Echo_DR_SD



###put all rugosity plots together
allrug<-grid.arrange(rugSTDROV, rugRatioROV, rugRatioSTD, rug_box, ncol = 2, respect=TRUE)

ggsave("figures/ROV_Echo_Rugosity_spearmancor.png", plot = allrug, width = 25, height = 25, units = "cm")

####deadzone vs rugosity plots####

#density vs Echo rugosity
DZ_RUG <- ggplot(sitefull, aes(x = Rugosity, y = Cumulative_LG_DZ_Area)) +
  geom_point(color = "gray40") +
  labs(x = "Rugosity (ROV)", y = "Deadzone Area")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  sm_statCorr(corr_method="spearman", color = "black")+
  geom_smooth(method = "lm", se = TRUE, color = "black")
DZ_RUG

DZ_RUG <- ggplot(sitefull, aes(x = Slope, y = Cumulative_LG_DZ_Area)) +
  geom_point(color = "gray40") +
  labs(x = "Slope (ROV)", y = "Deadzone Area")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  sm_statCorr(corr_method="spearman", color = "black")+
  geom_smooth(method = "lm", se = TRUE, color = "black")
DZ_RUG

DZ_RUG <- ggplot(sitefull, aes(x = Slope, y = Rugosity)) +
  geom_point(color = "gray40") +
  labs(x = "Slope (ROV)", y = "Rugosity")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  sm_statCorr(corr_method="spearman", color = "black")+
  geom_smooth(method = "lm", se = TRUE, color = "black")
DZ_RUG




#########Transect 1 vs. Transect 3 (10m RFZ NASC sign. difference tests)#########

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
  labs(x = "Transect 1 NASC", y = "Transect 3 NASC")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Cor_T1T3

ggsave("figures/T1_T3_pearsonscor_plot.png", plot = Cor_T1T3, width = 25, height = 25, units = "cm")


## box plot for NASC at 10m for each DEADZONE
T1_T3 <- T1_T3_NASC %>%
  rename(
    "Transect 1" = NASC_10_1m,
    "Transect 3" = NASC_10_t3
  ) %>%
  pivot_longer(cols = c(`Transect 1`, `Transect 3`),
               names_to = "Method",
               values_to = "NASC")

# Define the desired order of Method levels
method_order <- c("Transect 1", "Transect 3")  # Replace with your actual method names and desired order

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


