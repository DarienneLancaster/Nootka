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
install.packages("GGally")
library(GGally)
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
str(trandur1)
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
ROV_Sub_Site<-ROV_Sub_Site%>%
  rename(Site_ID=Site)


#join dataframes together
sitefull<-left_join(ROV_Sub_Site, site_DE, by="Site_ID")

sitefull<-sitefull%>%
  ungroup()

#remove site NS05
sitefull<-sitefull%>%
   filter( Site_ID !="NS05")

#scale variables for comparison
str(sitefull)
str(sitefull_s)

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
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Rugosity", y = "Echosounder Rugosity (SD Slope)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugSTDROV  

rugRatioROV <- ggplot(sitefull_s, aes(x = Rugosity_s, y = Ratio_s)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Rugosity", y = "Echosounder Rugosity (Chain Length Ratio)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugRatioROV  

rugRatioSTD <- ggplot(sitefull_s, aes(x = Ratio_s, y = Std_Dev_Slope_s)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "Echosounder Rugosity (Chain Length Ratio)", y = "Echosounder Rugosity (SD Slope)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugRatioSTD

## box plot of RUGOSITY data 
sitefull_s_subR <- sitefull_s %>%
  dplyr::select("Site_ID", "Rugosity_s", "Std_Dev_Slope_s", "Ratio_s")

sitefull_s_longR <- sitefull_s_subR %>%
  rename(
    "ROV" = Rugosity_s,
    "Echosounder (SD Slope)" = Std_Dev_Slope_s,
    "Echosounder (CL Ratio)" = Ratio_s
  ) %>%
  pivot_longer(cols = c(ROV, `Echosounder (SD Slope)`, `Echosounder (CL Ratio)`),
               names_to = "Method",
               values_to = "Rugosity")

rug_box<-ggplot(sitefull_s_longR, aes(x = Method, y = Rugosity )) +
  geom_boxplot(color = "gray40") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
rug_box

###put all rugosity plots together
allrug<-grid.arrange(rugSTDROV, rugRatioROV, rugRatioSTD, rug_box, ncol = 2, respect=TRUE)

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

####plot rock vs rugosity metrics
rugSTDrock <- ggplot(sitefull_s, aes(x = Rock, y = Std_Dev_Slope_s)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Rock (%)", y = "Echosounder Rugosity (SD Slope)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugSTDrock

rugRATIOrock <- ggplot(sitefull_s, aes(x = Rock, y = Ratio_s)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Rock (%)", y = "Echosounder Rugosity (CL Ratio)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rugRATIOrock

## box plot the ROCK data to make sense of the value distribution 
sitefull_s_subRock <- sitefull_s %>%
  dplyr::select("Site_ID", "Rock_s", "Std_Dev_Slope_s", "Ratio_s")

sitefull_s_longRock <- sitefull_s_subRock %>%
  rename(
    "ROV" = Rock_s,
    "Echosounder (SD Slope)" = Std_Dev_Slope_s,
    "Echosounder (CL Ratio)" = Ratio_s
  ) %>%
  pivot_longer(cols = c(ROV, `Echosounder (SD Slope)`, `Echosounder (CL Ratio)`),
               names_to = "Method",
               values_to = "Rock")

rock_box<-ggplot(sitefull_s_longRock, aes(x = Method, y = Rock )) +
  geom_boxplot(color = "gray40") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
rock_box

allrock<-grid.arrange(rugRATIOrock, rugSTDrock, rock_box, ncol = 2, respect=TRUE)
allrock
ggsave("figures/ROV_Echo_Rock_spearmancor.png", plot = allrock, width = 25, height = 25, units = "cm")

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

slope5_s <- ggplot(sitefull_s, aes(x = Slope_s, y = Average_5m_slope_s)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Slope scaled", y = "Echosounder Slope scaled (5m Average)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope5_s



slope20_s <- ggplot(sitefull_s, aes(x = Slope_s, y = Average_20m_slope_s)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Slope scaled", y = "Echosounder Slope scaled (20m Average)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
slope20_s



## box plot the SLOPE data to make sense of the value distribution 
sitefull_s_sub <- sitefull_s %>%
  dplyr::select("Site_ID", "Slope_s", "Average_5m_slope_s", "Average_20m_slope_s")
str(sitefull_s_sub)

sitefull_s_long <- sitefull_s_sub %>%
  rename(
    "ROV" = Slope_s,
    "Echosounder (5m)" = Average_5m_slope_s,
    "Echosounder (20m)" = Average_20m_slope_s
  ) %>%
  pivot_longer(cols = c(ROV, `Echosounder (5m)`,`Echosounder (20m)` ),
               names_to = "Method",
               values_to = "Slope")

slopebox_s<- ggplot(sitefull_s_long, aes(x = Method, y = Slope )) +
  geom_boxplot(color = "gray40") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black"),  axis.text.y = element_text(color = "black"),)
slopebox_s

allslope_s<-grid.arrange(slope5_s, slope20_s, slopebox_s,  ncol = 2, respect=TRUE)
ggsave("figures/ROV_Echo_Slope_scaled_spearmancor.png", plot = allslope_s, width = 25, height = 25, units = "cm")


#### try not scaled ###

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
  mutate_at(vars(47:55), as.numeric)
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
  geom_smooth(method="lm", formula = y ~ poly(x, 2, raw = TRUE),se=FALSE)+ #formula = y ~ poly(x, 2, raw = TRUE) makes line a quadratic
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS

#slope with echoslope and fish count
ggplot(sitefull, aes(x = Slope, y = Average_5m_slope, color = Ben)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +  # You can adjust colors as per your preference
  labs(x = "SlopeROV", y = "SlopeEcho", color = "AbundanceNonSchooling")+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)

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


#######################################
#5m model
#### Echo model with 5m RFZ NASC######

#Full model with all vars and no quadratic for slope (problems with residuals)
D1 <- glm(Ben ~ NASC_5_1m + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_5_1m:Average_5m_slope + NASC_5_1m:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-205 (42% exp dev)

####*FINAL 5m ECHO MODEL* - based on AIC####
#### AIC selection steps: remove Std_Dev_Slope  - AIC 207, 42%),  Average_Depth (AIC 208, 40%), Cumulative_LG_DZ_Area (AIC 207, 37%),
# NASC_5_1m:Cumulative_LG_DZ_Area(AIC 209, 36%)
####Do not include slope as quadratic as it does not significantly improve model from slope as simple linear term - they are almost identical####
D1 <- glm(Ben ~ NASC_5_1m + Average_5m_slope +  
            NASC_5_1m:Average_5m_slope, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-209 (36% exp dev)

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
  geom_point(aes(x = NASC_5_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_5_1m,
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


#######################################################
#15m model
#### Echo model with 15m RFZ NASC######

#Full model with all vars and no quadratic for slope 
D1 <- glm(Ben ~ NASC_15_1m + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_15_1m:Average_5m_slope + NASC_15_1m:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-202 (38% exp dev)

####*FINAL 15m ECHO MODEL* - based on AIC####

#### AIC selection steps: remove Std_Dev_Slope  - AIC 204, 38%),  NASC_5_1m:Cumulative_LG_DZ_Area (AIC 205, 36%), Cumulative_LG_DZ_Area (AIC 205, 32%),
#Average_Depth (AIC 206, 30%)
####Do not include slope as quadratic as it does not significantly improve model from slope as simple linear term - they are almost identical####
D1 <- glm(Ben ~ NASC_15_1m + Average_5m_slope + 
            NASC_15_1m:Average_5m_slope , 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-206 (30% exp dev)

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
  geom_point(aes(x = NASC_15_1m,
                 y = resid)) +
  geom_smooth(aes(x = NASC_15_1m,
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


###################################################################
####Echo model using only ROV available habitat variables####
#model copying variables from ROV (no significant predictors and explains 7% of deviation only (ROV model explains 18% - still low))
D1 <- glm(Ben ~  Average_5m_slope + Std_Dev_Slope + Average_Depth, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-193 (4% Dev exp)

D1 <- glm(Ben ~  Average_5m_slope  + Average_Depth, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-195 (4% Dev exp)

D1 <- glm(Ben ~  Average_5m_slope, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-197 (4% Dev exp)

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
D1 <- glm(Ben ~  Slope + Rugosity + Depth,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-199 (18% Dev Exp)

###AIC selection BEST MODEL (remove Depth)
D1 <- glm(Ben ~  Slope + Rugosity,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-201 (17% Dev Exp)




##### ROV model with rock included ####
D1 <- glm(Ben ~  Slope + Rugosity + Depth + Rock,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-202 (27% Dev Exp)

#AIC selection (remove depth)  BEST MODEL
D1 <- glm(Ben ~  Slope + Rugosity+ Rock,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-204 (27% Dev Exp)




##### ROV/ECHO hybrid model with rock and NASC included ####
## Full model
D1 <- glm(Ben ~  Slope + Rugosity + Depth + Rock +NASC_10_1m +
            NASC_10_1m:Slope,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-220 (59% Dev Exp)


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
D1 <- glm(Ben ~ NASC_10_MAN + Average_5m_slope + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_10_MAN:Average_5m_slope + NASC_10_MAN:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-212 (51% exp dev)

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


