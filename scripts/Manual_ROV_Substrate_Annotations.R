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

#load manual substrate annotations from ROV videos
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
         Std_Dev_Slope_s=scale(Std_Dev_Slope),
         Ratio_s=scale(Ratio),
         Cumulative_LG_DZ_Area_s=scale(Cumulative_LG_DZ_Area),
         NASC_10_1m_s=scale(NASC_10_1m), 
         Rugosity_s=scale(Rugosity),
         Slope_s=scale(Slope),
         Depth_s=scale(Depth),
         Average_Depth_s=scale(Average_Depth))%>%
  mutate_at(vars(55:63), as.numeric)

#####site level comparison of habitat variables####

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

#test rugosity from ROV and Echosounder for significant difference (Spearman rank correlation)
rugSTDROV_spear<-cor.test(sitefull$Rugosity, sitefull$Std_Dev_Slope,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugSTDROV_spear)
#rho = 0.38, p = 0.01 (weak significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”


# Create ggplot plot
rugSTDROV <- ggplot(sitefull, aes(x = Rugosity, y = Std_Dev_Slope)) +
  geom_point(color = "gray40") +
  sm_statCorr(corr_method="spearman", color = "black")+
  labs(x = "ROV Rugosity", y = "Echosounder Rugosity (StDevSlope)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
rugSTDROV  
  
  

rugRatioROV_spear<-cor.test(sitefull$Rugosity, sitefull$Ratio,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugRatioROV_spear)
#rho = 0.48, p = 0.002 (moderate significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

rugRatioSTD_spear<-cor.test(sitefull$Std_Dev_Slope, sitefull$Ratio,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugRatioSTD_spear)
#rho = 0.898, p = 8.08e-15 (very strong significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

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

#test relationship between rock percentage and Ratio
rugRATIOrock_spear<-cor.test(sitefull$Ratio, sitefull$Rock,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(rugRATIOrock_spear)
#rho = 0.55, p = 0.0006 (moderate significant positive relationship)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”


## box plot the data to make sense of the value distribution 
sitefull_s_subR <- sitefull_s %>%
  dplyr::select("Site_ID", "Rugosity_s", "Std_Dev_Slope_s", "Ratio_s")

sitefull_s_longR <- sitefull_s_subR %>%
  pivot_longer(cols = c(Rugosity_s, Std_Dev_Slope_s, Ratio_s),
               names_to = "Method",
               values_to = "Rugosity")

ggplot(sitefull_s_longR, aes(x = Method, y = Rugosity )) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "ROV vs Echosounder Rugosity",
       y = "Rugosity Scaled")

#test slope from ROV and Echosounder for significant difference (Spearman rank correlation)
slope_spear<-cor.test(sitefull$Slope, sitefull$Average_5m_slope,  method="spearman", exact= FALSE) #exact = FALSE gets rid of impact of tied values on ranking
print(slope_spear)
#interpret results - looking for rho value between - 1 and +1 with significant p value
# .00-.19 “very weak”
#  .20-.39 “weak”
#  .40-.59 “moderate”
#  .60-.79 “strong”
#  .80-1.0 “very strong”

## box plot the data to make sense of the value distribution 
sitefull_s_sub <- sitefull_s %>%
  dplyr::select("Site_ID", "Slope_s", "Average_5m_slope_s")

sitefull_s_long <- sitefull_s_sub %>%
  pivot_longer(cols = c(Slope_s, Average_5m_slope_s),
               names_to = "Method",
               values_to = "Slope")

ggplot(sitefull_s_long, aes(x = Method, y = Slope )) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "ROV vs Echosounder Slope",
       y = "Slope Scaled")

####check relationship between slope and benthic fish abundance
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
#### run neg binom glm on site data from ROV####
str(sitefull)

sitefull$LFOVnVis<-log(sitefull$FOVnVis)
sitefull$LVol<-log(sitefull$Volume)

#model with only variables also included in echosounder model
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) + Rugosity + Depth+ Slope, #remove
             data = sitefull)
summary(M1)
AIC(M1)

M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) + Rugosity +  NASC_10_1m, #remove
             data = sitefull)
summary(M1)
AIC(M1)

#model with rock (explains more variance - better model)
M1 <- glm.nb(AbundanceNonSchooling ~ offset(LFOVnVis) + Rugosity + Depth+ Slope +Rock, #remove
             data = sitefull)
summary(M1)




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

####try density gaussian glm####

#convert benthic count to density

sitefull$Ben<-sitefull$AbundanceNonSchooling/sitefull$Volume

BvS<-ggplot(sitefull, aes(x=NASC_10_1m, y=Ben))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS

BvS<-ggplot(sitefull, aes(x=Cumulative_LG_DZ_Area, y=Ben))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS

#rugosity and echorugosity with fish count
ggplot(sitefull_s, aes(x = Rugosity_s , y = Std_Dev_Slope_s, color = AbundanceNonSchooling)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +  # You can adjust colors as per your preference
  labs(x = "Rugosity", y = "Std_Dev_Slope", color = "AbundanceNonSchooling")+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)

#slope with echoslope and fish count
ggplot(sitefull_s, aes(x = Slope_s , y = Average_5m_slope_s, color = AbundanceNonSchooling)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +  # You can adjust colors as per your preference
  labs(x = "SlopeROV", y = "SlopeEcho", color = "AbundanceNonSchooling")+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)

L1<-lm(Ben~Average_5m_slope + I(Average_5m_slope^2), data = sitefull)
summary(L1)

L2<-lm(Ben~Average_5m_slope , data = sitefull)
summary(L2)

L3<-lm(Ben~ I(Average_5m_slope^2), data = sitefull)
summary(L3)

L4 <- glm(Ben ~ NASC_10_1m + Average_5m_slope + I(Average_5m_slope^2) + 
            NASC_10_1m:Average_5m_slope , 
          family = gaussian,  data = sitefull)
summary(L4)


L4 <- glm(Ben ~ NASC_10_1m + Average_5m_slope + I(Average_5m_slope^2) + 
            NASC_10_1m:I(Average_5m_slope^2) , 
          family = gaussian,  data = sitefull)
summary(L4)

L4 <- glm(Ben ~ NASC_10_1m + I(Average_5m_slope^2) + 
            NASC_10_1m:I(Average_5m_slope^2) , 
          family = gaussian,  data = sitefull)
summary(L4)

L4 <- glm(Ben ~ NASC_10_1m + Average_5m_slope + 
            NASC_10_1m:Average_5m_slope , 
          family = gaussian,  data = sitefull)
summary(L4)
#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((L4)$null.deviance-(L4)$deviance)/(L4)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

# Install and load the visreg package
install.packages("visreg")
library(visreg)

###plot model vs observed values for ben and NASC
par(mfrow = c(1, 1))
# Create visreg plot
vis <- visreg(L4, "NASC_10_1m", scale = "response", main = "Effect Plot of NASC_10_1m on Ben")

# Extract predicted values from the visreg object
predicted <- vis$fit

# Plot observed points
points(sitefull$NASC_10_1m, sitefull$Ben, col = "blue", pch = 16)

###plot model vs observed values for ben and slope
par(mfrow = c(1, 1))
# Create visreg plot
vis <- visreg(L4, "NASC_10_1m:Average_5m_slope", scale = "response", main = "Effect Plot of Slope on Ben")

# Extract predicted values from the visreg object
predicted <- vis$fit

# Plot observed points
points(sitefull$Average_5m_slope, sitefull$Ben, col = "blue", pch = 16)

par(mfrow = c(2, 2))
plot(L4)

check_overdispersion(D1)


#####calculate residuals and add to dataframe
residuals <- residuals(L4)
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
r <- simulateResiduals(L4, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot (not an issue in density version of this model)
#check dispersion
testDispersion(D1)


###echosounder density model
#interaction between NASC and slope (NASC positive relationships with fish density, NASC positive relationship with slope, 
#Slope has parabolic relationship with fish density- increase then decrease)

#deadzone and nasc positive relationship, but deadzone and fish density negative relationship
###this model has best residual plots and lowest AIC w 54% dev explained
D1 <- glm(Ben ~ NASC_10_1m + I(Average_5m_slope^2) + Std_Dev_Slope + Average_Depth + Cumulative_LG_DZ_Area +
            NASC_10_1m:Average_5m_slope + NASC_10_1m:Cumulative_LG_DZ_Area, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-215

#model copying variables from ROV (no significant predictors and explains 7% of deviation only (ROV model explains 18% - still low))
D1 <- glm(Ben ~  I(Average_5m_slope^2) + Std_Dev_Slope + Average_Depth, 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-215

#model selection based on AIC
#this is the best model based on only keeping significant predictors (still pretty good residual plots and 52% deviance explained )
sitefull$LNASC10<- log(sitefull$NASC_10_1m+1)

D1 <- glm(Ben ~ LNASC10 + 
            LNASC10:Average_5m_slope , 
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-222

###full model
# D1 <- glm(Ben ~ 
#             NASC_10_1m:Average_5m_slope + 
#             NASC_10_1m 
#           , #remove
#           family = gaussian,  data = TF)


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

######density model with ROV data
#model with rock (issues with residuals)
D1 <- glm(Ben ~  I(Slope^2) + Rugosity + Depth + Rock +
            NASC_10_1m:Slope,
          family = gaussian,  data = sitefull)

#model using only variables also available from echosounder (best model - 18% dev explained)
D1 <- glm(Ben ~  I(Slope^2) + Rugosity + Depth,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-199

#model selection with AIC (all significant predictors but start to get issues with residuals)
D1 <- glm(Ben ~  I(Slope^2) + Rugosity ,
          family = gaussian,  data = sitefull)

summary(D1)
AIC(D1) #-201

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

#####
#full ROV/ECHO hybrid model with NASC included (best model with both ROV and echosounder predictors used - 58% dev explained and nice residuals)
D1 <- glm(Ben ~ NASC_10_1m + I(Slope^2) + Rugosity + Depth + Rock +
            NASC_10_1m:Slope,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-220

#model selection for echo/ROV model (most parsimonious model with only significant predictors)
D1 <- glm(Ben ~ NASC_10_1m  + Rugosity +  
            NASC_10_1m:Slope,
          family = gaussian,  data = sitefull)
summary(D1)
AIC(D1) #-221

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