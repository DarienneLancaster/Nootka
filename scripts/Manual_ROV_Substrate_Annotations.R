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

#Ratio vs RugosityROV
t_test_result <- t.test(sitefull_s$Rugosity_s, binfull_s$Ratio_s, paired = TRUE)

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

#scale variables for comparison
str(sitefull)
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
  mutate_at(vars(46:54), as.numeric)
str(binfull_s)

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

#test binned slope from ROV and Echosounder for significant difference
t_test_result <- t.test(binfull_s$Slope_s, binfull_s$Average_5m_slope_s, paired = TRUE)

print(t_test_result)
  

## box plot the data to make sense of the value distribution 
binfull_s$Bin_ID <- paste0(binfull_s$Site_ID, "_", binfull_s$Bin)

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
t_test_result <- t.test(binfull_s$Rugosity_s, binfull_s$Std_Dev_Slope_s, paired = TRUE)

print(t_test_result)

#Ratio vs RugosityROV
t_test_result <- t.test(binfull_s$Rugosity_s, binfull_s$Ratio_s, paired = TRUE)

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
