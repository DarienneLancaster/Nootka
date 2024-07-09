# Darienne Lancaster
#May 29, 2024
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
lp("ggplot2")
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
lp("GGally")


#load site dataframe 
load("wdata/site_DE.RData")
str(site_DE)
#create total fish abundance (schooling and benthics) column with herring counts excluded
site_DE$TotAbunNoH<-site_DE$total_number_NoHschoolingfish+site_DE$AbundanceNonSchooling
site_DE$LVol<-log(site_DE$Volume)

#create density value for solo benthic fish + schooling benthic fish
site_DE$TBen<-site_DE$TotBen/site_DE$Volume

site_DE$TBenSQ<-sqrt(site_DE$TBen)


#remove outlier sites (need to edit these echograms)
site_DE<-site_DE%>%
  filter(Site_ID !="NS15", )
####looking at basic linear relationships between different NASC values  (NASC_10_1m has highest correlation)####

BvS<-ggplot(site_DE, aes(x=NASC_10_1m, y=TBenSQ))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(site_DE$NASC_10_1m, site_DE$TBenSQ)
correlation_coefficient



#######First run model with density of benthic fish as response################

TF<- site_DE%>% #TF = TotalFish (no herring)
  dplyr::select(c(Site_ID,TBenSQ,Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Depth_mean_5_MAN))
str(TF)


###################check outliers ###########

explanatory_vars <- names(TF)[!names(TF) %in% "TBenSQ"]
explanatory_vars
plots_list <- list()

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("TBenSQ  vs", var)
  plot <- ggplot(TF, aes(x = !!sym(var), y = TBenSQ)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

#no visible outliers

####site - normality check with histograms################
##
all_vars <- names(TF)[!names(TF) %in% "Site_ID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(TF, aes(x = !!sym(var),)) +
    geom_histogram() +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

### none of the data looks normally distributed,  lots of skew

######try nb.glm
#full model 
#poisson is overdispersed
# M1 <- glm(TBen ~ offset(LVol) +
#                Average_5m_slope + Std_Dev_Slope + + Depth_mean_5_MAN + NASC_10_1m +
#                Cumulative_LG_DZ_Area:NASC_10_1m - Cumulative_LG_DZ_Area , #remove
#              family = poisson, data = TF)
# summary(M1)

M1 <- glm(TBenSQ ~ 
            Average_5m_slope + Std_Dev_Slope  + Depth_mean_5_MAN + I(NASC_10_1m^2) +
            NASC_10_1m:Cumulative_LG_DZ_Area  - Cumulative_LG_DZ_Area , #remove
               family = gaussian, data = TF)
summary(M1)
AIC(M1)

M1 <- glm(TBenSQ ~ 
            Average_5m_slope + Std_Dev_Slope + + Depth_mean_5_MAN + I(NASC_10_1m^2) +
            Cumulative_LG_DZ_Area:NASC_10_1m - I(Cumulative_LG_DZ_Area^2) , #remove
          family = gaussian, data = TF)
summary(M1)

###try gamma distribution
TF$TBenG<-TF$TBen+0.00001


M1 <- glm(TBenG ~ 
            Average_5m_slope + Std_Dev_Slope + + Depth_mean_5_MAN + NASC_10_1m +
            Cumulative_LG_DZ_Area:NASC_10_1m - Cumulative_LG_DZ_Area , #remove
          family = Gamma(link="inverse"), data = TF)
summary(M1)
AIC(M1)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(M1)




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
  geom_point(aes(x = Depth_mean_5_MAN,
                 y = resid)) +
  geom_smooth(aes(x = Depth_mean_5_MAN,
                  y = resid),
              method = "lm")

#check model fit with DHARMa tests
r <- simulateResiduals(M1, n = 1000, plot = TRUE)  #some issues with resid vs pred quantile plot 
#check dispersion
testDispersion(M1)


#gam doesn't seems to be fitting this data well
# ##potential non-linearity - could apply quadratic transform to vars or could try gam
# 
# 
# #start with gam
# #full model
# GAM<-gam(TBenSQ ~  
#             s(Average_5m_slope, k=4) +
#             s(Std_Dev_Slope, k=4) +
#             s(NASC_10_1m, k=4)+
#            s(Depth_mean_5_MAN, k=4) +
#            s(Cumulative_LG_DZ_Area, k=4)+
#            s(NASC_10_1m, by=Cumulative_LG_DZ_Area, k=4),
#            data = TF)
# 
# GAM<-gam(TBenSQ ~  
#            s(Average_5m_slope, k=4) +
#            Std_Dev_Slope +
#            NASC_10_1m +
#            Depth_mean_5_MAN +
#            s(Cumulative_LG_DZ_Area, k=4)
#            ,
#          data = TF)
# 
# summary(GAM)
# k.check(GAM)
# AIC(GAM) #551
# par(mfrow = c(3, 2))
# plot(GAM)
# par(mfrow = c(2, 2))
# gam.check(GAM)



      
