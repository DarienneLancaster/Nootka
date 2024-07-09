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
install.packages("GGally")
library(GGally)

#load site dataframe 
load("wdata/site_DE.RData")
str(site_DE)
#create total fish abundance (schooling and benthics) column with herring counts excluded
site_DE$TotAbunNoH<-site_DE$total_number_NoHschoolingfish+site_DE$AbundanceNonSchooling
site_DE$LVol<-log(site_DE$Volume)


####looking at basic linear relationships between different NASC values  (NASC_10_1m has highest correlation)####

#decent linear relationship between NASC and benthic fish counts
BvS<-ggplot(site_DE, aes(x=NASC_10_1m, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(site_DE$NASC_10_1m, site_DE$AbundanceNonSchooling)
correlation_coefficient


#Benthic fish counts appear to increase up to ~20degrees and then begin to decrease (effect of poor detection with ROV?)
#(rugosity and deadzone size show same pattern as slope - very correlated variables)
BvS<-ggplot(site_DE, aes(x=Average_5m_slope, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(site_DE$Average_5m_slope, site_DE$AbundanceNonSchooling)

correlation_coefficient

#NASC appears to increase with slope to approximately 20degrees and then moderate decrease 
BvS<-ggplot(site_DE, aes(x=Average_5m_slope, y=NASC_10_1m))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS

ggplot(site_DE, aes(x = NASC_10_1m, y = AbundanceNonSchooling, color = Average_5m_slope)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +  # You can adjust colors as per your preference
  labs(x = "NASC_10_1m", y = "AbundanceNonSchooling", color = "Average_5m_slope")



#######First run model with density of benthic fish as response################

TF<- site_DE%>% #TF = TotalFish (no herring)
  dplyr::select(c(Site_ID,AbundanceNonSchooling,Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Average_Depth, LVol))
str(TF)


###################check outliers ###########

explanatory_vars <- names(TF)[!names(TF) %in% "AbundanceNonSchooling"]
explanatory_vars
plots_list <- list()

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("AbundanceNonSchooling  vs", var)
  plot <- ggplot(TF, aes(x = !!sym(var), y = AbundanceNonSchooling)) +
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

str(TF)

site_explan<-TF%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

ggpairs(site_explan,
        lower = list(continuous = "smooth"))

######try nb.glm####
#full model 
#poisson is overdispersed
# M1 <- glm(AbundanceNonSchooling ~ offset(LVol) +
#                Average_5m_slope + Std_Dev_Slope + + Depth_mean_5_MAN + NASC_10_1m +
#                Cumulative_LG_DZ_Area:NASC_10_1m - Cumulative_LG_DZ_Area , #remove
#              family = poisson, data = TF)
# summary(M1)

#quadratics added in because of apparent increase and then decrease in fish count
#not sure if these are done correctly
M1 <- glm.nb(AbundanceNonSchooling ~ NASC_10_1m + Average_Depth+
               I(Cumulative_LG_DZ_Area^2)+ I(Std_Dev_Slope^2)+ I(Average_5m_slope^2)+
               NASC_10_1m:I(Average_5m_slope^2)  , #remove
            data = TF)
summary(M1)
AIC(M1)

M1 <- glm.nb(AbundanceNonSchooling ~ NASC_10_1m + Average_Depth+
               Cumulative_LG_DZ_Area + Std_Dev_Slope+ Average_5m_slope+
               NASC_10_1m:Average_5m_slope , #remove
             data = TF)
summary(M1)
AIC(M1)


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

#### try a nb gam####

GAM1<-gam(AbundanceNonSchooling ~ offset(LVol) + 
            NASC_10_1m+
            s(NASC_10_1m, by=Average_5m_slope, k=4),
              family = negbin(theta = 1, link = "log"),  data = TF)
summary(GAM1)
k.check(GAM1)
par(mfrow = c(1, 3))
plot(GAM1)
