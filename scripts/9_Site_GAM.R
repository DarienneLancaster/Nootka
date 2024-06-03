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
install.packages("GGally")
library(GGally)

#load site dataframe 
load("wdata/site_DE.RData")
str(site_DE)
#create total fish abundance (schooling and benthics) column with herring counts excluded
site_DE$TotAbunNoH<-site_DE$total_number_NoHschoolingfish+site_DE$AbundanceNonSchooling

####looking at basic linear relationships between different NASC values####

#test log transformed response
site_DEsr<-site_DE
#NS41 and 31 have large NASC outliers from missed semi-pelagic school and missed herring school (I think these should be reexported
#with missing schools removed as this is a measurement error in the ROV methodology)
site_DEsr<-site_DEsr%>%
  filter(Site_ID!="NS41", Site_ID !="NS31")

#experimenting with log transformed response variables (add +1 to avoid issues with log transforming zero values)
#in particular schooling fish needs to be log transformed as it is highly variable depending on whether you saw a fish school or not
#with the ROV. The echosounder will ensonify an entire fish school where as ROV will likely catch only a small segment due to small FOV
#log transforming makes scale of observations for schooling fish more comparable to observations of benthic non-schooling fish
site_DEsr$TotAbunNoH_s<-as.numeric(log(site_DEsr$TotAbunNoH+1))
site_DEsr$AbundanceNonSchooling_s<-as.numeric(log(site_DEsr$AbundanceNonSchooling+1))
site_DEsr$total_number_NoHschoolingfish_s<-as.numeric(log(site_DEsr$total_number_NoHschoolingfish+1))
site_DEsr$total_number_schoolingfish_s<-as.numeric(log(site_DEsr$total_number_schoolingfish+1))
site_DEsr$NASC_10_1m_s<-as.numeric(log(site_DEsr$NASC_10_1m+1))
str(site_DEsr)

#relationship between benthic fish abundance and log of schooling fish (with herring removed - not expected to be correlated with benthics)
#BvS - benthics vs. schooling fish
BvS<-ggplot(site_DEsr, aes(x=total_number_NoHschoolingfish_s, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(site_DEsr$total_number_NoHschoolingfish_s, site_DEsr$AbundanceNonSchooling)
correlation_coefficient
#moderately correlated but zeros suggest non-linear modelling required

#SvNASC- schools vs NASC
SvNASC<-ggplot(site_DEsr, aes(x=NASC_10_1m, y=total_number_NoHschoolingfish_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
SvNASC
correlation_coefficient <- cor(site_DEsr$NASC_10_1m, site_DEsr$total_number_schoolingfish_s)
correlation_coefficient
#moderately correlated but zeros suggest non-linear modelling required

#TFvNASC- total fish (school and benthic) vs NASC
TFvNASC<-ggplot(site_DEsr, aes(x=NASC_10_1m, y=TotAbunNoH_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
TFvNASC
correlation_coefficient <- cor(site_DEsr$NASC_10_1m, site_DEsr$TotAbunNoH_s)
correlation_coefficient
#relatively strong correlation (NASC10_1m is highest correlation (5m not bad, 15 is less))

#TFnoLOGvNASC- total fish not log transformed (school and benthic) vs NASC
TFnoLOGvNASC<-ggplot(site_DE, aes(x=NASC_10_1m, y=TotAbunNoH))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
TFnoLOGvNASC
correlation_coefficient <- cor(site_DE$NASC_10_1m, site_DE$TotAbunNoH)
correlation_coefficient

#BvNASC - benthics vs NASC
BvNASC<-ggplot(site_DEsr, aes(x=NASC_10_1m, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3)
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvNASC
correlation_coefficient <- cor(site_DE$NASC_10_1m, site_DE$AbundanceNonSchooling)
correlation_coefficient
#relatively strong correlation (NASC10_1m is highest correlation (5m not bad, 15 is less))

#Deadzone area seems to be moderately correlated with RF species richness (Ratio and StDevofSlope is also correlated)
NASC1m<-ggplot(site_DE, aes(x=Cumulative_LG_DZ_Area , y=RFSpeciesRichness))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASC1m
correlation_coefficient <- cor(site_DE$Cumulative_LG_DZ_Area , site_DE$RFSpeciesRichness)
correlation_coefficient

####Based on prelim investigation I will first model total fish abundance with no herring #####
#vs 10mRFZ_1mDZ_NASC with additional predictor vars#
#then I'll move on to NASC vs benthic rockfish and finally species richness as response var with habitat predictor variables

#######First run model with Total Fish Abundance (minus herring) as response and Volume as an offset################

TF<- site_DE%>% #TF = TotalFish (no herring)
  dplyr::select(c(Site_ID,TotAbunNoH,Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Depth_mean_5_MAN, Volume))
str(TF)

###################check outliers ###########

explanatory_vars <- names(TF)[!names(TF) %in% "TotAbunNoH"]
explanatory_vars
plots_list <- list()

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("TotAbunNoH  vs", var)
  plot <- ggplot(TF, aes(x = !!sym(var), y = TotAbunNoH)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

#NS15 has very high totalfish abundances due to large numbers of benthic juvenile rockfish schools we saw at this site
#leave in this outlier for now, no good reason to remove
#there are also some NASC outliers. NS41 seems like we hit a semi pelagic school with the echosounder
#that we didn't see with the ROV (this is an inherent issue with looking at Total Fish (from ROV) vs. NASC, ROV was not designed to see all
#the schooling fish, only the benthic ones, this could be why NASC is doing a better job predicting benthic fish than total fish (ROV has blind spots in water column so not matching data well))
#may want to remove NS31 (large herring school that was missed on ROV - need to reexport with herring ball removed)

#for now try model without removing outliers

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


#####site - create pair plot with correlation coefficients for all variables####

site_explan<-TF%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

####can also check colinearity with variance inflation factors####
model <- lm(TotAbunNoH ~ ., data = site_explan)
vif(model)
#remove Ratio (highly collinear)
site_explan2<-site_explan%>%
  dplyr::select(-c( Ratio))
str(site_explan2)

model2 <- lm(TotAbunNoH ~ ., data = site_explan2)
vif(model2)

#none of the other variables have a VIF above 3 which is a typical cut off

#create pairs plot with pearsons correlation coefficeint and smoother on dotplot

ggpairs(site_explan2,
        lower = list(continuous = "smooth"))

#cumulativearea (e.g. deadzone size) and Std deviation of slope/Ratio (E.g. rugosity) are highly collinear with average slope
#(Do not use rugosity metrics in model as deadzone size seems to get at this as well and is less collinear with slope)
str(TF)

TFstd<-TF%>%
  mutate(Average_5m_slope_s=scale(Average_5m_slope),
         Std_Dev_Slope_s=scale(Std_Dev_Slope),
         Ratio_s=scale(Ratio),
         Cumulative_LG_DZ_Area_s=scale(Cumulative_LG_DZ_Area),
         NASC_10_1m_s=scale(NASC_10_1m), 
         Depth_mean_5_MAN_s=scale(Depth_mean_5_MAN),
         LVol=log(Volume))%>%
  dplyr::select(-c(3:9)) #remove non-standardized columns

site_explan<-TFstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan)


###################standardized vars check outliers ###########

explanatory_vars <- names(TFstd)[!names(TFstd) %in% "TotAbunNoH"]
explanatory_vars
plots_list <- list()
str(TFstd)

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("TotAbunNoH  vs", var)
  plot <- ggplot(TFstd, aes(x = !!sym(var), y = TotAbunNoH)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

#same outliers as non-standardized


####standardized site - normality check with histograms################
##
all_vars <- names(TFstd)[!names(TFstd) %in% "Site_ID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(TFstd, aes(x = !!sym(var),)) +
    geom_histogram() +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)


#create pairs plot with pearsons correlation coefficeint and smoother on dotplot
#after standardization set the vars back to numeric to work with code
TFstd <- TFstd %>%
  mutate_at(vars(3:8), as.numeric)

str(TFstd)

site_explan<-TFstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

ggpairs(site_explan,
        lower = list(continuous = "smooth"))

#same collinearity issues with depth but we will continue with modelling
#check variance inflation factors
site_explan2<-TFstd%>%
  dplyr::select(-c( Site_ID, Ratio_s))
str(site_explan2)

model3 <- lm(TotAbunNoH ~ ., data = site_explan2)
vif(model3)
#Should remove ratio, too colinear (use only one rugosity metric)

#check diagnostic plots####
#NASC
lmNASC <- lm(TotAbunNoH*LVol ~ NASC_10_1m_s, data = TFstd) #total fish is offset by log transformed sampling volume
lmNASC
summary(lmNASC)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmNASC)

#stdev_slope
lmSTDslope <- lm(TotAbunNoH*LVol ~ Std_Dev_Slope_s , data = TFstd) #total fish is offset by log transformed sampling volume
lmSTDslope
summary(lmSTDslope)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmSTDslope)

#Ratio_s
lmRatio <- lm(TotAbunNoH*LVol ~ Ratio_s , data = TFstd) #total fish is offset by log transformed sampling volume
lmRatio
summary(lmRatio)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmRatio)

#Cumulative_LG_DZ_Area_s 
lmDZ <- lm(TotAbunNoH*LVol ~ Cumulative_LG_DZ_Area_s  , data = TFstd) #total fish is offset by log transformed sampling volume
lmDZ
summary(lmDZ)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmDZ)

#Depth_mean_5_MAN_s
lmD <- lm(TotAbunNoH*LVol ~ Depth_mean_5_MAN_s  , data = TFstd) #total fish is offset by log transformed sampling volume
lmD
summary(lmD)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmD)

#Rediduals vs Fitted - evidence of non-linearity and heteroscedasticity (over dispersed)
#Scale-Location - No major trends but some patterned variation of residuals around  smoothing line
#QQ - residuals are not normally distributed (heavy tails and skewedness with some outliers)
#residuals vs leverage (cooks distance)  - only large cooks distance looks like NS31 (row 23 - may need to remove)

#try poisson glm

M1 <- glm(TotAbunNoH ~ offset(LVol) + Average_5m_slope_s + Std_Dev_Slope_s +
            Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
            Average_5m_slope_s:Cumulative_LG_DZ_Area_s +
            Average_5m_slope_s:Depth_mean_5_MAN_s +
            NASC_10_1m_s:Cumulative_LG_DZ_Area_s +
            Std_Dev_Slope_s:Cumulative_LG_DZ_Area_s +
            Std_Dev_Slope_s:Average_5m_slope_s,
          family = poisson,  data = TFstd)
summary(M1)


#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

###check overdispersion with ratio of residual deviance to degrees of freedom

# Extract the residual deviance and degrees of freedom
residual_deviance <- M1$deviance
df <- M1$df.residual

# Calculate the ratio of residual deviance to residual degrees of freedom
ratio <- residual_deviance / df

# Compare the ratio to 1 to check for overdispersion
if (ratio > 1) {
  cat("Overdispersion present: Residual deviance / df =", ratio, "\n")
} else {
  cat("No overdispersion: Residual deviance / df =", ratio, "\n")
}

#can also check with this tool
check_overdispersion(M1)

# Plot the four diagnostic plots 
par(mfrow = c(2, 2))
plot(M1)

#data is overdispersed and there are patterns
#in the diagnostic plots (overdispersion) need to try quasipoisson and then negative binomial then GAM

#try quasi-poisson to deal with overdispersion (all vars and interactions removed stepwise until significant)

#full model - least significant vars removed stepwise
# M1 <- glm(TotAbunNoH ~ offset(LVol) + Average_5m_slope_s + Std_Dev_Slope_s +
#             Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
#             Average_5m_slope_s:Cumulative_LG_DZ_Area_s +
#             Average_5m_slope_s:Depth_mean_5_MAN_s +
#             NASC_10_1m_s:Cumulative_LG_DZ_Area_s +
#             Std_Dev_Slope_s:Cumulative_LG_DZ_Area_s +
#             Std_Dev_Slope_s:Average_5m_slope_s,
#           family = quasipoisson,  data = TFstd)
# summary(M1)

M1qp <- glm(TotAbunNoH ~ offset(LVol)  +
            Std_Dev_Slope_s:Cumulative_LG_DZ_Area_s,
          family = quasipoisson,  data = TFstd)
summary(M1qp)


#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1qp)$null.deviance-(M1qp)$deviance)/(M1qp)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

###check overdispersion with ratio of residual deviance to degrees of freedom

# Extract the residual deviance and degrees of freedom
residual_deviance <- M1qp$deviance
df <- M1qp$df.residual

# Calculate the ratio of residual deviance to residual degrees of freedom
ratio <- residual_deviance / df

# Compare the ratio to 1 to check for overdispersion
if (ratio > 1) {
  cat("Overdispersion present: Residual deviance / df =", ratio, "\n")
} else {
  cat("No overdispersion: Residual deviance / df =", ratio, "\n")
}

check_overdispersion(M1qp)

# Plot the four diagnostic plots 
par(mfrow = c(2, 2))
plot(M1qp)

#model is still overdispersed and there are some strong outliers and trends but starting to look better.  Try neg binom.

#try negative binomial to deal with overdispersion (all vars and interactions removed stepwise until significant)

#full NB model
# M1nb <- glm(TotAbunNoH ~ offset(LVol) + Average_5m_slope_s + Std_Dev_Slope_s +
#             Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
#             Average_5m_slope_s:Cumulative_LG_DZ_Area_s +
#             Average_5m_slope_s:Depth_mean_5_MAN_s +
#             NASC_10_1m_s:Cumulative_LG_DZ_Area_s +
#             Std_Dev_Slope_s:Cumulative_LG_DZ_Area_s +
#             Std_Dev_Slope_s:Average_5m_slope_s,
#             data = TFstd)
# summary(M1nb)

M1nb <- glm.nb(TotAbunNoH ~ offset(LVol) + 
               NASC_10_1m_s + 
                 Std_Dev_Slope_s:Average_5m_slope_s,
             data = TFstd)
summary(M1nb)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1nb)$null.deviance-(M1nb)$deviance)/(M1nb)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

###check overdispersion with ratio of residual deviance to degrees of freedom

# Extract the residual deviance and degrees of freedom
residual_deviance <- M1nb$deviance
df <- M1nb$df.residual

# Calculate the ratio of residual deviance to residual degrees of freedom
ratio <- residual_deviance / df

# Compare the ratio to 1 to check for overdispersion
if (ratio > 1) {
  cat("Overdispersion present: Residual deviance / df =", ratio, "\n")
} else {
  cat("No overdispersion: Residual deviance / df =", ratio, "\n")
}

check_overdispersion(M1nb)

# Plot the four diagnostic plots 
par(mfrow = c(2, 2))
plot(M1nb)

#qqplot is still showing non-normal patterns in distribution and seeing some trends in the residuals
#move on to GAMs to allow for non-linear patterns

`?`(family.mgcv)

GAMnb<-gam(TotAbunNoH ~ offset(LVol) + s(Average_5m_slope_s, by=Depth_mean_5_MAN_s)+
           s(Std_Dev_Slope_s)+
           s(Cumulative_LG_DZ_Area_s)+
           s(NASC_10_1m_s) +
           s(Depth_mean_5_MAN_s),
            family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMnb)

#try forward selection (backward selection is confusing with GAMs with potential non-linear colinearity
# this method is from Zuur 2009. Run models with only one predictor and compare AICs. Then try with two vars
#and 3 vars until you no longer get lower AICs)
GAMn<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Average_5m_slope) +
          s(Std_Dev_Slope) +
            s(NASC_10_1m) ,
          family = negbin(theta=1, link = "log"),  data = TFt)
summary(GAMn)
AIC(GAMn) #551
par(mfrow = c(2, 2))
plot(GAMn)


GAMn<-gam(TotAbunNoH ~ offset(LVol) + 
             s(NASC_10_1m_s) ,
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMn)
AIC(GAMn) #551
plot(GAMn)

GAMs<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Average_5m_slope_s) ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMs)
AIC(GAMs) #534

GAMr<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Std_Dev_Slope_s) ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMr)
AIC(GAMr) #546

GAMdz<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Cumulative_LG_DZ_Area_s) ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdz)
AIC(GAMdz) #554

GAMd<-gam(TotAbunNoH ~ offset(LVol) + 
             s(Depth_mean_5_MAN_s) ,
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMd)
AIC(GAMd) #552

#lowest AIC is slope model (GAMs). Now fit model with 2 vars keeping slope always
GAMns<-gam(TotAbunNoH ~ offset(LVol) + 
            s(NASC_10_1m_s)+ 
            s(Average_5m_slope_s) ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMns)
AIC(GAMns) #519


GAMrs<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Std_Dev_Slope_s)+ 
            s(Average_5m_slope_s) ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMrs)
AIC(GAMrs) #522

GAMdzs<-gam(TotAbunNoH ~ offset(LVol) + 
             s(Cumulative_LG_DZ_Area_s)+ 
             s(Average_5m_slope_s) ,
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdzs)
AIC(GAMdzs) #526

GAMds<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Depth_mean_5_MAN_s) + 
            s(Average_5m_slope_s),
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMds)
AIC(GAMds) #529

#lowest AIC is slope + NASC model (GAMns). Now fit model with 3 vars keeping slope and NASC always

##TOP MODEL##
GAMrsn<-gam(TotAbunNoH ~ offset(LVol) +
             s(NASC_10_1m_s)+
             s(Std_Dev_Slope_s)+ 
             s(Average_5m_slope_s) ,
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMrsn)
AIC(GAMrsn) #482

GAMdzsn<-gam(TotAbunNoH ~ offset(LVol) + 
              s(NASC_10_1m_s)+
              s(Cumulative_LG_DZ_Area_s)+ 
              s(Average_5m_slope_s) ,
            family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdzsn)
AIC(GAMdzsn) #508

GAMdsn<-gam(TotAbunNoH ~ offset(LVol) + 
             s(NASC_10_1m_s)+
             s(Depth_mean_5_MAN_s) + 
             s(Average_5m_slope_s),
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdsn)
AIC(GAMdsn) #514

#lowest AIC is slope + NASC + std_slope(rugosity) model (GAMns). Now fit model with 4 vars keeping slope and NASC and rugosity always


GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
               s(NASC_10_1m_s)+
               s(Std_Dev_Slope_s)+ 
               s(Cumulative_LG_DZ_Area_s)+ 
               s(Average_5m_slope_s) ,
             family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdzsnr)
AIC(GAMdzsnr) #483

GAMdsnr<-gam(TotAbunNoH ~ offset(LVol) + 
              s(NASC_10_1m_s)+
              s(Std_Dev_Slope_s)+ 
              s(Depth_mean_5_MAN_s) + 
              s(Average_5m_slope_s),
            family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdsnr)
AIC(GAMdsnr) #484

#best model is slope + NASC + std_slope(rugosity) model (AIC = 482) and all vars are significant, explains 89.4% of variance.
# 4 var models have similar AICs but not all vars are significant and simpler model is always best.

par(mfrow = c(1, 1))
M3Pred <- predict(GAMrsn, se = TRUE, type = "response") 
plot(TFstd$NASC_10_1m_s, TFstd$TotAbunNoH, cex = 1.1, pch = 16, main = "Negative binomial GAM", 
     xlab = "NASC", ylab = "Total Fish") 
I <- order(TFstd$NASC_10_1m_s)
lines(TFstd$NASC_10_1m_s[I], M3Pred$fit[I], lwd = 2)
lines(TFstd$NASC_10_1m_s[I], M3Pred$fit[I] + 2 * M3Pred$se.fit[I], lty = 2, lwd = 2)
lines(TFstd$NASC_10_1m_s[I], M3Pred$fit[I] + 2 * M3Pred$se.fit[I], lty = 2, lwd = 2)
for (i in 1:52){ y <- rnbinom(100, size = 11.8, mu = M3Pred$fit[i])
points(rep(TFstd$NASC_10_1m_s[i], 100), y, cex = 0.5)}


# Plot the  diagnostic plots of smoothers 
par(mfrow = c(2, 2))
plot(GAMrsn)

##plot pearson residuals vs explanatory variables
par(mfrow = c(2, 2))
# Extract Pearson residuals
residuals <- residuals(GAMrsn, type = "pearson")

# Extract NASC_10_1m_s values
NASC_10_1m_s <- TFstd$NASC_10_1m_s  # Assuming NASC_10_1m_s is in TFstd

# Fit a smoother
smoothed_line <- smooth.spline(NASC_10_1m_s, residuals)

# Create a scatter plot of Pearson residuals versus NASC_10_1m_s
plot(NASC_10_1m_s, residuals, xlab = "NASC_10_1m_s", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs NASC_10_1m_s")

# Add the smoother line to the plot
lines(smoothed_line, col = "red")


# Fit a smoother
smoothed_line <- smooth.spline(TFstd$Average_5m_slope_s, residuals)

# Create a scatter plot of Pearson residuals versus NASC_10_1m_s
plot(TFstd$Average_5m_slope_s, residuals, xlab = "Slope", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Slope")

# Add the smoother line to the plot
lines(smoothed_line, col = "red")

# Fit a smoother
smoothed_line <- smooth.spline(TFstd$Std_Dev_Slope_s, residuals, spar = 0.5)

# Create a scatter plot of Pearson residuals versus NASC_10_1m_s
plot(TFstd$Std_Dev_Slope_s, residuals, xlab = "Rugosity", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Rugosity")

# Add the smoother line to the plot
lines(smoothed_line, col = "red")

#repeat GAM model selection with non-standardized data
#try contraining line wiggliness


#####try constraining k to 4
#try forward selection (backward selection is confusing with GAMs with potential non-linear colinearity
# this method is from Zuur 2009. Run models with only one predictor and compare AICs. Then try with two vars
#and 3 vars until you no longer get lower AICs)

GAMn<-gam(TotAbunNoH ~ offset(LVol) + 
            s(NASC_10_1m_s, k = 4, bs="cs") ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMn)
AIC(GAMn) #550

GAMs<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Average_5m_slope_s, k = 4, bs="cs") ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMs)
AIC(GAMs) #548

GAMr<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Std_Dev_Slope_s, k = 4, bs="cs") ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMr)
AIC(GAMr) #550

GAMdz<-gam(TotAbunNoH ~ offset(LVol) + 
             s(Cumulative_LG_DZ_Area_s, k = 4, bs="cs") ,
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdz)
AIC(GAMdz) #558

GAMd<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Depth_mean_5_MAN_s, k = 4, bs="cs") ,
          family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMd)
AIC(GAMd) #551

#lowest AIC is slope model (GAMs). Now fit model with 2 vars keeping slope always
GAMns<-gam(TotAbunNoH ~ offset(LVol) + 
             s(NASC_10_1m_s, k = 4, bs="cs")+ 
             s(Average_5m_slope_s, k = 4, bs="cs") ,
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMns)
AIC(GAMns) #524


GAMrs<-gam(TotAbunNoH ~ offset(LVol) + 
             s(Std_Dev_Slope_s, k = 4, bs="cs")+ 
             s(Average_5m_slope_s, k = 4, bs="cs") ,
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMrs)
AIC(GAMrs) #537

GAMdzs<-gam(TotAbunNoH ~ offset(LVol) + 
              s(Cumulative_LG_DZ_Area_s, k = 4, bs="cs")+ 
              s(Average_5m_slope_s, k = 4, bs="cs") ,
            family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdzs)
AIC(GAMdzs) #541

GAMds<-gam(TotAbunNoH ~ offset(LVol) + 
             s(Depth_mean_5_MAN_s, k = 4, bs="cs") + 
             s(Average_5m_slope_s, k = 4, bs="cs"),
           family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMds)
AIC(GAMds) #531

#lowest AIC is slope + NASC model (GAMns). Now fit model with 3 vars keeping slope and NASC always

GAMrsn<-gam(TotAbunNoH ~ offset(LVol) +
              s(NASC_10_1m_s, k = 4, bs="cs")+
              s(Std_Dev_Slope_s, k = 4, bs="cs")+ 
              s(Average_5m_slope_s, k = 4, bs="cs") ,
            family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMrsn)
AIC(GAMrsn) #508

GAMdzsn<-gam(TotAbunNoH ~ offset(LVol) + 
               s(NASC_10_1m_s, k = 4, bs="cs")+
               s(Cumulative_LG_DZ_Area_s, k = 4, bs="cs")+ 
               s(Average_5m_slope_s, k = 4, bs="cs") ,
             family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdzsn)
AIC(GAMdzsn) #511

GAMdsn<-gam(TotAbunNoH ~ offset(LVol) + 
              s(NASC_10_1m_s, k = 4, bs="cs")+
              s(Depth_mean_5_MAN_s, k = 4, bs="cs") + 
              s(Average_5m_slope_s, k = 4, bs="cs"),
            family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdsn)
AIC(GAMdsn) #515

#lowest AIC is slope + NASC + std_slope(rugosity) model (GAMns). Now fit model with 4 vars keeping slope and NASC and rugosity always


GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                s(NASC_10_1m_s, k = 4, bs="cs")+
                s(Std_Dev_Slope_s, k = 4, bs="cs")+ 
                s(Cumulative_LG_DZ_Area_s, k = 4, bs="cs")+ 
                s(Average_5m_slope_s, k = 4, bs="cs") ,
              family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdzsnr)
AIC(GAMdzsnr) #498

GAMdsnr<-gam(TotAbunNoH ~ offset(LVol) + 
               s(NASC_10_1m_s, k = 4, bs="cs")+
               s(Std_Dev_Slope_s, k = 4, bs="cs")+ 
               s(Depth_mean_5_MAN_s, k = 4, bs="cs") + 
               s(Average_5m_slope_s, k = 4, bs="cs"),
             family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdsnr)
AIC(GAMdsnr) #503

#lowest AIC is slope + NASC + std_slope(rugosity) and deadzone model (GAMdzsnr). Now fit model with 4 vars keeping slope and NASC and rugosity always


GAMdzsnrd<-gam(TotAbunNoH ~ offset(LVol) + 
                s(NASC_10_1m_s, k = 4, bs="cs")+
                s(Std_Dev_Slope_s, k = 4, bs="cs")+ 
                s(Depth_mean_5_MAN_s, k = 4, bs="cs") +
                s(Cumulative_LG_DZ_Area_s, k = 4, bs="cs")+ 
                s(Average_5m_slope_s, k = 4, bs="cs") ,
              family = negbin(theta=1, link = "log"),  data = TFstd)
summary(GAMdzsnrd)
AIC(GAMdzsnrd) #499

#GAMdzsnr is best model (AIC 498), all vars significant, explains 58% of deviance. 5 var model depth is not significant and AIC is comparable.
#so simpler model is best


par(mfrow = c(1, 1))
M3Pred <- predict(GAMdzsnr, se = TRUE, type = "response") 
plot(TFstd$NASC_10_1m_s, TFstd$TotAbunNoH, cex = 1.1, pch = 16, main = "Negative binomial GAM", 
     xlab = "NASC", ylab = "Total Fish") 
I <- order(TFstd$NASC_10_1m_s)
lines(TFstd$NASC_10_1m_s[I], M3Pred$fit[I], lwd = 2)
lines(TFstd$NASC_10_1m_s[I], M3Pred$fit[I] + 2 * M3Pred$se.fit[I], lty = 2, lwd = 2)
lines(TFstd$NASC_10_1m_s[I], M3Pred$fit[I] + 2 * M3Pred$se.fit[I], lty = 2, lwd = 2)
for (i in 1:52){ y <- rnbinom(100, size = 11.8, mu = M3Pred$fit[i])
points(rep(TFstd$NASC_10_1m_s[i], 100), y, cex = 0.5)}


# Plot the  diagnostic plots of smoothers 
par(mfrow = c(2, 2))
plot(GAMdzsnr)

##plot pearson residuals vs explanatory variables
par(mfrow = c(2, 2))
# Extract Pearson residuals
residuals <- residuals(GAMdzsnr, type = "pearson")

#Plot residuals vs. explanatory variables with a smoothing spline

smoothed_line1 <- smooth.spline(TFstd$NASC_10_1m_s, residuals, spar = 1.25)
smoothed_line2 <- smooth.spline(TFstd$Average_5m_slope_s, residuals, spar = 1.25)
smoothed_line3 <- smooth.spline(TFstd$Std_Dev_Slope_s, residuals, spar = 1.25)
smoothed_line4 <- smooth.spline(TFstd$Cumulative_LG_DZ_Area_s, residuals, spar = 1.25)


# Create the first scatter plot with LOESS smoother
plot(TFstd$NASC_10_1m_s, residuals, xlab = "NASC_10_1m_s", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs NASC_10_1m_s")
lines(smoothed_line1, col = "red")

# Create the second scatter plot with LOESS smoother
plot(TFstd$Average_5m_slope_s, residuals, xlab = "Slope", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Slope")
lines(smoothed_line2, col = "red")

# Create the third scatter plot with LOESS smoother
plot(TFstd$Std_Dev_Slope_s, residuals, xlab = "Rugosity", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Rugosity")
lines(smoothed_line3, col = "red")

# Create the fourth scatter plot with LOESS smoother
plot(TFstd$Cumulative_LG_DZ_Area_s, residuals, xlab = "Deadzone", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Deadzone")
lines(smoothed_line4, col = "red")

# residuals look pretty good

### rerun forwards model selection on non-standardized data

#####try constraining k to 4
#try forward selection (backward selection is confusing with GAMs with potential non-linear colinearity
# this method is from Zuur 2009. Run models with only one predictor and compare AICs. Then try with two vars
#and 3 vars until you no longer get lower AICs)
str(TF)
TF$LVol<-log(TF$Volume)

GAMn<-gam(TotAbunNoH ~ offset(LVol) + 
            s(NASC_10_1m, k = 10, bs="cs") ,
          family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMn)
AIC(GAMn) #551

GAMs<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Average_5m_slope, k = 10, bs="cs") ,
          family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMs)
AIC(GAMs) #537

GAMr<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Std_Dev_Slope, k = 10, bs="cs") ,
          family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMr)
AIC(GAMr) #540

GAMdz<-gam(TotAbunNoH ~ offset(LVol) + 
             s(Cumulative_LG_DZ_Area, k = 10, bs="cs") ,
           family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMdz)
AIC(GAMdz) #561

GAMd<-gam(TotAbunNoH ~ offset(LVol) + 
            s(Depth_mean_5_MAN, k = 10, bs="cs") ,
          family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMd)
AIC(GAMd) #553

#lowest AIC is slope model (GAMs). Now fit model with 2 vars keeping slope always
GAMns<-gam(TotAbunNoH ~ offset(LVol) + 
             s(NASC_10_1m, k = 10, bs="cs")+ 
             s(Average_5m_slope, k = 10, bs="cs") ,
           family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMns)
AIC(GAMns) #523


GAMrs<-gam(TotAbunNoH ~ offset(LVol) + 
             s(Std_Dev_Slope, k = 10, bs="cs")+ 
             s(Average_5m_slope, k = 10, bs="cs") ,
           family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMrs)
AIC(GAMrs) #528

GAMdzs<-gam(TotAbunNoH ~ offset(LVol) + 
              s(Cumulative_LG_DZ_Area, k = 10, bs="cs")+ 
              s(Average_5m_slope, k = 10, bs="cs") ,
            family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMdzs)
AIC(GAMdzs) #538

GAMds<-gam(TotAbunNoH ~ offset(LVol) + 
             s(Depth_mean_5_MAN, k = 10, bs="cs") + 
             s(Average_5m_slope, k = 10, bs="cs"),
           family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMds)
AIC(GAMds) #533

#lowest AIC is slope + NASC model (GAMns). Now fit model with 3 vars keeping slope and NASC always

GAMrsn<-gam(TotAbunNoH ~ offset(LVol) +
              s(NASC_10_1m, k = 10, bs="cs")+
              s(Std_Dev_Slope, k = 10, bs="cs")+ 
              s(Average_5m_slope, k = 10, bs="cs") ,
            family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMrsn)
AIC(GAMrsn) #510

GAMdzsn<-gam(TotAbunNoH ~ offset(LVol) + 
               s(NASC_10_1m, k = 10, bs="cs")+
               s(Cumulative_LG_DZ_Area, k = 10, bs="cs")+ 
               s(Average_5m_slope, k = 10, bs="cs") ,
             family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMdzsn)
AIC(GAMdzsn) #513

GAMdsn<-gam(TotAbunNoH ~ offset(LVol) + 
              s(NASC_10_1m, k = 10, bs="cs")+
              s(Depth_mean_5_MAN, k = 10, bs="cs") + 
              s(Average_5m_slope, k = 10, bs="cs"),
            family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMdsn)
AIC(GAMdsn) #519

#lowest AIC is slope + NASC + std_slope(rugosity) model (GAMrsn). Now fit model with 4 vars keeping slope and NASC and rugosity always

##TOP MODEL##
GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area, k = 10, bs="cs")+ 
                s(Average_5m_slope, k = 10, bs="cs") ,
              family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #500

GAMdsnr<-gam(TotAbunNoH ~ offset(LVol) + 
               s(NASC_10_1m, k = 10, bs="cs")+
               s(Std_Dev_Slope, k = 10, bs="cs")+ 
               s(Depth_mean_5_MAN, k = 10, bs="cs") + 
               s(Average_5m_slope, k = 10, bs="cs"),
             family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMdsnr)
AIC(GAMdsnr) #508

#lowest AIC is slope + NASC + std_slope(rugosity) and deadzone model (GAMdzsnr). Now fit model with 4 vars keeping slope and NASC and rugosity always


GAMdzsnrd<-gam(TotAbunNoH ~ offset(LVol) + 
                 s(NASC_10_1m, k = 10, bs="cs")+
                 s(Std_Dev_Slope, k = 10, bs="cs")+ 
                 s(Depth_mean_5_MAN, k = 10, bs="cs") +
                 s(Cumulative_LG_DZ_Area, k = 10, bs="cs")+ 
                 s(Average_5m_slope, k = 10, bs="cs") ,
               family = negbin(theta=1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnrd)
AIC(GAMdzsnrd) #500

#GAMdzsnr is best model (AIC 500), all vars significant, explains 62% of deviance. 5 var model depth is not significant and AIC is comparable.
#so simpler model is best

#check our k value is large enough (should be quite a bit larger than our estimated degrees of freedom - https://r.qcbs.ca/workshop08/book-en/gam-model-checking.html)
k.check(GAMdzsnr)
#k looks much larger than edf so okay

#check our residuals plots
par(mfrow = c(2, 2))
gam.check(GAMdzsnr)

par(mfrow = c(1, 1))
M3Pred <- predict(GAMdzsnr, se = TRUE, type = "response") 
plot(TF$NASC_10_1m, TF$TotAbunNoH, cex = 1.1, pch = 16, main = "Negative binomial GAM", 
     xlab = "NASC", ylab = "Total Fish") 
I <- order(TF$NASC_10_1m)
lines(TF$NASC_10_1m[I], M3Pred$fit[I], lwd = 2)
lines(TF$NASC_10_1m[I], M3Pred$fit[I] + 2 * M3Pred$se.fit[I], lty = 2, lwd = 2)
lines(TF$NASC_10_1m[I], M3Pred$fit[I] + 2 * M3Pred$se.fit[I], lty = 2, lwd = 2)
for (i in 1:52){ y <- rnbinom(100, size = 11.8, mu = M3Pred$fit[i])
points(rep(TF$NASC_10_1m[i], 100), y, cex = 0.5)}


# Plot the smoothers 
par(mfrow = c(2, 2))
plot(GAMdzsnr)

##plot pearson residuals vs explanatory variables
par(mfrow = c(2, 2))
# Extract Pearson residuals
residuals <- residuals(GAMdzsnr, type = "pearson")

#Plot residuals vs. explanatory variables with a smoothing spline

smoothed_line1 <- smooth.spline(TF$NASC_10_1m, residuals, spar = 1.5)
smoothed_line2 <- smooth.spline(TF$Average_5m_slope, residuals, spar = 1)
smoothed_line3 <- smooth.spline(TF$Std_Dev_Slope, residuals, spar = 1)
smoothed_line4 <- smooth.spline(TF$Cumulative_LG_DZ_Area, residuals, spar = 1)


# Create the first scatter plot with LOESS smoother
plot(TF$NASC_10_1m, residuals, xlab = "NASC_10_1m", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs NASC_10_1m")
lines(smoothed_line1, col = "red")

# Create the second scatter plot with LOESS smoother
plot(TF$Average_5m_slope, residuals, xlab = "Slope", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Slope")
lines(smoothed_line2, col = "red")

# Create the third scatter plot with LOESS smoother
plot(TF$Std_Dev_Slope, residuals, xlab = "Rugosity", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Rugosity")
lines(smoothed_line3, col = "red")

# Create the fourth scatter plot with LOESS smoother
plot(TF$Cumulative_LG_DZ_Area, residuals, xlab = "Deadzone", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Deadzone")
lines(smoothed_line4, col = "red")



### lets try a tweedie distribution on our Top Model instead of neg.bin

##TOP MODEL##
twGAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area, k = 10, bs="cs")+ 
                s(Average_5m_slope, k = 10, bs="cs") ,
              family = tw(link = "log"),  data = TF, method="REML")
summary(twGAMdzsnr)
AIC(twGAMdzsnr) #513 - compared to top model neg.bin of 500
summary(twGAMdzsnr)$p.table
summary(twGAMdzsnr)$s.table
k.check(twGAMdzsnr)
par(mfrow=c(2,2))
gam.check(twGAMdzsnr)
AIC(GAMdzsnr, twGAMdzsnr)
logLik.gam(GAMdzsnr, REML=TRUE) #the higher the log-likelihood the better so neg.bin is better than tweedie model (note: only compare models with the same number of predictor variables as adding vars will automatically increase log-likelihood)
logLik.gam(twGAMdzsnr, REML=TRUE)
#neg.bin is better than the tweedie model but it still looks like we're missing something in our model
#maybe we need to try some interactions

##try backwards selection with interactions and linear terms
str(TF)

GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                NASC_10_1m + Std_Dev_Slope + Cumulative_LG_DZ_Area + Average_5m_slope + Depth_mean_5_MAN +
                  s(NASC_10_1m, k = 10, bs="cs")+
                  s(Std_Dev_Slope, k = 10, bs="cs")+ 
                  s(Cumulative_LG_DZ_Area, by = Std_Dev_Slope, k = 10, bs="cs")+ 
                  s(Average_5m_slope, by= Depth_mean_5_MAN,  k = 10, bs="cs") ,
                family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #502

GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                 Std_Dev_Slope + Cumulative_LG_DZ_Area + Average_5m_slope + Depth_mean_5_MAN +
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area, by = Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Average_5m_slope, by= Depth_mean_5_MAN,  k = 10, bs="cs") ,
              family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #502

GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                Std_Dev_Slope + Cumulative_LG_DZ_Area + Average_5m_slope + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area, by = Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Average_5m_slope, by= Depth_mean_5_MAN,  k = 10, bs="cs") ,
              family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #503


GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                Cumulative_LG_DZ_Area + Average_5m_slope + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area, by = Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Average_5m_slope, by= Depth_mean_5_MAN,  k = 10, bs="cs") ,
              family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #503

GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                Average_5m_slope + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area, by = Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Average_5m_slope, by= Depth_mean_5_MAN,  k = 10, bs="cs") ,
              family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #502

GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area, by = Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Average_5m_slope, by= Depth_mean_5_MAN,  k = 10, bs="cs") ,
              family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #503

GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area,  k = 10, bs="cs")+ 
                s(Average_5m_slope, by= Depth_mean_5_MAN,  k = 10, bs="cs") ,
              family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #504

#even with backwards selection this model comes out on top
GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Std_Dev_Slope, k = 10, bs="cs")+ 
                s(Cumulative_LG_DZ_Area,  k = 10, bs="cs")+ 
                s(Average_5m_slope,   k = 10, bs="cs") ,
              family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #500

GAMdzsnr<-gam(TotAbunNoH ~ offset(LVol) + 
                s(NASC_10_1m, k = 10, bs="cs")+
                s(Cumulative_LG_DZ_Area,  k = 10, bs="cs")+ 
                s(Average_5m_slope,   k = 10, bs="cs") ,
              family = negbin(theta = 1, link = "log"),  data = TF, method="REML")
summary(GAMdzsnr)
AIC(GAMdzsnr) #513


##remove outliers and re-try backwards selection with interactions and linear terms
str(TF)

TFt<- TF%>% 
  filter(Site_ID !="NS31", Site_ID != "NS41")

