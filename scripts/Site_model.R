# Darienne Lancaster
#May 7, 2024
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
lp("AED")
lp("broom")
lp("mosaic")
lp("car")
lp("performance")
install.packages("GGally")
library(GGally)

#load site dataframe (this needs to be updated with missing sites once reexported)
load("wdata/site_DE.RData")
str(site_DE)

site_DE<- site_DE%>% 
  filter(Site_ID !="NS44") # waiting for updated file from Hutton

####looking at basic linear relationships between different NASC values####

NASC1m<-ggplot(site_DE, aes(x=NASC_10_1m, y=TotalAbundance))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)
 # geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASC1m
correlation_coefficient <- cor(site_DE$NASC_10_1m, site_DE$TotalAbundance)
correlation_coefficient

####- seems like 1m DZ with 10m Rockfish Zone NASC is highly correlated (0.56)#####
#but only when outliers (site 44 and 46 are removed - look into why these NASC values are so high)
#need to double check this when the rest of the sites are added in
#may want to remove all sites with  very large NASC as outliers once I've looked into this
NASC1m10<-ggplot(site_DE, aes(x=NASC_10_1m, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
 #geom_smooth(method="auto")+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASC1m10
correlation_coefficient <- cor(site_DEtest$NASC_10_1m, site_DEtest$AbundanceNonSchooling)
correlation_coefficient

#exploration with outlier NASC values included and non-linear smoother applied
NASC1m10<-ggplot(site_DE, aes(x=NASC_10_1m, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="auto")+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASC1m10
#could be an interesting non-linear relationship with large NASC values indicating sidelobe interference?? look into this
#may want to keep these extreme values and then do a GAM analysis allowing for non-linearity

NASC1m5<-ggplot(site_DEtest, aes(x=NASC_5_1m, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASC1m5
correlation_coefficient <- cor(site_DEtest$NASC_5_1m, site_DEtest$AbundanceNonSchooling)
correlation_coefficient

#manual deadzone is slightly correlated but less so than 1m
NASCMAN<-ggplot(site_DEtest, aes(x=NASC_15_MAN, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASCMAN
correlation_coefficient <- cor(site_DEtest$NASC_15_MAN, site_DEtest$AbundanceNonSchooling)
correlation_coefficient

NASCMAN10<-ggplot(site_DE, aes(x=NASC_10_MAN, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="auto")+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASCMAN10
correlation_coefficient <- cor(site_DEtest$NASC_10_MAN, site_DEtest$AbundanceNonSchooling)
correlation_coefficient

NASCMAN5<-ggplot(site_DEtest, aes(x=NASC_5_MAN, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASCMAN5
correlation_coefficient <- cor(site_DEtest$NASC_5_MAN, site_DEtest$AbundanceNonSchooling)
correlation_coefficient
### most NASC values are zero for the LG deadzone files so won't use these for modelling
NASCLG<-ggplot(site_DEtest, aes(x=NASC_15LG, y=AbundanceNonSchooling))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASCLG
correlation_coefficient <- cor(site_DEtest$NASC_15LG, site_DEtest$AbundanceNonSchooling)
correlation_coefficient

####using abundandce non schooling, large deadzone area, NASC10m_1mDZ (most correlated with benthic fish so far)####
#create smaller dataframe

BbyV<- site_DE%>% 
  dplyr::select(c(Site_ID,TotalAbundance, AbundanceNonSchooling, BenthicbyVol, TotBenbyVol, AllFishbyVol, TotPelbyVol, Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                 NASC_10_1m, Depth_mean_5_MAN, Volume))
str(BbyV)

#remove sites that still need NASC exporting some outliers (NS31 (need .ecs file), NS33 (hit kelp so large values),
#NS44 (missed a dense school with ROV), NS46 (massive deadzone)) 
# BbyV<- BbyV%>% 
#   filter(Site_ID !="NS20",Site_ID !="NS44",Site_ID !="NS46")

NASC<-ggplot(BbyV, aes(x=NASC_10_1m))+
  geom_histogram()
NASC

#######First run model with Total Abundance as response and Volume as an offset################

BbyV<- BbyV%>%
  dplyr::select(c(Site_ID, TotalAbundance, AbundanceNonSchooling,Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Depth_mean_5_MAN, Volume))
str(BbyV)
BbyV$TotalAbundance<-as.numeric(BbyV$TotalAbundance)

###################check outliers ###########

explanatory_vars <- names(BbyV)[!names(BbyV) %in% "TotalAbundance"]
explanatory_vars
plots_list <- list()

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("TotalAbundance  vs", var)
  plot <- ggplot(BbyV, aes(x = !!sym(var), y = TotalAbundance)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)


####site - normality check with histograms################
##
all_vars <- names(BbyV)[!names(BbyV) %in% "Site_ID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(BbyV, aes(x = !!sym(var),)) +
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

site_explan<-BbyV%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

####can also check colinearity with variance inflation factors####
model <- lm(TotalAbundance ~ ., data = site_explan)
vif(model)
#remove Ratio (highly collinear)
site_explan2<-site_explan%>%
  dplyr::select(-c( Ratio))
str(site_explan2)

model2 <- lm(TotalAbundance ~ ., data = site_explan2)
vif(model2)

#none of the other variables have a VIF above 3 which is a typical cut off

#create pairs plot with pearsons correlation coefficeint and smoother on dotplot

ggpairs(site_explan2,
        lower = list(continuous = "smooth"))

#cumulativearea (e.g. deadzone size) and Std deviation of slope/Ratio (E.g. rugosity) are highly collinear with average slope
#(REMOVE Both rugosity metrics now as deadzone size seems to get at this as well and is less collinear with slope)

BbyV<-BbyV%>%
  dplyr::select(-c(Ratio))

site_explan<-BbyV%>%
  dplyr::select(-c( Site_ID))
str(site_explan)


ggpairs(site_explan,
        lower = list(continuous = "smooth"))



#depth and average slope/deadzone area are still correlated (but I feel like they're all important - look into impacts of collinearity in GLM)
#keep slope and depth for now

#########lets standardize our variables and try again####

BbyVstd<-BbyV%>%
  mutate(Average_5m_slope_s=scale(Average_5m_slope),
         Std_Dev_Slope_s=scale(Std_Dev_Slope),
         Cumulative_LG_DZ_Area_s=scale(Cumulative_LG_DZ_Area),
         NASC_10_1m_s=scale(NASC_10_1m), 
         Depth_mean_5_MAN_s=scale(Depth_mean_5_MAN),
         LVol=log(Volume),)%>%
  dplyr::select(-c(3:9)) #remove non-standardized columns

site_explan<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

###################standardized vars check outliers ###########

explanatory_vars <- names(BbyVstd)[!names(BbyVstd) %in% "TotalAbundance"]
explanatory_vars
plots_list <- list()
str(BbyVstd)

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("TotalAbundance  vs", var)
  plot <- ggplot(BbyVstd, aes(x = !!sym(var), y = TotalAbundance)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

#NS21 seems to be an outlier for abundancenonschooling (could remove)
#NS44 and 31 have large NASC values compared to the others (could transform or bin values)

####standardized site - normality check with histograms################
##
all_vars <- names(BbyVstd)[!names(BbyVstd) %in% "Site_ID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(BbyVstd, aes(x = !!sym(var),)) +
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
BbyVstd <- BbyVstd %>%
  mutate_at(vars(3:7), as.numeric)

str(BbyVstd)

site_explan<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

ggpairs(site_explan,
        lower = list(continuous = "smooth"))

#same collinearity issues with depth but we will continue with modelling
#check variance inflation factors
site_explan2<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan2)

model3 <- lm(TotalAbundance ~ ., data = site_explan2)
vif(model3)
#doesn't look like an issue with VIF values


####check residuals####
####site - create linear model for each explanatory variable vs abundance non schooling fish (response variable)####
# then print out fitted vs residuals, histogram of residuals, and qqplot for each model

##remove other predictors at this point (fish schools, total schooling fish, may use volume later as offset)

BbyV_s<-BbyVstd%>%
  dplyr::select(-c(Site_ID))
str(BbyV_s)

explanatory_vars <- names(BbyV_s)[!names(BbyV_s) %in% "TotalAbundance"]
explanatory_vars

for (var in explanatory_vars) {
  # Create the linear model
  lm_model <- lm(formula(paste("TotalAbundance ~", var)), data = BbyV_s)
  
  # Print the summary of the linear model
  cat("\nLinear Model for", var, "vs TotalAbundance", ":\n")
  print(summary(lm_model))
  
  # Plot residuals vs fitted values
  plot_resid_fitted <- plot(lm_model, which = 1) #which = 1 gives you resid vs fitted plot
  cat("\nResiduals vs Fitted Values Plot for", var, "vs TotalAbundance", ":\n")
  print(plot_resid_fitted)
  
  # Add plot title
  title(main = paste("Residuals vs Fitted Values Plot for", var, "vs TotalAbundance"))
  
  # Create a histogram of residuals - looking for no pattern in plot and evenly distributed around zero line
  residuals_hist <- hist(residuals(lm_model), main = paste("Histogram of Residuals for", var, "vs TotalAbundancel"))
  
  # Print the histogram - look for normal distribution shape
  print(residuals_hist)
  
  # Plot qqplot of residuals - looking for straight line (parabolic line suggests skewedness, s shaped line suggest heavy tails on distribution, all non straight patterns suggest non-normal)
  plot_QQ <- plot(lm_model, which = 2) #which = 2 gives you qq plot
  cat("\n QQplot of Residuals Plot for", var, "vs TotalAbundance", ":\n")
  print(plot_QQ)
  
  # Add plot title
  title(main = paste("QQplot of Residuals Plot for", var, "vs TotalAbundance"))
}

#histogram of NASC vs BenthicbyVol looks pretty good but all resid vs fitted plots seem to show patterns indicating non-linearity
#qq plots indicate a lot of skewedness and heavy tails. 

str(BbyV_s)
BbyV_s$Std_Dev_Slope_s
####attempt neg binomial GLM with standardized data - I know data is overdispersed####

#include volume as offset and interactions between colinear terms

M1 <- glm(TotalAbundance ~ offset(LVol) + Average_5m_slope_s + Std_Dev_Slope_s +
            Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
            NASC_10_1m_s:Cumulative_LG_DZ_Area_s,
          family = negbin(theta = 1, link = "log"),  data = BbyV_s)
summary(M1)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#removed many interactions until the model would converge, finally converged with only one interaction
#now remove least significant term

M2 <- glm(TotalAbundance ~ offset(LVol) + Average_5m_slope_s +
            Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
            NASC_10_1m_s:Cumulative_LG_DZ_Area_s,
          family = negbin(theta = 1, link = "log"),  data = BbyV_s)
summary(M2)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M2)$null.deviance-(M2)$deviance)/(M2)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#now remove least significant term

M3 <- glm(TotalAbundance ~ offset(LVol) +
            Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
            NASC_10_1m_s:Cumulative_LG_DZ_Area_s,
          family = negbin(theta = 1, link = "log"),  data = BbyV_s)
summary(M3)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M3)$null.deviance-(M3)$deviance)/(M3)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#now remove least significant term

M4 <- glm(TotalAbundance ~ offset(LVol) +
             NASC_10_1m_s + Depth_mean_5_MAN_s,
          family = negative.binomial(1),  data = BbyV_s)
summary(M4)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M4)$null.deviance-(M4)$deviance)/(M4)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

## M3 looks like the best model NASC is significant and 16% of deviance is explained (low but not nothing, I suspect non-linear trend going on here)

#lets check quality of model

res<- residuals(M3) #residuals

plot(BbyV_s$NASC_10_1m_s, res, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, res, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, res, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$TotalAbundance, res, xlab = "Total Abundance", ylab="residuals")

pres<- residuals(M3, type = "pearson") # pearson residuals

plot(BbyV_s$NASC_10_1m_s, pres, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, pres, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, pres, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$TotalAbundance, pres, xlab = "Total Abundance", ylab="residuals")

pres_s <- residuals(M3, type = "pearson") / sqrt(1 - hatvalues(M1)) #scaled pearson residuals

plot(BbyV_s$NASC_10_1m_s, pres_s, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, pres_s, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, pres_s, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$TotalAbundance, pres_s, xlab = "Total Abundance", ylab="residuals")

dres<- residuals(M3, type = "deviance") # deviance residuals

plot(BbyV_s$NASC_10_1m_s, dres, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, dres, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, dres, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$TotalAbundance, dres, xlab = "Total Abundance", ylab="residuals")

# Generate new data points for prediction
new_data <- data.frame(
  NASC_10_1m_s = seq(from = min(BbyV_s$NASC_10_1m_s), to = max(BbyV_s$NASC_10_1m_s), length.out = 100),
  #Average_5m_slope_s = median(BbyV_s$Average_5m_slope_s),  # You can change this to any value
  Cumulative_LG_DZ_Area_s = median(BbyV_s$Cumulative_LG_DZ_Area_s),
  Depth_mean_5_MAN_s = median(BbyV_s$Depth_mean_5_MAN_s),   # You can change this to any value
  LVol = median(BbyV_s$LVol)   # You can change this to any value
)


# Predict the response variable and its standard errors
predictions <- predict(M3, newdata = new_data, type = "link", se.fit = TRUE)

# Transform predictions to the response scale
F <- exp(predictions$fit)
FSEUP <- exp(predictions$fit + 1.96 * predictions$se.fit)
FSELOW <- exp(predictions$fit - 1.96 * predictions$se.fit)

# Plot the predicted values against NASC_10_1m_s with 95% confidence bands
plot(new_data$NASC_10_1m_s, F, type = "l", xlab = "NASC_10_1m_s", ylab = "Predicted AbundanceNonSchooling", main = "Predicted values from Negative Binomial regression")
lines(new_data$NASC_10_1m_s, FSEUP, lty = 2) 
lines(new_data$NASC_10_1m_s, FSELOW, lty = 2)

# Add scatterplot points for NASC_10_1m_s vs AbundanceNonSchooling
points(BbyV_s$NASC_10_1m_s, BbyV_s$TotalAbundance, col = "red", pch = 16)

#check for patterns in residuals
EP<-resid(M3, type = "pearson")
ED<-resid(M3, type = "deviance")
mu<-predict(M3, type = "response")
E<-BbyVstd$TotalAbundance-mu
EP2 <- E / sqrt(7.630148 * mu)
op <- par(mfrow = c(2, 2))
plot(x=mu, y=E, main = "Response residuals")
plot(x = mu, y = EP, main = "Pearson residuals")
plot(x = mu, y = EP2, main = "Pearson residuals scaled")
plot(x = mu, y = ED, main = "Deviance residuals")
par(op)

###check overdispersion with chi-squared test
chisq_test <- chisq.test(BbyVstd$TotalAbundance)
print(chisq_test)

check_overdispersion(M1)


####################rerun steps using TotBens in negbinom model###########
#create smaller dataframe

BbyV<- site_DE%>% 
  dplyr::select(c(Site_ID,TotalAbundance, TotBen, AbundanceNonSchooling, BenthicbyVol, TotBenbyVol, AllFishbyVol, TotPelbyVol, Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Depth_mean_5_MAN, Volume))
str(BbyV)


#######Run model with TotBen as response and Volume as an offset################

BbyV<- BbyV%>%
  dplyr::select(c(Site_ID, TotBen,Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Depth_mean_5_MAN, Volume))
str(BbyV)
BbyV$TotBen<-as.numeric(BbyV$TotBen)

###################check outliers ###########

explanatory_vars <- names(BbyV)[!names(BbyV) %in% "TotBen"]
explanatory_vars
plots_list <- list()

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("TotBen  vs", var)
  plot <- ggplot(BbyV, aes(x = !!sym(var), y = TotBen)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)


####site - normality check with histograms################
##
all_vars <- names(BbyV)[!names(BbyV) %in% "Site_ID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(BbyV, aes(x = !!sym(var),)) +
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

site_explan<-BbyV%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

####can also check colinearity with variance inflation factors####
model <- lm(TotBen ~ ., data = site_explan)
vif(model)
#remove Ratio (highly collinear)
site_explan2<-site_explan%>%
  dplyr::select(-c( Ratio))
str(site_explan2)

model2 <- lm(TotBen ~ ., data = site_explan2)
vif(model2)

#none of the other variables have a VIF above 3 which is a typical cut off

#create pairs plot with pearsons correlation coefficeint and smoother on dotplot

ggpairs(site_explan2,
        lower = list(continuous = "smooth"))

#cumulativearea (e.g. deadzone size) and Std deviation of slope/Ratio (E.g. rugosity) are highly collinear with average slope
#(REMOVE Both rugosity metrics now as deadzone size seems to get at this as well and is less collinear with slope)

BbyV<-BbyV%>%
  dplyr::select(-c(Ratio))

site_explan<-BbyV%>%
  dplyr::select(-c( Site_ID))
str(site_explan)


ggpairs(site_explan,
        lower = list(continuous = "smooth"))



#depth and average slope/deadzone area are still correlated (but I feel like they're all important - look into impacts of collinearity in GLM)
#keep slope and depth for now

#########lets standardize our variables and try again####

BbyVstd<-BbyV%>%
  mutate(Average_5m_slope_s=scale(Average_5m_slope),
         Std_Dev_Slope_s=scale(Std_Dev_Slope),
         Cumulative_LG_DZ_Area_s=scale(Cumulative_LG_DZ_Area),
         NASC_10_1m_s=scale(NASC_10_1m), 
         Depth_mean_5_MAN_s=scale(Depth_mean_5_MAN),
         LVol=log(Volume),)%>%
  dplyr::select(-c(3:8)) #remove non-standardized columns

site_explan<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

###################standardized vars check outliers ###########

explanatory_vars <- names(BbyVstd)[!names(BbyVstd) %in% "TotBen"]
explanatory_vars
plots_list <- list()
str(BbyVstd)

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("TotBen  vs", var)
  plot <- ggplot(BbyVstd, aes(x = !!sym(var), y = TotBen)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)


####standardized site - normality check with histograms################
##
all_vars <- names(BbyVstd)[!names(BbyVstd) %in% "Site_ID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(BbyVstd, aes(x = !!sym(var),)) +
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
BbyVstd <- BbyVstd %>%
  mutate_at(vars(3:7), as.numeric)

str(BbyVstd)

site_explan<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

ggpairs(site_explan,
        lower = list(continuous = "smooth"))

#same collinearity issues with depth but we will continue with modelling
#check variance inflation factors
site_explan2<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan2)

model3 <- lm(TotBen ~ ., data = site_explan2)
vif(model3)
#doesn't look like an issue with VIF values


####check residuals####
####site - create linear model for each explanatory variable vs abundance non schooling fish (response variable)####
# then print out fitted vs residuals, histogram of residuals, and qqplot for each model

##remove other predictors at this point (fish schools, total schooling fish, may use volume later as offset)

BbyV_s<-BbyVstd%>%
  dplyr::select(-c(Site_ID))
str(BbyV_s)

explanatory_vars <- names(BbyV_s)[!names(BbyV_s) %in% "TotBen"]
explanatory_vars

for (var in explanatory_vars) {
  # Create the linear model
  lm_model <- lm(formula(paste("TotBen ~", var)), data = BbyV_s)
  
  # Print the summary of the linear model
  cat("\nLinear Model for", var, "vs TotBen", ":\n")
  print(summary(lm_model))
  
  # Plot residuals vs fitted values
  plot_resid_fitted <- plot(lm_model, which = 1) #which = 1 gives you resid vs fitted plot
  cat("\nResiduals vs Fitted Values Plot for", var, "vs TotBen", ":\n")
  print(plot_resid_fitted)
  
  # Add plot title
  title(main = paste("Residuals vs Fitted Values Plot for", var, "vs TotBen"))
  
  # Create a histogram of residuals - looking for no pattern in plot and evenly distributed around zero line
  residuals_hist <- hist(residuals(lm_model), main = paste("Histogram of Residuals for", var, "vs TotBen"))
  
  # Print the histogram - look for normal distribution shape
  print(residuals_hist)
  
  # Plot qqplot of residuals - looking for straight line (parabolic line suggests skewedness, s shaped line suggest heavy tails on distribution, all non straight patterns suggest non-normal)
  plot_QQ <- plot(lm_model, which = 2) #which = 2 gives you qq plot
  cat("\n QQplot of Residuals Plot for", var, "vs TotBen", ":\n")
  print(plot_QQ)
  
  # Add plot title
  title(main = paste("QQplot of Residuals Plot for", var, "vs TotBen"))
}

#histogram of NASC vs BenthicbyVol looks pretty good but all resid vs fitted plots seem to show patterns indicating non-linearity
#qq plots indicate a lot of skewedness and heavy tails. 

str(BbyV_s)
####attempt neg binomial GLM with standardized data - I know data is overdispersed####

#include volume as offset and interactions between colinear terms
#poisson model is overdispersed

M1 <- glm(TotBen ~ offset(LVol) +
             NASC_10_1m_s + Depth_mean_5_MAN_s ,
          family = negative.binomial(1),  data = BbyV_s)
summary(M1)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#removed many interactions until the model would converge, finally converged with only one interaction
#now remove least significant term

M3 <- glm(TotBen ~ offset(LVol) +
            NASC_10_1m_s ,
          family = negative.binomial(1),  data = BbyV_s)
summary(M3)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M3)$null.deviance-(M3)$deviance)/(M3)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

## M3 looks like the best model NASC is significant and 16% of deviance is explained (low but not nothing, I suspect non-linear trend going on here)

#lets check quality of model

res<- residuals(M3) #residuals

plot(BbyV_s$NASC_10_1m_s, res, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, res, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, res, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$TotBen, res, xlab = "Total Abundance", ylab="residuals")

pres<- residuals(M3, type = "pearson") # pearson residuals

plot(BbyV_s$NASC_10_1m_s, pres, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, pres, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, pres, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$TotBen, pres, xlab = "Total Abundance", ylab="residuals")

pres_s <- residuals(M3, type = "pearson") / sqrt(1 - hatvalues(M1)) #scaled pearson residuals

plot(BbyV_s$NASC_10_1m_s, pres_s, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, pres_s, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, pres_s, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$TotBen, pres_s, xlab = "Total Abundance", ylab="residuals")

dres<- residuals(M3, type = "deviance") # deviance residuals

plot(BbyV_s$NASC_10_1m_s, dres, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, dres, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, dres, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$TotBen, dres, xlab = "Total Abundance", ylab="residuals")

# Generate new data points for prediction
new_data <- data.frame(
  NASC_10_1m_s = seq(from = min(BbyV_s$NASC_10_1m_s), to = max(BbyV_s$NASC_10_1m_s), length.out = 10),
  LVol = seq(from = min(BbyV_s$LVol), to = max(BbyV_s$LVol), length.out = 1)   # You can change this to any value
)

# Predict the response variable and its standard errors
predictions <- predict(M3, newdata = new_data, type = "link", se.fit = TRUE)

# Transform predictions to the response scale
F <- exp(predictions$fit)
FSEUP <- exp(predictions$fit + 1.96 * predictions$se.fit)
FSELOW <- exp(predictions$fit - 1.96 * predictions$se.fit)

# Plot the predicted values against NASC_10_1m_s with 95% confidence bands
plot(new_data$NASC_10_1m_s, F, type = "l", xlab = "NASC_10_1m_s", ylab = "Predicted TotBen", main = "Predicted values from Negative Binomial regression")
lines(new_data$NASC_10_1m_s, FSEUP, lty = 2) 
lines(new_data$NASC_10_1m_s, FSELOW, lty = 2)

# Add scatterplot points for NASC_10_1m_s vs AbundanceNonSchooling
points(BbyV_s$NASC_10_1m_s, BbyV_s$TotBen, col = "red", pch = 16)

#check for patterns in residuals
EP<-resid(M3, type = "pearson")
ED<-resid(M3, type = "deviance")
mu<-predict(M3, type = "response")
E<-BbyVstd$TotalAbundance-mu
EP2 <- E / sqrt(7.630148 * mu)
op <- par(mfrow = c(2, 2))
plot(x=mu, y=E, main = "Response residuals")
plot(x = mu, y = EP, main = "Pearson residuals")
plot(x = mu, y = EP2, main = "Pearson residuals scaled")
plot(x = mu, y = ED, main = "Deviance residuals")
par(op)

###check overdispersion with chi-squared test
chisq_test <- chisq.test(BbyVstd$TotalAbundance)
print(chisq_test)

check_overdispersion(M3)

####################rerun steps using AbundanceNonSchooling in negbinom model###########
#create smaller dataframe

BbyV<- site_DE%>% 
  dplyr::select(c(Site_ID,TotalAbundance, TotBen, AbundanceNonSchooling, BenthicbyVol, TotBenbyVol, AllFishbyVol, TotPelbyVol, Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Depth_mean_5_MAN, Volume))
str(BbyV)


#######Run model with TotBen as response and Volume as an offset################

BbyV<- BbyV%>%
  dplyr::select(c(Site_ID, AbundanceNonSchooling,Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Depth_mean_5_MAN, Volume))
str(BbyV)
BbyV$AbundanceNonSchooling<-as.numeric(BbyV$AbundanceNonSchooling)

###################check outliers ###########

explanatory_vars <- names(BbyV)[!names(BbyV) %in% "AbundanceNonSchooling"]
explanatory_vars
plots_list <- list()

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("AbundanceNonSchooling  vs", var)
  plot <- ggplot(BbyV, aes(x = !!sym(var), y = AbundanceNonSchooling)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)


####site - normality check with histograms################
##
all_vars <- names(BbyV)[!names(BbyV) %in% "Site_ID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(BbyV, aes(x = !!sym(var),)) +
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

site_explan<-BbyV%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

####can also check colinearity with variance inflation factors####
model <- lm(TotBen ~ ., data = site_explan)
vif(model)
#remove Ratio (highly collinear)
site_explan2<-site_explan%>%
  dplyr::select(-c( Ratio))
str(site_explan2)

model2 <- lm(TotBen ~ ., data = site_explan2)
vif(model2)

#none of the other variables have a VIF above 3 which is a typical cut off

#create pairs plot with pearsons correlation coefficeint and smoother on dotplot

ggpairs(site_explan2,
        lower = list(continuous = "smooth"))

#cumulativearea (e.g. deadzone size) and Std deviation of slope/Ratio (E.g. rugosity) are highly collinear with average slope
#(REMOVE Both rugosity metrics now as deadzone size seems to get at this as well and is less collinear with slope)

BbyV<-BbyV%>%
  dplyr::select(-c(Ratio))

site_explan<-BbyV%>%
  dplyr::select(-c( Site_ID))
str(site_explan)


ggpairs(site_explan,
        lower = list(continuous = "smooth"))



#depth and average slope/deadzone area are still correlated (but I feel like they're all important - look into impacts of collinearity in GLM)
#keep slope and depth for now

#########lets standardize our variables and try again####

BbyVstd<-BbyV%>%
  mutate(Average_5m_slope_s=scale(Average_5m_slope),
         Std_Dev_Slope_s=scale(Std_Dev_Slope),
         Cumulative_LG_DZ_Area_s=scale(Cumulative_LG_DZ_Area),
         NASC_10_1m_s=scale(NASC_10_1m), 
         Depth_mean_5_MAN_s=scale(Depth_mean_5_MAN),
         LVol=log(Volume),)%>%
  dplyr::select(-c(3:8)) #remove non-standardized columns

site_explan<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

###################standardized vars check outliers ###########

explanatory_vars <- names(BbyVstd)[!names(BbyVstd) %in% "AbundanceNonSchooling"]
explanatory_vars
plots_list <- list()
str(BbyVstd)

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("AbundanceNonSchooling  vs", var)
  plot <- ggplot(BbyVstd, aes(x = !!sym(var), y = AbundanceNonSchooling)) +
    geom_point() +
    geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)


####standardized site - normality check with histograms################
##
all_vars <- names(BbyVstd)[!names(BbyVstd) %in% "Site_ID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(BbyVstd, aes(x = !!sym(var),)) +
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
BbyVstd <- BbyVstd %>%
  mutate_at(vars(3:7), as.numeric)

str(BbyVstd)

site_explan<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

ggpairs(site_explan,
        lower = list(continuous = "smooth"))

#same collinearity issues with depth but we will continue with modelling
#check variance inflation factors
site_explan2<-BbyVstd%>%
  dplyr::select(-c( Site_ID))
str(site_explan2)

model3 <- lm(AbundanceNonSchooling ~ ., data = site_explan2)
vif(model3)
#doesn't look like an issue with VIF values


####check residuals####
####site - create linear model for each explanatory variable vs abundance non schooling fish (response variable)####
# then print out fitted vs residuals, histogram of residuals, and qqplot for each model

##remove other predictors at this point (fish schools, total schooling fish, may use volume later as offset)

BbyV_s<-BbyVstd%>%
  dplyr::select(-c(Site_ID))
str(BbyV_s)

explanatory_vars <- names(BbyV_s)[!names(BbyV_s) %in% "AbundanceNonSchooling"]
explanatory_vars

for (var in explanatory_vars) {
  # Create the linear model
  lm_model <- lm(formula(paste("AbundanceNonSchooling ~", var)), data = BbyV_s)
  
  # Print the summary of the linear model
  cat("\nLinear Model for", var, "vs AbundanceNonSchooling", ":\n")
  print(summary(lm_model))
  
  # Plot residuals vs fitted values
  plot_resid_fitted <- plot(lm_model, which = 1) #which = 1 gives you resid vs fitted plot
  cat("\nResiduals vs Fitted Values Plot for", var, "vs AbundanceNonSchooling", ":\n")
  print(plot_resid_fitted)
  
  # Add plot title
  title(main = paste("Residuals vs Fitted Values Plot for", var, "vs AbundanceNonSchooling"))
  
  # Create a histogram of residuals - looking for no pattern in plot and evenly distributed around zero line
  residuals_hist <- hist(residuals(lm_model), main = paste("Histogram of Residuals for", var, "vs AbundanceNonSchooling"))
  
  # Print the histogram - look for normal distribution shape
  print(residuals_hist)
  
  # Plot qqplot of residuals - looking for straight line (parabolic line suggests skewedness, s shaped line suggest heavy tails on distribution, all non straight patterns suggest non-normal)
  plot_QQ <- plot(lm_model, which = 2) #which = 2 gives you qq plot
  cat("\n QQplot of Residuals Plot for", var, "vs AbundanceNonSchooling", ":\n")
  print(plot_QQ)
  
  # Add plot title
  title(main = paste("QQplot of Residuals Plot for", var, "vs AbundanceNonSchooling"))
}

#histogram of NASC vs BenthicbyVol looks pretty good but all resid vs fitted plots seem to show patterns indicating non-linearity
#qq plots indicate a lot of skewedness and heavy tails. 

str(BbyV_s)
####attempt neg binomial GLM with standardized data - I know data is overdispersed####

#include volume as offset and interactions between colinear terms
#poisson model is overdispersed
# M1 <- glm(AbundanceNonSchooling ~ offset(LVol) + Average_5m_slope_s + Std_Dev_Slope_s +
#             Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
#             Average_5m_slope_s:Cumulative_LG_DZ_Area_s +
#             Average_5m_slope_s:Depth_mean_5_MAN_s +
#             NASC_10_1m_s:Cumulative_LG_DZ_Area_s +
#             Std_Dev_Slope_s:Cumulative_LG_DZ_Area_s +
#             Std_Dev_Slope_s:Average_5m_slope_s,
#           family = poisson,  data = BbyV_s)
# summary(M1)

M3 <- glm(AbundanceNonSchooling ~ offset(LVol) + Average_5m_slope_s + Std_Dev_Slope_s +
            Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
            Average_5m_slope_s:Cumulative_LG_DZ_Area_s +
            Average_5m_slope_s:Depth_mean_5_MAN_s +
            NASC_10_1m_s:Cumulative_LG_DZ_Area_s +
            Std_Dev_Slope_s:Cumulative_LG_DZ_Area_s +
            Std_Dev_Slope_s:Average_5m_slope_s,
          family = negbin(theta = 1, link = "log"),  data = BbyV_s)
summary(M3)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M3)$null.deviance-(M3)$deviance)/(M3)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#now remove least significant term

M1 <- glm(AbundanceNonSchooling ~ offset(LVol) + Average_5m_slope_s  +
            Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
            Average_5m_slope_s:Cumulative_LG_DZ_Area_s +
            Average_5m_slope_s:Depth_mean_5_MAN_s +
            NASC_10_1m_s:Cumulative_LG_DZ_Area_s,
          family = negbin(theta = 1, link = "log"),  data = BbyV_s)
summary(M1)

M1 <- glm(AbundanceNonSchooling ~ offset(LVol) +
            Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Depth_mean_5_MAN_s +
            NASC_10_1m_s:Cumulative_LG_DZ_Area_s,
          family = negbin(theta = 1, link = "log"),  data = BbyV_s)
summary(M1)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

M3 <- glm(TotBen ~ offset(LVol) +
            NASC_10_1m_s ,
          family = negative.binomial(1),  data = BbyV_s)
summary(M3)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M3)$null.deviance-(M3)$deviance)/(M3)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

## M3 looks like the best model NASC is significant and 16% of deviance is explained (low but not nothing, I suspect non-linear trend going on here)

#lets check quality of model

res<- residuals(M3) #residuals

plot(BbyV_s$NASC_10_1m_s, res, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, res, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, res, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$AbundanceNonSchooling, res, xlab = "AbundanceNonSchooling", ylab="residuals")

pres<- residuals(M3, type = "pearson") # pearson residuals

plot(BbyV_s$NASC_10_1m_s, pres, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, pres, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, pres, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$AbundanceNonSchooling, pres, xlab = "AbundanceNonSchooling", ylab="residuals")

pres_s <- residuals(M3, type = "pearson") / sqrt(1 - hatvalues(M1)) #scaled pearson residuals

plot(BbyV_s$NASC_10_1m_s, pres_s, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, pres_s, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, pres_s, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$AbundanceNonSchooling, pres_s, xlab = "AbundanceNonSchooling", ylab="residuals")

dres<- residuals(M3, type = "deviance") # deviance residuals

plot(BbyV_s$NASC_10_1m_s, dres, xlab = "NASC", ylab="residuals")
plot(BbyV_s$Depth_mean_5_MAN_s, dres, xlab = "Depth", ylab="residuals")
plot(BbyV_s$Cumulative_LG_DZ_Area_s, dres, xlab = "Deadzone", ylab="residuals")
plot(BbyV_s$AbundanceNonSchooling, dres, xlab = "AbundanceNonSchooling", ylab="residuals")

# Generate new data points for prediction
new_data <- data.frame(
  NASC_10_1m_s = seq(from = min(BbyV_s$NASC_10_1m_s), to = max(BbyV_s$NASC_10_1m_s), length.out = 100),
  Average_5m_slope_s = median(BbyV_s$Average_5m_slope_s),
  Std_Dev_Slope_s  = median(BbyV_s$Std_Dev_Slope_s),
  Cumulative_LG_DZ_Area_s = median(BbyV_s$Cumulative_LG_DZ_Area_s),
  Depth_mean_5_MAN_s = median(BbyV_s$Depth_mean_5_MAN_s),
  LVol = median(BbyV_s$LVol)   # You can change this to any value
)

# Predict the response variable and its standard errors
predictions <- predict(M3, newdata = new_data, type = "link", se.fit = TRUE)

# Transform predictions to the response scale
F <- exp(predictions$fit)
FSEUP <- exp(predictions$fit + 1.96 * predictions$se.fit)
FSELOW <- exp(predictions$fit - 1.96 * predictions$se.fit)

# Plot the predicted values against NASC_10_1m_s with 95% confidence bands
plot(new_data$NASC_10_1m_s, F, type = "l", xlab = "NASC_10_1m_s", ylab = "Predicted AbundanceNonSchooling", main = "Predicted values from Negative Binomial regression")
lines(new_data$NASC_10_1m_s, FSEUP, lty = 2) 
lines(new_data$NASC_10_1m_s, FSELOW, lty = 2)

# Add scatterplot points for NASC_10_1m_s vs AbundanceNonSchooling
points(BbyV_s$NASC_10_1m_s, BbyV_s$AbundanceNonSchooling, col = "red", pch = 16)

#check for patterns in residuals
EP<-resid(M3, type = "pearson")
ED<-resid(M3, type = "deviance")
mu<-predict(M3, type = "response")
E<-BbyVstd$TotalAbundance-mu
EP2 <- E / sqrt(7.630148 * mu)
op <- par(mfrow = c(2, 2))
plot(x=mu, y=E, main = "Response residuals")
plot(x = mu, y = EP, main = "Pearson residuals")
plot(x = mu, y = EP2, main = "Pearson residuals scaled")
plot(x = mu, y = ED, main = "Deviance residuals")
par(op)

###check overdispersion with chi-squared test
chisq_test <- chisq.test(BbyVstd$TotalAbundance)
print(chisq_test)

check_overdispersion(M1)

# 
# ####attempt poisson GLM with standardized data####
# str(BbyV_s)
# str(BbyV)
# BbyV_t<-BbyV%>%
#   dplyr::select(-c(Site_ID))
# 
# 
# M1<-glm(BenthicbyVol ~ Average_5m_slope_s+ Cumulative_LG_DZ_Area_s +NASC_10_1m_s + Depth_mean_5_MAN_s+
#           Average_5m_slope_s:Cumulative_LG_DZ_Area_s + Average_5m_slope_s:Depth_mean_5_MAN_s,
#         NASC_10_1m_s:Cumulative_LG_DZ_Area_s,data = BbyV_s)
# summary(M1)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# 
# #remove least significant deadzone area
# M2<-glm(BenthicbyVol ~ Cumulative_LG_DZ_Area_s +NASC_10_1m_s + Depth_mean_5_MAN_s,
#          data = BbyV_s)
# summary(M2)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M2)$null.deviance-(M2)$deviance)/(M2)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance

## M2 looks like the best model with all variables significant and 46% of the variance explained

#lets check quality of model

# Generate new data points for prediction
new_data <- data.frame(
  NASC_10_1m_s = seq(from = min(BbyV_s$NASC_10_1m_s), to = max(BbyV_s$NASC_10_1m_s), length.out = 100),
  Average_5m_slope_s = median(BbyV_s$Average_5m_slope_s),  # You can change this to any value
  Cumulative_LG_DZ_Area_s = median(BbyV_s$Cumulative_LG_DZ_Area_s),
  Depth_mean_5_MAN_s = median(BbyV_s$Depth_mean_5_MAN_s)   # You can change this to any value
)

# Predict the response variable and its standard errors
predictions <- predict(M2, newdata = new_data, type = "link", se.fit = TRUE)

# Transform predictions to the response scale
F <- exp(predictions$fit)
FSEUP <- exp(predictions$fit + 1.96 * predictions$se.fit)
FSELOW <- exp(predictions$fit - 1.96 * predictions$se.fit)

# Plot the predicted values against NASC_10_1m_s with 95% confidence bands
plot(new_data$NASC_10_1m_s, F, type = "l", xlab = "NASC_10_1m_s", ylab = "Predicted AbundanceNonSchooling", main = "Predicted values from Poisson regression")
lines(new_data$NASC_10_1m_s, FSEUP, lty = 2) 
lines(new_data$NASC_10_1m_s, FSELOW, lty = 2)

# Add scatterplot points for NASC_10_1m_s vs AbundanceNonSchooling
points(BbyV_s$NASC_10_1m_s, BbyV_s$AbundanceNonSchooling, col = "red", pch = 16)

#check for patterns in residuals
EP<-resid(M2, type = "pearson")
ED<-resid(M2, type = "deviance")
mu<-predict(M2, type = "response")
E<-BbyVstd$AbundanceNonSchooling-mu
EP2 <- E / sqrt(7.630148 * mu)
op <- par(mfrow = c(2, 2))
plot(x=mu, y=E, main = "Response residuals")
plot(x = mu, y = EP, main = "Pearson residuals")
plot(x = mu, y = EP2, main = "Pearson residuals scaled")
plot(x = mu, y = ED, main = "Deviance residuals")
par(op)

###check overdispersion with chi-squared test
chisq_test <- chisq.test(BbyVstd$AbundanceNonSchooling)
print(chisq_test)

check_overdispersion(M2)

####attempt poisson GLM with nonstandardized data####
str(BbyV)
M1ns<-glm(AbundanceNonSchooling ~ Average_5m_slope+ Cumulative_LG_DZ_Area +NASC_10_1m + Depth_mean_5_MAN+
          Average_5m_slope:Cumulative_LG_DZ_Area + Average_5m_slope:Depth_mean_5_MAN,
        family = poisson, data = BbyV)
summary(M1ns)
#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1ns)$null.deviance-(M1ns)$deviance)/(M1ns)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#remove least significant

M2ns<-glm(AbundanceNonSchooling ~ Average_5m_slope+ Cumulative_LG_DZ_Area +NASC_10_1m+
            Average_5m_slope:Cumulative_LG_DZ_Area + Average_5m_slope:Depth_mean_5_MAN,
          family = poisson, data = BbyV)
summary(M2ns)
#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M2ns)$null.deviance-(M2ns)$deviance)/(M2ns)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#remove least significant

M3ns<-glm(AbundanceNonSchooling ~  Cumulative_LG_DZ_Area +NASC_10_1m+
            Average_5m_slope:Cumulative_LG_DZ_Area + Average_5m_slope:Depth_mean_5_MAN,
          family = poisson, data = BbyV)
summary(M3ns)
#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M3ns)$null.deviance-(M3ns)$deviance)/(M3ns)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

#nonstandardized model performs similarly with slightly less significant variables (need to look up if you leave in interaction when you remove predictor)

check_overdispersion(M3ns)
