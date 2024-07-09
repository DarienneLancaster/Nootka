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

#convert benthic count to density

site_DE$Ben<-site_DE$AbundanceNonSchooling/site_DE$Volume

#convert total fish (no herring to density)

site_DE$TFD<-site_DE$TotAbunNoH/site_DE$Volume

site_DE$TFDsq<- sqrt(site_DE$TFD)

#remove outlier sites (need to edit these echograms)
site_DE<-site_DE%>%
  filter(Site_ID!="NS41", Site_ID !="NS31")
####looking at basic linear relationships between different NASC values  (NASC_10_1m has highest correlation)####

BvS<-ggplot(site_DE, aes(x=NASC_10_1m, y=number_FS))+
  geom_point()+
  geom_smooth(method="lm", se=TRUE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(site_DE$NASC_10_1m, site_DE$TFDsq)
correlation_coefficient



#######First run model with density of total fish (no herring) as response################

TF<- site_DE%>% #TF = TotalFish (no herring)
  dplyr::select(c(Site_ID,TFD, TFDsq,Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Depth_mean_5_MAN))
str(TF)

###################check outliers ###########

explanatory_vars <- names(TF)[!names(TF) %in% "TFDsq"]
explanatory_vars
plots_list <- list()

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("TFDsq  vs", var)
  plot <- ggplot(TF, aes(x = !!sym(var), y = TFDsq)) +
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


#####site - create pair plot with correlation coefficients for all variables####

site_explan<-TF%>%
  dplyr::select(-c( Site_ID))
str(site_explan)

####can also check colinearity with variance inflation factors####
model <- lm(TFDsq ~ ., data = site_explan)
vif(model)
#remove Ratio (highly collinear)
site_explan2<-site_explan%>%
  dplyr::select(-c( Ratio))
str(site_explan2)

model2 <- lm(TFDsq ~ ., data = site_explan2)
vif(model2)

#deadzone area is just over 3, we will leave it in for now

#create pairs plot with pearsons correlation coefficeint and smoother on dotplot

ggpairs(site_explan2,
        lower = list(continuous = "smooth"))

#cumulativearea (e.g. deadzone size) and Std deviation of slope/Ratio (E.g. rugosity) are highly collinear with average slope
#(Do not use rugosity metrics in model as deadzone size seems to get at this as well and is less collinear with slope)
str(TF)
######Standardize vars######
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
lmNASC <- lm(TFDsq ~ NASC_10_1m, data = TF) #total fish is offset by log transformed sampling volume
lmNASC
summary(lmNASC)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmNASC)

#stdev_slope
lmSTDslope <- lm(TFDsq ~ Std_Dev_Slope , data = TF) #total fish is offset by log transformed sampling volume
lmSTDslope
summary(lmSTDslope)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmSTDslope)

#Ratio_s
lmRatio <- lm(TFDsq ~ Ratio , data = TF) #total fish is offset by log transformed sampling volume
lmRatio
summary(lmRatio)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmRatio)

#Cumulative_LG_DZ_Area_s 
lmDZ <- lm(TFDsq ~ Cumulative_LG_DZ_Area  , data = TF) #total fish is offset by log transformed sampling volume
lmDZ
summary(lmDZ)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmDZ)

#Depth_mean_5_MAN_s
lmD <- lm(TFDsq ~ Depth_mean_5_MAN  , data = TF) #total fish is offset by log transformed sampling volume
lmD
summary(lmD)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmD)

#Rediduals vs Fitted - evidence of non-linearity and heteroscedasticity (over dispersed)
#Scale-Location - No major trends but some patterned variation of residuals around  smoothing line
#QQ - residuals are not normally distributed (heavy tails and skewedness with some outliers)
#residuals vs leverage (cooks distance)  - only large cooks distance looks like NS31 (row 23 - may need to remove)

#try gaussian glm

#original full model (vars removed by least significant predictor)
# M1 <- glm(TFDsq ~ Average_5m_slope + Std_Dev_Slope +
#             Cumulative_LG_DZ_Area + NASC_10_1m + Depth_mean_5_MAN, #remove
#           family = gaussian,  data = TF)
# summary(M1)

M1 <- glm(TFDsq ~ 
            Cumulative_LG_DZ_Area + NASC_10_1m , #remove
          family = gaussian,  data = TF)
summary(M1)
AIC(M1)


#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(M1)

#major issues in residuals plots (lots of patterns and evidence that data is not normally distributed - try gamma distribution)

#original full model (vars removed by least significant predictor)
# M1 <- glm(TFDsqG ~ Average_5m_slope + Std_Dev_Slope +
#             Cumulative_LG_DZ_Area + NASC_10_1m + Depth_mean_5_MAN, #remove
#           family = Gamma(link="inverse"),  data = TF)
# summary(M1)

#add constant to get rid of zero value in Ben column

TF$TFDsqG<-TF$TFDsq+0.00001

M1 <- glm(TFDsqG ~ Average_5m_slope + 
            Cumulative_LG_DZ_Area + NASC_10_1m , #remove
          family = Gamma(link="inverse"),  data = TF)
summary(M1)
AIC(M1)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(M1)

### try gaussian gam###

#original full model
# M1 <- gam(Ben ~ s(Average_5m_slope) + s(Std_Dev_Slope) +
#             s(Cumulative_LG_DZ_Area) + s(NASC_10_1m) + s(Depth_mean_5_MAN), #remove
#           data = TF)

M1 <- gam(TFDsq ~ 
            s(Cumulative_LG_DZ_Area) + s(NASC_10_1m) , #remove
          data = TF)

summary(M1)
AIC(M1)

par(mfrow = c(2, 2))
plot(M1)

k.check(M1)


# Extract residuals
residuals <- residuals(M1)

# Calculate leverage manually
X <- model.matrix(~ s(Cumulative_LG_DZ_Area) + s(NASC_10_1m), data = TF)
leverage <- rowSums(X * solve(t(X) %*% X) %*% t(X))

# Create diagnostic plots manually
par(mfrow = c(2, 2))  # Set up a 2x2 grid for plots

# 1. Residuals vs Fitted
plot(fitted(M1), residuals, xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")

# 2. QQ Plot of Residuals
qqnorm(residuals)
qqline(residuals)

# 3. Scale-Location Plot
plot(sqrt(abs(residuals)) ~ fitted(M1), xlab = "Fitted values", ylab = "Square root of |Residuals|",
     main = "Scale-Location Plot")

# 4. Residuals vs Leverage
plot(leverage, residuals, xlab = "Leverage", ylab = "Residuals",
     main = "Residuals vs Leverage")


par(mfrow = c(1, 1))
M3Pred <- predict(M1, se = TRUE, type = "response") 
plot(TF$NASC_10_1m, TF$Ben, cex = 1.1, pch = 16, main = "Gaussian GAM", 
     xlab = "NASC", ylab = "Total Benthic Fish") 
I <- order(TF$NASC_10_1m)
lines(TF$NASC_10_1m[I], M3Pred$fit[I], lwd = 2)
lines(TF$NASC_10_1m[I], M3Pred$fit[I] + 2 * M3Pred$se.fit[I], lty = 2, lwd = 2)
lines(TF$NASC_10_1m[I], M3Pred$fit[I] - 2 * M3Pred$se.fit[I], lty = 2, lwd = 2)
for (i in 1:37) {
  y <- rnorm(100, mean = M3Pred$fit[i], sd = 0.5)
  points(rep(TF$NASC_10_1m[i], 100), y, cex = 0.5)
}





# Plot the  diagnostic plots of smoothers 
par(mfrow = c(2, 2))
plot(M1)

##plot pearson residuals vs explanatory variables
par(mfrow = c(2, 2))
# Extract Pearson residuals
residuals <- residuals(M1, type = "pearson")

#Plot residuals vs. explanatory variables with a smoothing spline

smoothed_line1 <- smooth.spline(TF$NASC_10_1m, residuals, spar = 1.25)
smoothed_line4 <- smooth.spline(TF$Cumulative_LG_DZ_Area, residuals, spar = 1)


# Create the first scatter plot with LOESS smoother
plot(TF$NASC_10_1m, residuals, xlab = "NASC_10_1m_s", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs NASC_10_1m_s")
lines(smoothed_line1, col = "red")



# Create the fourth scatter plot with LOESS smoother
plot(TF$Cumulative_LG_DZ_Area, residuals, xlab = "Deadzone", ylab = "Pearson Residuals", 
     main = "Pearson Residuals vs Deadzone")
lines(smoothed_line4, col = "red")
