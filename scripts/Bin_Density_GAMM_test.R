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

#load bin dataframe 
load("wdata/bin_DE.RData")
str(bin_DE)
#create total fish abundance (schooling and benthics) column with herring counts excluded
bin_DE$TotAbunNoH<-bin_DE$total_number_NoHschoolingfish+bin_DE$AbundanceNonSchooling

#convert benthic count to density

bin_DE$Ben<-bin_DE$AbundanceNonSchooling/bin_DE$Volume
bin_DE$BenSQ<-sqrt(bin_DE$Ben)

#remove outlier bins (need to edit these echograms)
bin_DE<-bin_DE%>%
  filter( BinID !="NS31_5")
####looking at basic linear relationships between different NASC values  (NASC_10_1m has highest correlation)####

BvS<-ggplot(bin_DE, aes(x=Average_5m_slope, y=Average_Depth))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = BinID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(bin_DE$NASC_10_1m, bin_DE$BenSQ)
correlation_coefficient

ggplot(bin_DE, aes(x = Average_5m_slope, y = BenSQ, color = Average_Depth)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +  # You can adjust colors as per your preference
  labs(x = "Slope", y = "Ben", color = "Depth")

BvS<-ggplot(bin_DE, aes(x=Average_5m_slope, y=BenSQ))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = BinID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
# geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
BvS
correlation_coefficient <- cor(bin_DE$NASC_10_1m, bin_DE$BenSQ)
correlation_coefficient

coplot(BenSQ ~ NASC_10_1m|Average_5m_slope, data = bin_DE, overlap = FALSE,
       panel = function(x, y, ...) {
         points(x, y, ...)
         abline(lm(y ~ x), col = "blue")})

#strong non-linear pattern in data


#######First run model with density of benthic fish as response################

TF<- bin_DE%>% #TF = TotalFish (no herring)
  dplyr::select(c(Site_ID,BinID, Interval, AbundanceNonSchooling,Average_5m_slope,Std_Dev_Slope, Ratio, Cumulative_LG_DZ_Area, 
                  NASC_10_1m, Average_Depth, Volume))
str(TF)
TF$LVol<-log(TF$Volume)

###################check outliers ###########

explanatory_vars <- names(TF)[!names(TF) %in% "AbundanceNonSchooling"]
explanatory_vars
plots_list <- list()

# Loop through each explanatory variable and create scatterplot

for (var in explanatory_vars) {
  plot_title <- paste("AbundanceNonSchooling  vs", var)
  plot <- ggplot(TF, aes(x = !!sym(var), y = AbundanceNonSchooling)) +
    geom_point() +
    geom_text(aes(label = BinID), nudge_x = 0.1, nudge_y = 0.1, size = 3) +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

#no visible outliers

####bin - normality check with histograms################
##
TF1<-TF%>%
  dplyr::select(-Site_ID, -Interval)

all_vars <- names(TF1)[!names(TF1) %in% "BinID"]
all_vars #check they're all there
plots_list <- list()  #make empty list to store plots
str(TF1)

# Loop through each variable and create histogram

for (var in all_vars) {
  plot_title <- paste(var)
  plot <- ggplot(TF1, aes(x = !!sym(var),)) +
    geom_histogram() +
    labs(title = plot_title)
  plots_list[[length(plots_list) + 1]] <- plot
}

# Combine all plots into one
combined_plots <- wrap_plots(plots_list)

# Print the combined plot
print(combined_plots)

### lots of skew


#####bin - create pair plot with correlation coefficients for all variables####

bin_explan<-TF1%>%
  dplyr::select(-c( BinID))
str(bin_explan)

####can also check colinearity with variance inflation factors####
model <- lm(AbundanceNonSchooling ~ ., data = bin_explan)
vif(model)
#remove Ratio (highly collinear)
bin_explan2<-bin_explan%>%
  dplyr::select(-c( Ratio))
str(bin_explan2)

model2 <- lm(AbundanceNonSchooling ~ ., data = bin_explan2)
vif(model2)

#none of the other variables have a VIF above 3 which is a typical cut off

#create pairs plot with pearsons correlation coefficeint and smoother on dotplot

ggpairs(bin_explan2,
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
         Average_Depth_s=scale(Average_Depth),
         LVol=log(Volume))%>%
  mutate_at(vars(13:18), as.numeric) #remove non-standardized columns
str(TFstd)

bin_explan<-TFstd%>%
  dplyr::select(-c( Site_ID))
str(bin_explan)


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


####standardized bin - normality check with histograms################
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

bin_explan<-TFstd%>%
  dplyr::select(-c( Site_ID))
str(bin_explan)

ggpairs(bin_explan,
        lower = list(continuous = "smooth"))

#same collinearity issues with depth but we will continue with modelling
#check variance inflation factors
bin_explan2<-TFstd%>%
  dplyr::select(-c( Site_ID, Ratio_s))
str(bin_explan2)

model3 <- lm(TotAbunNoH ~ ., data = bin_explan2)
vif(model3)
#Should remove ratio, too colinear (use only one rugosity metric)

#check diagnostic plots####
#NASC
lmNASC <- lm(AbundanceNonSchooling ~ NASC_10_1m, data = TF) #total fish is offset by log transformed sampling volume
lmNASC
summary(lmNASC)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmNASC)

#stdev_slope
lmSTDslope <- lm(AbundanceNonSchooling ~ Std_Dev_Slope , data = TF) #total fish is offset by log transformed sampling volume
lmSTDslope
summary(lmSTDslope)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmSTDslope)

#Ratio_s
lmRatio <- lm(AbundanceNonSchooling ~ Ratio , data = TF) #total fish is offset by log transformed sampling volume
lmRatio
summary(lmRatio)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmRatio)

#Cumulative_LG_DZ_Area_s 
lmDZ <- lm(AbundanceNonSchooling ~ Cumulative_LG_DZ_Area  , data = TF) #total fish is offset by log transformed sampling volume
lmDZ
summary(lmDZ)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmDZ)

#Depth_mean_5_MAN_s
lmD <- lm(AbundanceNonSchooling ~ Depth_mean_5_MAN  , data = TF) #total fish is offset by log transformed sampling volume
lmD
summary(lmD)

# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lmD)

#Rediduals vs Fitted - evidence of non-linearity and heteroscedasticity (over dispersed)
#Scale-Location - No major trends but some patterned variation of residuals around  smoothing line
#QQ - residuals are not normally distributed (heavy tails and skewedness with some outliers)
#residuals vs leverage (cooks distance)  - only large cooks distance looks like NS31 (row 23 - may need to remove)

#what kind of distribution would you use?

#original full model (vars removed by least significant predictor)
# M1 <- glm(Ben ~ Average_5m_slope + Std_Dev_Slope +
#             Cumulative_LG_DZ_Area + NASC_10_1m + Depth_mean_5_MAN, #remove
#           family = gaussian,  data = TF)
# summary(M1)
#turn site ID into factor
TF$Site_ID<-as.factor(TF$Site_ID)
str(TF)

M1 <- glmer(AbundanceNonSchooling ~ offset(LVol) + Average_5m_slope + Std_Dev_Slope +
            Cumulative_LG_DZ_Area + NASC_10_1m + Average_Depth + Interval +
          (1|Site_ID), #remove
          family = poisson,  data = TF)
summary(M1)
AIC(M1)

#remove non significant predictors stepwise

M1 <- glmer(AbundanceNonSchooling ~ offset(LVol)  + Interval +
              NASC_10_1m_s:Cumulative_LG_DZ_Area_s +
              (1|Site_ID), #remove
            family = poisson,  data = TFstd)
summary(M1)
AIC(M1)

#model is overdispersed (try neg.bin)

M1 <- glmer(AbundanceNonSchooling ~ offset(LVol) + Average_5m_slope_s + Std_Dev_Slope_s +
              Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Average_Depth_s + Interval +
              NASC_10_1m_s:Cumulative_LG_DZ_Area_s +
              (1|Site_ID), #remove
            family = negative.binomial(theta=1, link="log"),  data = TFstd)
summary(M1)

M1 <- glmer(AbundanceNonSchooling ~ offset(LVol) + Average_5m_slope_s + Std_Dev_Slope_s +
              Cumulative_LG_DZ_Area_s + NASC_10_1m_s + Average_Depth_s + 
              (1|Site_ID), #remove
            family = negative.binomial(theta=1, link="log"),  data = TFstd)
summary(M1)
AIC(M1)

#remove non significant predictors stepwise

M1 <- glmer(AbundanceNonSchooling ~ offset(LVol) +  
               Interval  +
              (1|Site_ID), #remove
            family = negative.binomial(theta=1, link="log"),  data = TFstd)
summary(M1)
AIC(M1)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(M1)
check_overdispersion(M1)

#major issues in residuals plots (lots of patterns and evidence that data is not normally distributed - try gamma distribution)

#original full model (vars removed by least significant predictor)
# M1 <- glm(Ben ~ Average_5m_slope + Std_Dev_Slope +
#             Cumulative_LG_DZ_Area + NASC_10_1m + Depth_mean_5_MAN, #remove
#           family = gaussian,  data = TF)
# summary(M1)

#add constant to get rid of zero value in Ben column

TF$BenG<-TF$Ben+0.00001

M1 <- glm(BenG ~ 
            NASC_10_1m , #remove
          family = Gamma(link="inverse"),  data = TF)
summary(M1)

#get glm equivalent of R-squared (explained deviance)
explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
#get value for explained deviance (also known as pseudo Rsquared)
explaineddeviance

par(mfrow = c(2, 2))
plot(M1)

### try gam###

#original full model
# M1 <- gam(Ben ~ s(Average_5m_slope) + s(Std_Dev_Slope) +
#             s(Cumulative_LG_DZ_Area) + s(NASC_10_1m) + s(Depth_mean_5_MAN), #remove
#           data = TF)

#if k is not specified it defaults to 10
#if method is not specified it defaults to GCV (generalied cross correlation)

# Setting Gamma to 1.4 to reduce overfitting of GCV.
# A final default choice, that it is worth being aware of, is the value for γ in 
# the GCV or UBRE scores (expressions (4.28) or (4.29), respectively) optimized in
# order to select the degree of smoothness of each term. The default value is 1, 
# but GCV is known to have some tendency to overfitting on occasion, and it has 
# been suggested that using γ ≈ 1.4 can largely correct this without compromising model fit

#####good code for creating factor variable out of continuous var####
# # Define the variable you want to split
# variable_to_split <- TF$NASC_10_1m
# 
# # Determine the breakpoints for splitting the data evenly into three levels
# breakpoints <- quantile(variable_to_split, probs = c(1/3, 2/3))
# 
# # Cut the variable into three factor levels based on the breakpoints
# factor_levels <- cut(variable_to_split, breaks = c(-Inf, breakpoints, Inf), labels = c("small", "medium", "large"))
# 
# # Assign the factor levels back to your dataset
# TF$NASC_10_1m_levels <- factor_levels
# #GAM with NASC as a factor variable
# M1 <- gam(Ben ~  s(Cumulative_LG_DZ_Area) + NASC_10_1m_levels, #remove
#                         data = TF, gamma=1.4)
#####
str(TF)

M1 <- gamm(AbundanceNonSchooling ~offset(LVol) + s(Average_5m_slope) + s(Std_Dev_Slope)+ 
             s(Depth_mean_5_MAN) +
             s(Cumulative_LG_DZ_Area) + s(NASC_10_1m), #remove
          family = quasi(link=log, variance ="mu"), data = TFstd,
          random=list(Site_ID=~1))

M1$gam

summary(M1)

par(mfrow = c(2, 2))
plot(M1, residuals=TRUE)
#this plots pearson residuals against values for covariate (well fitting models should have 
#residuals evenly scattered around smoothing line.  Number in y-axis label is effective degrees of freedom
#for each covariate - Wood (GAMS book))
par(mfrow = c(2, 2))
gam.check(M1)
#linear pattern in response vs fitted plot (based on Woods it doesn't seem like that's an issue)

k.check(M1)

print.gam(M1)

# #try with Gamma distribution
# 
# M1 <- gam(BenG ~  s(Cumulative_LG_DZ_Area) + s(NASC_10_1m), family=Gamma(link = log) , #remove
#           data = TF)
# summary(M1)
# 
# par(mfrow = c(2, 2))
# plot(M1, residuals=TRUE)
# #this plots pearson residuals against values for covariate (well fitting models should have 
# #residuals evenly scattered around smoothing line.  Number in y-axis label is effective degrees of freedom
# #for each covariate - Wood (GAMS book))
# 
# k.check(M1)
# 
# print.gam(M1)


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

