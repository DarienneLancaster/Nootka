# Darienne Lancaster
# April 19, 2024
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
install.packages("GGally")
library(GGally)
library(ggplot2)

load("wdata/full_lines.RData")
load("wdata/bin_lines.RData")
load("wdata/binNASC.RData")
load("wdata/siteNASC.RData")
load("wdata/bininfo.RData")
load("wdata/siteinfo.RData")

################### merge the site dataframes############ 
merged_site <- left_join(siteinfo, sitelines, by = "Site_ID")
site_complete <- left_join(merged_site, siteNASC, by = "Site_ID")
site_complete <- site_complete %>% filter(!is.na(TopSub))
#site_complete <- site_complete %>% filter(!is.na(Layer_depth_max))
site_complete <- site_complete %>% mutate(Average_Slope = abs(Average_Slope))
#site_complete <- site_complete %>% mutate(Layer_depth_min = abs(Layer_depth_min))

#remove unwanted sites and make nas 0  
site_complete<- site_complete%>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  filter(Site_ID !="NS01", Site_ID != "NS02", Site_ID != "NS03", Site_ID != "NS04",
         Site_ID != "NS06", Site_ID != "NS08", Site_ID != "NS18")

###### merge the bin dataframes #####
merged_bin <- left_join(bininfo, binlines, by = "BinID")
bin_complete <- left_join(merged_bin, binNASC, by = c("BinID", "Site_ID"))


#remove unwanted sites and make nas 0
bin_complete<- bin_complete%>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  filter(Site_ID !="NS01", Site_ID != "NS02", Site_ID != "NS03", Site_ID != "NS04",
         Site_ID != "NS05",Site_ID != "NS06", Site_ID != "NS08", Site_ID != "NS18")

#remove unwanted sites and make nas 0
site_complete<- site_complete%>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  mutate(Average_Depth = ifelse(Average_Depth == 0, NA, Average_Depth))%>%
  filter(Site_ID !="NS01", Site_ID != "NS02", Site_ID != "NS03", Site_ID != "NS04",
         Site_ID != "NS05", Site_ID != "NS06", Site_ID != "NS08", Site_ID != "NS18")


# 
# #create column for bin number NOTE: THIS ALREADY EXISTS IN DATAFRAME AS INTERVAL
# bin_complete<- bin_complete%>% separate(BinID, into = c("site","bin_num"), sep = "_", remove = FALSE )%>%
#   dplyr::select(-c(site))

####subset dataframe to only include variables for GLMMs or GAMS####

#AbundanceNonSchooling = Response variable

# Explanatory variables are (all derived from Echograms) Average_5m_slope,Std_Dev_Slope (1st Rugosity metric - calculated off bottom line, not 5m slope bins),
# Ratio (2nd rugosity metric calculated using chain length ratio - removed and needs work on zero values and all ratios starting at 1),
# CumulativeArea (Deadzone metric), NASC_15, NASC_10, NASC_5, Average_Depth 
#NOTE: we will need to add in NASC values for each deadzone type (e.g. NASC_5_manualDZ, NASC_5_LargeDZ, NASC_5_1mDZ)
#################bin dataframe##############
str(bin_complete)

#create some new variables for fish abundance
bin_complete$TotPel<-bin_complete$`sum_SF--BP`+bin_complete$`sum_SF--SP` #combine big and small pelagics sum total
bin_complete$TotBen<-bin_complete$`sum_SF--SB`+bin_complete$AbundanceNonSchooling #combine solo benthics with benthic schools

bin_DE<- bin_complete%>% 
  # dplyr::select(c(BinID,bin_num, Site_ID.x,total_number_schoolingfish, number_FS,
  #                 AbundanceNonSchooling,Average_5m_slope,Std_Dev_Slope, CumulativeArea, Sv_mean_15))
  dplyr::select(c(Site_ID,BinID,Interval,TotalAbundance,AbundanceNonSchooling,number_FS, total_number_schoolingfish, 
                  total_number_NoHschoolingfish,TotPel,TotBen,
                  Average_5m_slope,Std_Dev_Slope, Ratio, 
                  Cumulative_LG_DZ_Area, 
                  NASC_5_MAN,NASC_10_MAN,NASC_15_MAN,
                  NASC_5_1m,NASC_10_1m,NASC_15_1m, 
                  NASC_5_LG,NASC_10_LG,NASC_15_LG, 
                  Average_Depth,Volume))
 # dplyr::select(c(AbundanceNonSchooling,Average_5m_slope,Std_Dev_Slope, CumulativeArea, NASC_15, NASC_10, NASC_5, Average_Depth))
bin_DE$Interval<-as.factor(bin_DE$Interval)
bin_DE$TotalAbundance<-as.numeric(bin_DE$TotalAbundance)
bin_DE$TotPel<-as.numeric(bin_DE$TotPel)
bin_DE$TotBen<-as.numeric(bin_DE$TotBen)
bin_DE$total_number_schoolingfish<-as.numeric(bin_DE$total_number_schoolingfish)
bin_DE$total_number_NoHschoolingfish<-as.numeric(bin_DE$total_number_NoHschoolingfish)
bin_DE$number_FS<-as.numeric(bin_DE$number_FS)
bin_DE$AbundanceNonSchooling<-as.numeric(bin_DE$AbundanceNonSchooling)

bin_DE$BenthicbyVol<-bin_DE$AbundanceNonSchooling/bin_DE$Volume
bin_DE$SchoolbyVol<-bin_DE$total_number_schoolingfish/bin_DE$Volume
bin_DE$AllFish<-bin_DE$total_number_schoolingfish+bin_DE$AbundanceNonSchooling #create column with schooling fish and benthics combined (no mud fish)
bin_DE$AllFishbyVol<-bin_DE$AllFish/bin_DE$Volume
bin_DE$TotPelbyVol<-bin_DE$TotPel/bin_DE$Volume
bin_DE$TotBenbyVol<-bin_DE$TotBen/bin_DE$Volume
str(bin_DE)

NASCLG<-ggplot(bin_DE, aes(x=NASC_10_1m, y=AllFishbyVol))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  geom_text(aes(label = Site_ID), nudge_x = 0.1, nudge_y = 0.1, size = 3) 
NASCLG
correlation_coefficient <- cor(bin_DE$NASC_10_1m, bin_DE$AllFishbyVol)
correlation_coefficient

str(bin_DE)
save(bin_DE, file = "wdata/bin_DE.RData")
##################site dataframe###################
#do the same for site
str(site_complete)

#create some new variables for fish abundance
site_complete$TotPel<-site_complete$`sum_SF--BP`+site_complete$`sum_SF--SP` #combine big and small pelagics sum total
site_complete$TotBen<-site_complete$`sum_SF--SB`+site_complete$AbundanceNonSchooling #combine solo benthics with benthic schools

site_DE<- site_complete%>% 
  # dplyr::select(c(BinID,bin_num, Site_ID.x,total_number_schoolingfish, number_FS,
  #                 AbundanceNonSchooling,Average_5m_slope,Std_Dev_Slope, CumulativeArea, Sv_mean_15))
  #dplyr::select(c(AbundanceNonSchooling,total_number_schoolingfish, number_FS,Average_5m_slope,Std_Dev_Slope, CumulativeArea, NASC_15, NASC_10, NASC_5, Average_Depth))
  dplyr::select(c(Site_ID,TotalAbundance,AbundanceNonSchooling,number_FS, total_number_schoolingfish, 
                  total_number_NoHschoolingfish,TotPel,TotBen, RFSpeciesRichness,SpeciesRichness,
                  Average_5m_slope,Std_Dev_Slope, Ratio, 
                  Cumulative_LG_DZ_Area, 
                  NASC_5_MAN,NASC_10_MAN,NASC_15_MAN,
                  NASC_5_1m,NASC_10_1m,NASC_15_1m, 
                  NASC_5_LG,NASC_10_LG,NASC_15LG, Average_Depth,Volume,
                  TopSub, SubPercent,TopSlope, SlopePercent))


#### need to factor in volume surveyed by ROV FOV (prelim linear model tests seem to show stronger correlations using volume adjusted values than non adjusted)
site_DE$BenthicbyVol<-site_DE$AbundanceNonSchooling/site_DE$Volume
site_DE$SchoolbyVol<-site_DE$total_number_schoolingfish/site_DE$Volume
site_DE$AllFish<-site_DE$total_number_schoolingfish+site_DE$AbundanceNonSchooling #create column with schooling fish and benthics combined (no mud fish)
site_DE$AllFishbyVol<-site_DE$AllFish/site_DE$Volume
site_DE$TotPelbyVol<-site_DE$TotPel/site_DE$Volume
site_DE$TotBenbyVol<-site_DE$TotBen/site_DE$Volume

#make all vars numeric
site_DE<- site_DE%>%
  mutate_at(vars(2:25,27:31), as.numeric)

str(site_DE)

mean(site_DE$BenthicbyVol)
var(site_DE$BenthicbyVol)

mean(site_DE$AbundanceNonSchooling)
var(site_DE$AbundanceNonSchooling)

str(site_DE)
save(site_DE, file = "wdata/site_DE.RData")

#move to script 9_Site_GAM.R



######preliminary investigation - ignore code beyond this point#####
# ##Check for outliers bin#
# 
# explanatory_vars <- names(bin_DE)[!names(bin_DE) %in% "AbundanceNonSchooling"]
# explanatory_vars
# plots_list <- list()
# 
# # Loop through each explanatory variable and create scatterplot
# 
# for (var in explanatory_vars) {
#   plot_title <- paste("AbundanceNonSchooling vs", var)
#   plot <- ggplot(bin_DE, aes(x = !!sym(var), y = AbundanceNonSchooling)) +
#     geom_point() +
#     labs(title = plot_title)
#   plots_list[[length(plots_list) + 1]] <- plot
# }
# 
# # Combine all plots into one
# combined_plots <- wrap_plots(plots_list)
# 
# # Print the combined plot
# print(combined_plots)
# 
# #no major outliers I can see except for in NASC plots but those will change with new data exports.
# #major concerns around homogeneity (spread of data values is not the same for all variables), lots of issues to do with zero values.
# #this means basic linear modelling will not be appropriate as expected fromt these data.
# 
# ###################check outliers for sites###########
# 
# 
# explanatory_vars <- names(site_DE)[!names(site_DE) %in% "AbundanceNonSchooling"]
# explanatory_vars
# plots_list <- list()
# 
# # Loop through each explanatory variable and create scatterplot
# 
# for (var in explanatory_vars) {
#   plot_title <- paste("AbundanceNonSchooling  vs", var)
#   plot <- ggplot(site_DE, aes(x = !!sym(var), y = AbundanceNonSchooling )) +
#     geom_point() +
#     labs(title = plot_title)
#   plots_list[[length(plots_list) + 1]] <- plot
# }
# 
# # Combine all plots into one
# combined_plots <- wrap_plots(plots_list)
# 
# # Print the combined plot
# print(combined_plots)
# 
# #####bin - create pair plot with correlation coefficients for all variables###################################################################
# 
# #remove response variable
# bin_explan<-bin_DE%>%
#   select(-AbundanceNonSchooling)
# 
# #create pairs plot with pearsons correlation coefficeint and smoother on dotplot
# ggpairs(bin_explan,
#         lower = list(continuous = "smooth"))
# 
# #A decent amount of colinearity between predictor variables (which is expected given the nature of these vars,
# # will need to investigate if this is an issue for GAMs and GLMMs (do we need to use only one NASC value at a time in our models?))
# # lots of colinearity between average slope, ave depth, rugosity, and NASC vars.
# 
# #####site - create pair plot with correlation coefficients for all variables####
# 
# str(site_DE)
# site_explan<-site_DE%>%
#   dplyr::select(-AbundanceNonSchooling)
# 
# #create pairs plot with pearsons correlation coefficeint and smoother on dotplot
# 
# ggpairs(site_DE,
#         lower = list(continuous = "smooth"))
# 
# ####bin - check for normality in variables ####
# 
# 
# all_vars <- names(bin_DE) #create list of all variable names  
# all_vars #check they're all there
# plots_list <- list()  #make empty list to store plots
# 
# # Loop through each variable and create histogram
# 
# for (var in all_vars) {
#   plot_title <- paste(var)
#   plot <- ggplot(bin_DE, aes(x = !!sym(var),)) +
#     geom_histogram() +
#     labs(title = plot_title)
#   plots_list[[length(plots_list) + 1]] <- plot
# }
# 
# # Combine all plots into one
# combined_plots <- wrap_plots(plots_list)
# 
# # Print the combined plot
# print(combined_plots)
# 
# ####site - normality check for site################
# ##
# 
# all_vars <- names(site_DE) #create list of all variable names  
# all_vars #check they're all there
# plots_list <- list()  #make empty list to store plots
# 
# # Loop through each variable and create histogram
# 
# for (var in all_vars) {
#   plot_title <- paste(var)
#   plot <- ggplot(site_DE, aes(x = !!sym(var),)) +
#     geom_histogram() +
#     labs(title = plot_title)
#   plots_list[[length(plots_list) + 1]] <- plot
# }
# 
# # Combine all plots into one
# combined_plots <- wrap_plots(plots_list)
# 
# # Print the combined plot
# print(combined_plots)
# 
# 
# #obviously most of the variables are non normal. Need to fit lm and plot residuals vs fitted as well to double check (I know it's not going to be linear
# # but this is a good excercise)
# 
# ####bin - create linear model for each explanatory variable vs abundance non schooling fish (response variable)####
# # then print out fitted vs residuals, histogram of residuals, and qqplot for each model
# 
# explanatory_vars <- names(bin_DE)[!names(bin_DE) %in% "AbundanceNonSchooling"]
# explanatory_vars
# 
# for (var in explanatory_vars) {
#   # Create the linear model
#   lm_model <- lm(formula(paste("AbundanceNonSchooling ~", var)), data = bin_DE)
#   
#   # Print the summary of the linear model
#   cat("\nLinear Model for", var, "vs AbundanceNonSchooling", ":\n")
#   print(summary(lm_model))
# 
#   # Plot residuals vs fitted values
#   plot_resid_fitted <- plot(lm_model, which = 1) #which = 1 gives you resid vs fitted plot
#   cat("\nResiduals vs Fitted Values Plot for", var, "vs AbundanceNonSchooling", ":\n")
#   print(plot_resid_fitted)
#   
#   # Add plot title
#   title(main = paste("Residuals vs Fitted Values Plot for", var, "vs AbundanceNonSchooling"))
#   
#   # Create a histogram of residuals - looking for no pattern in plot and evenly distributed around zero line
#   residuals_hist <- hist(residuals(lm_model), main = paste("Histogram of Residuals for", var, "vs AbundanceNonSchooling"))
#   
#   # Print the histogram - look for normal distribution shape
#   print(residuals_hist)
#   
#   # Plot qqplot of residuals - looking for straight line (parabolic line suggests skewedness, s shaped line suggest heavy tails on distribution, all non straight patterns suggest non-normal)
#   plot_QQ <- plot(lm_model, which = 2) #which = 2 gives you qq plot
#   cat("\n QQplot of Residuals Plot for", var, "vs AbundanceNonSchooling", ":\n")
#   print(plot_resid_hist)
#   
#   # Add plot title
#   title(main = paste("QQplot of Residuals Plot for", var, "vs AbundanceNonSchooling"))
# }
# 
# ####site - create linear model for each explanatory variable vs abundance non schooling fish (response variable)####
# # then print out fitted vs residuals, histogram of residuals, and qqplot for each model
# 
# explanatory_vars <- names(site_DE)[!names(site_DE) %in% "AbundanceNonSchooling"]
# explanatory_vars
# 
# for (var in explanatory_vars) {
#   # Create the linear model
#   lm_model <- lm(formula(paste("AbundanceNonSchooling ~", var)), data = site_DE)
#   
#   # Print the summary of the linear model
#   cat("\nLinear Model for", var, "vs AbundanceNonSchooling", ":\n")
#   print(summary(lm_model))
#   
#   # Plot residuals vs fitted values
#   plot_resid_fitted <- plot(lm_model, which = 1) #which = 1 gives you resid vs fitted plot
#   cat("\nResiduals vs Fitted Values Plot for", var, "vs AbundanceNonSchooling", ":\n")
#   print(plot_resid_fitted)
#   
#   # Add plot title
#   title(main = paste("Residuals vs Fitted Values Plot for", var, "vs AbundanceNonSchooling"))
#   
#   # Create a histogram of residuals - looking for no pattern in plot and evenly distributed around zero line
#   residuals_hist <- hist(residuals(lm_model), main = paste("Histogram of Residuals for", var, "vs AbundanceNonSchooling"))
#   
#   # Print the histogram - look for normal distribution shape
#   print(residuals_hist)
#   
#   # Plot qqplot of residuals - looking for straight line (parabolic line suggests skewedness, s shaped line suggest heavy tails on distribution, all non straight patterns suggest non-normal)
#   plot_QQ <- plot(lm_model, which = 2) #which = 2 gives you qq plot
#   cat("\n QQplot of Residuals Plot for", var, "vs AbundanceNonSchooling", ":\n")
#   print(plot_resid_hist)
#   
#   # Add plot title
#   title(main = paste("QQplot of Residuals Plot for", var, "vs AbundanceNonSchooling"))
# }
# 
# #site plots have less issues caused by lots of zeros that are causing problems for bin analysis.  Still highly non-linear overall
# # and mostly very skewed data. 
# 
# #April 29, 2024
# #Next steps:
# #wait for new NASC data from manual deadzone and 1m deadzone
# #check for outliers in new dataset
# #determine how to bin NASC data, if at all
# #determine which vars to include in model at once (can we only include one NASC region at a time?)
# #how do I determine which distribution to use and if the binned data is zero inflated?
# 
# ###############################################################################################
# #can start playing around with GLMMs and GAMS with ROV dataframe if curious and want to try code
# #for this use (# benthic fish ~ # fish schools + total schooling fish (will be colinear with #fish schools)
# #              + depth (pull from benthic fish entries??) + substrate (Hard/Soft) + slope)
# # for binned data include site as random effect (likely don't need it for site data?)
# # do I need to consider spatial correlation?  How do we include this?  Site coordinates (can R tell how close they are based on these?)
# 
# 
# 
# ##################playing with ROV dataframe in mixed effects model############
# 
# #create ROV dataframe
# site_ROV<- site_complete%>% 
#   dplyr::select(c(AbundanceNonSchooling,total_number_schoolingfish,number_FS, TopSub, TopSlope, avg_depth))
# 
# site_ROV$total_number_schoolingfish<-as.numeric(site_ROV$total_number_schoolingfish)
# site_ROV$number_FS<-as.numeric(site_ROV$number_FS)
# site_ROV$TopSub<-as.factor(site_ROV$TopSub)
# site_ROV$TopSlope<-as.factor(site_ROV$TopSlope)
# #site_ROV$BenRF_Vol<-site_ROV$AbundanceNonSchooling/site_ROV$Volume
# 
# 
# str(site_ROV)
# 
# ####Check for outliers####
# 
# explanatory_vars <- names(site_ROV)[!names(site_ROV) %in% "AbundanceNonSchooling"]
# explanatory_vars
# plots_list <- list()
# 
# # Loop through each explanatory variable and create scatterplot
# 
# for (var in explanatory_vars) {
#   plot_title <- paste("AbundanceNonSchooling vs", var)
#   plot <- ggplot(site_ROV, aes(x = !!sym(var), y = AbundanceNonSchooling)) +
#     geom_point() +
#     labs(title = plot_title)
#   plots_list[[length(plots_list) + 1]] <- plot
# }
# 
# # Combine all plots into one
# combined_plots <- wrap_plots(plots_list)
# 
# # Print the combined plot
# print(combined_plots)
# 
# #remove NS12 with 40 fish outlier?  Can do later if needed
# #also remove site NS08 with 3300 total schooling fish?
# 
# ####create pair plot with correlation coefficients for all variables####
# #remove response variable
# site_pred<-site_ROV%>%
#   select(-AbundanceNonSchooling)
# 
# #check for significance between nonschooling and number fish schools (significant)
# m1<- lm(AbundanceNonSchooling~number_FS, data = site_ROV)
# summary(m1)
# 
# #create pairs plot with pearsons correlation coefficeint and smoother on dotplot
# #a bit strange with factor variables but still shows collinearity between number FS and total number schooling fish
# ggpairs(site_pred,
#         lower = list(continuous = "smooth"))
# 
# ####check for normality in continuous variables ####
# 
# site_ROVcon<- site_ROV%>% 
#   dplyr::select(c(AbundanceNonSchooling,total_number_schoolingfish,number_FS, avg_depth))
# 
# all_vars <- names(site_ROVcon) #create list of all variable names  
# all_vars #check they're all there
# plots_list <- list()  #make empty list to store plots
# 
# # Loop through each variable and create histogram
# 
# for (var in all_vars) {
#   plot_title <- paste(var)
#   plot <- ggplot(site_ROVcon, aes(x = !!sym(var),)) +
#     geom_histogram() +
#     labs(title = plot_title)
#   plots_list[[length(plots_list) + 1]] <- plot
# }
# 
# # Combine all plots into one
# combined_plots <- wrap_plots(plots_list)
# 
# # Print the combined plot
# print(combined_plots)
# 
# #definitely look a bit skewed
# 
# 
# ####create linear model for each explanatory variable vs abundance non schooling fish (response variable)
# # then print out fitted vs residuals, histogram of residuals, and qqplot for each model
# 
# explanatory_vars <- names(site_ROVcon)[!names(site_ROVcon) %in% "AbundanceNonSchooling"]
# explanatory_vars
# 
# for (var in explanatory_vars) {
#   # Create the linear model
#   lm_model <- lm(formula(paste("AbundanceNonSchooling ~", var)), data = site_ROVcon)
#   
#   # Print the summary of the linear model
#   cat("\nLinear Model for", var, "vs AbundanceNonSchooling", ":\n")
#   print(summary(lm_model))
#   
#   # Plot residuals vs fitted values
#   plot_resid_fitted <- plot(lm_model, which = 1) #which = 1 gives you resid vs fitted plot
#   cat("\nResiduals vs Fitted Values Plot for", var, "vs AbundanceNonSchooling", ":\n")
#   print(plot_resid_fitted)
#   
#   # Add plot title
#   title(main = paste("Residuals vs Fitted Values Plot for", var, "vs AbundanceNonSchooling"))
#   
#   # Create a histogram of residuals - looking for no pattern in plot and evenly distributed around zero line
#   residuals_hist <- hist(residuals(lm_model), main = paste("Histogram of Residuals for", var, "vs AbundanceNonSchooling"))
#   
#   # Print the histogram - look for normal distribution shape
#   print(residuals_hist)
#   
#   # Plot qqplot of residuals - looking for straight line (parabolic line suggests skewedness, s shaped line suggest heavy tails on distribution, all non straight patterns suggest non-normal)
#   plot_QQ <- plot(lm_model, which = 2) #which = 2 gives you qq plot
#   cat("\n QQplot of Residuals Plot for", var, "vs AbundanceNonSchooling", ":\n")
#   print(plot_resid_hist)
#   
#   # Add plot title
#   title(main = paste("QQplot of Residuals Plot for", var, "vs AbundanceNonSchooling"))
# }
# 
# #lets play with a linear model even though we'll likely need a mixed effects model with  poisson or neg binomial distribution
# 
# M.lm<- lm(AbundanceNonSchooling~number_FS+ avg_depth +TopSub + TopSlope, data=site_ROV)
# plot(M.lm, select=c(1)) # plot of residuals vs fitted is showing characteristic cone shape associated with heterogenetity (too much spread in data, will need a different model
# #but lets keep testing for fun)
# summary(M.lm)
# 
# 
# #remove slope as least significant
# M.lm2<- lm(AbundanceNonSchooling~number_FS+ avg_depth +TopSub, data=site_ROV)
# plot(M.lm2, select=c(1))
# summary(M.lm2)
# 
# #remove depth as not significant
# M.lm3<- lm(AbundanceNonSchooling~number_FS +TopSub, data=site_ROV)
# plot(M.lm3, select=c(1)) #much less cone shaped now but still a bit of heterogenety
# summary(M.lm3)
# #two variables are actually less significant without depth and explains less variation (r-squared)
# 
# #lets try with just number of FS
# #remove depth as not significant
# M.lm4<- lm(AbundanceNonSchooling~number_FS, data=site_ROV)
# plot(M.lm4, select=c(1)) #much less cone shaped now but still a bit of heterogenety
# summary(M.lm4)
# 
# #number of fish schools is a significant predictor of benthic fish abundance at the 0.02 level.
# #however model only explains 11% of variance, best variance explained is with depth, substrate, and number of FS
# #I also ran it with total number schooling fish as predictor and it was not significant. 
# 
# #lets play with a linear model for acoustic dataframe even though we'll likely need a mixed effects model with  poisson or neg binomial distribution
# str(bin_DE)
# 
# M.lm<- lm(AbundanceNonSchooling~number_FS+ CumulativeArea + Average_Depth, data=bin_DE)
# plot(M.lm, select=c(1)) # plot of residuals vs fitted is showing characteristic cone shape associated with heterogenetity (too much spread in data, will need a different model
# #but lets keep testing for fun)
# summary(M.lm)
# #number_FS is significant but R squared is pathetic lol
# 
# 
# #remove slope as least significant
# M.lm2<- lm(AbundanceNonSchooling~number_FS+ CumulativeArea, data=bin_DE)
# plot(M.lm2, select=c(1))
# summary(M.lm2)
# 
# #remove depth as not significant
# M.lm3<- lm(AbundanceNonSchooling~number_FS, data=bin_DE)
# plot(M.lm3, select=c(1)) #much less cone shaped now but still a bit of heterogenety
# summary(M.lm3)
# #two variables are actually less significant without depth and explains less variation (r-squared)
# 
# ####site dataframe###
# #lets play with a linear model for acoustic dataframe even though we'll likely need a mixed effects model with  poisson or neg binomial distribution
# str(site_DE)
# 
# M.lm<- lm(BenthicbyVol~SchoolbyVol + CumulativeArea + Average_Depth, data=site_DE)
# plot(M.lm, select=c(1)) # plot of residuals vs fitted is showing characteristic cone shape associated with heterogenetity (too much spread in data, will need a different model
# #but lets keep testing for fun)
# summary(M.lm)
# #number_FS is significant but R squared is pathetic lol
# 
# 
# #remove slope as least significant
# M.lm2<- lm(BenthicbyVol~SchoolbyVol + Average_Depth, data=site_DE)
# plot(M.lm2, select=c(1))
# summary(M.lm2)
# 
# #remove depth as not significant
# M.lm3<- lm(BenthicbyVol~SchoolbyVol, data=site_DE)
# plot(M.lm3, select=c(1)) #much less cone shaped now but still a bit of heterogenety
# summary(M.lm3)
# #two variables are actually less significant without depth and explains less variation (r-squared)
# str(site_DE)
# 
# #playing with glm with poisson distribution with data
# M1<- glm(AbundanceNonSchooling~total_number_schoolingfish, family = poisson, data = site_DE)
# summary(M1)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M1)$null.deviance-(M1)$deviance)/(M1)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# 
# plot(site_DE$total_number_schoolingfish, site_DE$AbundanceNonSchooling, xlab = "# schooling fish", ylab = "benthic fish")
# 
# #plot predicted values from model
# MyData <- data.frame(total_number_schoolingfish = seq(from = 0, to = 3500, by = 500))
# G<-predict(M1, newdata= MyData, type = "link", se=TRUE)
# F<-exp(G$fit)
# FSEUP<-exp(G$fit+1.96*G$se.fit)
# FSELOW<-exp(G$fit-1.96*G$se.fit)
# lines(MyData$total_number_schoolingfish,F, lty=1)
# lines(MyData$total_number_schoolingfish, FSEUP, lty = 2) 
# lines(MyData$total_number_schoolingfish, FSELOW, lty = 2)
# 
# str(site_DE)
# #add more variables to poisson model
# M2<- glm(AbundanceNonSchooling~number_FS+Average_5m_slope+CumulativeArea+Std_Dev_Slope+Average_Depth, family = poisson, data = site_DE)
# summary(M2)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M2)$null.deviance-(M2)$deviance)/(M2)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# drop1(M2, test = "Chi")
# 
# #remove least significant var from drop 1 summary = cumulative area)
# M3<- glm(AbundanceNonSchooling~number_FS+Average_5m_slope+Std_Dev_Slope+Average_Depth, family = poisson, data = site_DE)
# summary(M3)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M3)$null.deviance-(M3)$deviance)/(M3)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# drop1(M3, test = "Chi")
# 
# #remove least significant var from drop 1 summary = std SLope)
# M4<- glm(AbundanceNonSchooling~number_FS+Average_5m_slope+Average_Depth, family = poisson, data = site_DE)
# summary(M4)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M4)$null.deviance-(M4)$deviance)/(M4)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# drop1(M4, test = "Chi")
# 
# #seems like model 3 is the best but it still has some non-siginifcant vars (but explains 17% of deviance
# #and both depth and fish schools is significant)
# 
# plot(site_DE$total_number_schoolingfish, site_DE$AbundanceNonSchooling, xlab = "# schooling fish", ylab = "benthic fish")
# 
# #plot predicted values from model
# MyData <- data.frame(total_number_schoolingfish = seq(from = 0, to = 3500, by = 500))
# G<-predict(M1, newdata= MyData, type = "link", se=TRUE)
# F<-exp(G$fit)
# FSEUP<-exp(G$fit+1.96*G$se.fit)
# FSELOW<-exp(G$fit-1.96*G$se.fit)
# lines(MyData$total_number_schoolingfish,F, lty=1)
# lines(MyData$total_number_schoolingfish, FSEUP, lty = 2) 
# lines(MyData$total_number_schoolingfish, FSELOW, lty = 2)
# 
# #try using negative binomial model instead of poisson
# M6<- glm.nb(AbundanceNonSchooling~number_FS+Average_5m_slope+CumulativeArea+Std_Dev_Slope+Average_Depth, link="log", data = site_DE)
# summary(M6, cor=FALSE)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M6)$null.deviance-(M6)$deviance)/(M6)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# drop1(M6, test = "Chi")
# 
# M7<- glm.nb(AbundanceNonSchooling~number_FS+Average_5m_slope+Std_Dev_Slope+Average_Depth, link="log", data = site_DE)
# summary(M7, cor=FALSE)
# 
# M8<- glm.nb(AbundanceNonSchooling~number_FS+Average_5m_slope+Average_Depth, link="log", data = site_DE)
# summary(M8, cor=FALSE)
# 
# M9<- glm.nb(AbundanceNonSchooling~number_FS+Average_5m_slope, link="log", data = site_DE)
# summary(M9, cor=FALSE)
# 
# M10<- glm.nb(AbundanceNonSchooling~number_FS, link="log", data = site_DE)
# summary(M10, cor=FALSE)
# drop1(M10, test = "Chi")
# explaineddeviance<- 100*(((M10)$null.deviance-(M10)$deviance)/(M10)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# op <- par(mfrow = c(2, 2)) 
# plot(M10) 
# par(op)
# 
# #try using making an offset for sampled water volume and adding to nb glm (pg. 241 Zuur)
# 
# site_DE$LVol<-log(site_DE$Volume)
# 
# M11<- glm.nb(AbundanceNonSchooling~offset(LVol) +number_FS+Average_5m_slope+CumulativeArea+Std_Dev_Slope+Average_Depth, link="log", data = site_DE)
# summary(M11, cor=FALSE)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M11)$null.deviance-(M11)$deviance)/(M11)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# drop1(M11, test = "Chi")
# 
# #try using making an offset for sampled water volume and adding to nb glm (pg. 241 Zuur)
# 
# site_DE$LVol<-log(site_DE$Volume)
# 
# M11<- glm.nb(AbundanceNonSchooling~offset(LVol)+number_FS+Average_5m_slope+Average_Depth
#              +number_FS:Average_5m_slope +number_FS:Average_Depth
#              +Average_Depth:Average_5m_slope, link="log", data = site_DE)
# summary(M11, cor=FALSE)
# #get glm equivalent of R-squared (explained deviance)
# explaineddeviance<- 100*(((M11)$null.deviance-(M11)$deviance)/(M11)$null.deviance)
# #get value for explained deviance (also known as pseudo Rsquared)
# explaineddeviance
# drop1(M11, test = "Chi")
# 
# #try a gam
# 
# M11<- gam(AbundanceNonSchooling~offset(LVol)+number_FS+CumulativeArea+s(Average_Depth),
#           family=poisson, data = site_DE)
# summary(M11, cor=FALSE)
# plot(M11)
# #this seems to make number FS and cumulative area highly significant. Need to remember
# #how to select which vars get smoothing terms (go back to early chapters on additive modelling
# 
# #try a zero inflated mixture model for binned data
# str(bin_DE)
# 
# plot(table(bin_DE$AbundanceNonSchooling), ylab = "Frequencies", xlab = "Observed benthics")
# 
# Zip1<- zeroinfl(AbundanceNonSchooling ~ number_FS+CumulativeArea+Average_Depth|number_FS+CumulativeArea+Average_Depth,
#                 dist = "poisson", link = "logit", data = bin_DE)
# summary(Zip1)
# 
# #compare to NB model
# Nb1 <- zeroinfl(AbundanceNonSchooling ~ number_FS+CumulativeArea+Average_Depth|number_FS+CumulativeArea+Average_Depth,
#                   dist = "negbin", link = "logit", data = bin_DE)
# lrtest(Zip1,Nb1) #apply likelihood ratio test for variance (pg 281 Zuur)
# summary(Nb1)
# #drop depth - this causes some kind of error to do with linear dependencies or colinearity?  Works with total number schooling fish though
# Nb2 <- zeroinfl(AbundanceNonSchooling ~ total_number_schoolingfish|total_number_schoolingfish,
#                 dist = "negbin", link = "logit", data = bin_DE)
# summary(Nb2)
# cor(bin_DE[, c("number_FS", "CumulativeArea")])
# 
# EP <- residuals(Zip1, type = "pearson")
# 
# 
# ###############################################################################
# #walk through full data exploration and modelling with binned data
# 
# ####Check for outliers####
# 
# explanatory_vars <- names(bin_DE)[!names(bin_DE) %in% "AbundanceNonSchooling"]
# explanatory_vars
# plots_list <- list()
# 
# # Loop through each explanatory variable and create scatterplot
# 
# for (var in explanatory_vars) {
#   plot_title <- paste("AbundanceNonSchooling vs", var)
#   plot <- ggplot(bin_DE, aes(x = !!sym(var), y = AbundanceNonSchooling)) +
#     geom_point() +
#     labs(title = plot_title)
#   plots_list[[length(plots_list) + 1]] <- plot
# }
# 
# # Combine all plots into one
# combined_plots <- wrap_plots(plots_list)
# 
# # Print the combined plot
# print(combined_plots)
# 
# #no major outliers I can see except for in NASC plots but those will change with new data exports.
# #major concerns around homogeneity (spread of data values is not the same for all variables), lots of issues to do with zero values.
# #this means basic linear modelling will not be appropriate as expected fromt these data.
# 
# 
# ########################################################################
# ####create pair plot with correlation coefficients for all variables####
# #remove response variable
# bin_explan<-bin_DE%>%
#   dplyr::select(-AbundanceNonSchooling, -Site_ID.x, -bin_num)
# 
# #create pairs plot with pearsons correlation coefficeint and smoother on dotplot
# ggpairs(bin_explan,
#         lower = list(continuous = "smooth"))
# 
# #high correlation between all slope related values (remove Ratio, Std dev slope, and rerun)
# #for now don't worry about other collinearity as will only include one NASC value at a time
# 
# bin_explan2<-bin_DE%>%
#   dplyr::select(-AbundanceNonSchooling, -Site_ID.x, -bin_num, -Std_Dev_Slope, -Ratio)
# 
# #create pairs plot with pearsons correlation coefficeint and smoother on dotplot
# ggpairs(bin_explan2,
#         lower = list(continuous = "smooth"))
# #slope and DZ area are still correlated at .508 which isn't terrible but may cause issues in a GAM (likely only want to include one of these)
# #look at Zuur Appendix A for VIF values for determining colinearity
# 
# ####check for normality in variables ####
# 
# bin_cont<-bin_DE%>%
#   dplyr::select(-Site_ID.x, -bin_num)
# all_vars <- names(bin_cont) #create list of all variable names  
# all_vars #check they're all there
# plots_list <- list()  #make empty list to store plots
# 
# # Loop through each variable and create histogram
# 
# for (var in all_vars) {
#   plot_title <- paste(var)
#   plot <- ggplot(bin_cont, aes(x = !!sym(var),)) +
#     geom_histogram() +
#     labs(title = plot_title)
#   plots_list[[length(plots_list) + 1]] <- plot
# }
# 
# # Combine all plots into one
# combined_plots <- wrap_plots(plots_list)
# 
# # Print the combined plot
# print(combined_plots)
# 
# #main predictor variable (abundancenonschooling) is not normally distributed.  
# #ditrubution looks like Poisson or more likely negative binomial as variance is probably more than mean (lets check)
# #poisson needs variance to approximately equal the mean, NB allows for greater variance
# 
# mean(bin_DE$AbundanceNonSchooling)
# #2.005 mean
# var(bin_DE$AbundanceNonSchooling)
# #8.74 variance - much larger than mean so need to go negative binomial
# 
# ####try fitting glmm####
# 
# #need random effect for site as each site is binned into 5 and likely related to each other
# 
# #Variables of interest
# # 1. response variable is abundance non schooling (eg. benthic fish)
# # 2. predictors for this round are only from acoustic variables
# #    - Slope or Deadzone area are likely important predictors (pick one and then try the other, can also try rugosity metrics out of curiosity)
# #    - Depth is important predictor
# #    - NASC_5 important (most important, try with each rockfish depth zone)
# #    - Need to include spatial correlation in model as well
# #  NOTE: may need to look at transforming some predictors (check if overall trend stays the same after transformation)
# #  NOTE: may need to bin NASC (look at values and see if there are clear patterns in the way NASC values parse out)
# #Possible interaction terms:
# #         Biologically an interaction between NASC and depth is likely 
#           #prediction: NASC increases as depth decreases, possible decline at very shallow depths
# #         Interaction between Depth and Slope is likely 
#           #prediction: Slope is lower at extreme range of depths (biologically this is likely to be flatter, sandier areas in fjord habitats)
#           #Slope will increase as depth decreases with a plateau in shallow habitats
# #         Interaction between NASC and Slope is likely
#           #prediction: NASC will increase with slope to a certain point and then after ~40-50 degree slopes NASC will decrease (likely due to detection effect)
# #response variable predictions match NASC interaction term predictions as I expect NASC to be positively correlated with benthic fish abundance
# #may need offset for volume of water sampled by video (need to add volume column to binned data)
# 
# #full glmm model
# bin_DE$fSite<-factor(bin_DE$Site_ID.x)#make Site a factor
# str(bin_DE)
# binfull<- glmmPQL(AbundanceNonSchooling~Average_5m_slope+Average_Depth+NASC_5+
#                     Average_5m_slope*Average_Depth+Average_5m_slope*NASC_5 + Average_Depth*NASC_5,
#                   random = ~1|fSite, family = neg.bin(theta = 1), data=bin_DE)
# summary(binfull)
# par(mfrow = c(2, 2))
# plot(binfull)
# 
# binfuP<- glmer(AbundanceNonSchooling~Average_5m_slope+Average_Depth+
#                    (1|fSite), family = poisson, data=bin_DE)
# summary(binfuP)
# binfuNB<- glmer.nb(AbundanceNonSchooling~Average_5m_slope+Average_Depth+
#                     (1|fSite),  data=bin_DE)
# summary(binfuNB)
# par(mfrow = c(2, 2))
# plot(binfuNB)
# 
# binfull<- gamm(AbundanceNonSchooling~s(Average_5m_slope)+s(Average_Depth)+NASC_5,
#                random = list(fSite =~1), family= neg.bin(theta = 1), data=bin_DE)
# summary(binfull$gam, cor=FALSE)
# anova(binfull$gam)
# plot(binfull$gam)
# summary(binfull$lme)
# 
# 
# 
