#Run this script before any others
#creates a function to load and install any required packages

#load packages script
lp<-function(pck){
  if(!require(pck,character.only = TRUE))install.packages(pck);library(pck,character.only = TRUE)
}

