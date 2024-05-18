
###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function for easier library loading
suppressPackageStartupMessages(
  libraries("data.table",
            "dplyr",
            "Boruta", # for Boruta variable selection algorithm
            "BayesFactor", # for contingencyTableBF function
            "effsize", # for cohen.d() effect size test
            "randomForest",
            "ggplot2",
            "xgboost",
            "olsrr", # enables normality of residuals check
            "Ckmeans.1d.dp", # for xgb.ggplot.importance function
            "caret", # for varImp function
            "forcats", # for fct_lump() function
            "purrr", # for reduce()
            "gmodels", # for crosstable()
            "graphics", # for mosaicplot
            "corrplot", # for corplot
            "vcd", # for assocstats
            "fmsb", # oddsratio() function
            "MASS", # for loglinear analysis
            "mltools", # for one_hot function
            "stargazer"
  ))


###############################
###############################
#### Set Working Directory ####
###############################
###############################

setwd("C:/GTD")

#####################
#####################
#### Obtain Data ####
#####################
#####################

GTD <- read.csv("globalterrorismdb_0919dist.csv")

GTD_WD <- GTD_Prep(GTD)

##################
##################
# Country Counts #
##################
##################

t <- GTD_WD %>%
  dplyr::count(Country, sort = T)
t

factor_columns <- c("Group", "Target", "Attack", "Weapon", "Province", "City")