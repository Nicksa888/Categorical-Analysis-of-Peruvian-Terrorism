##########################
##########################
#### Project Contents ####
##########################
##########################

# This project contains
# Firstly, exploratory data Analysis will indicate the key data trends for all Peru, then broken down per group, decade, province etc.

# Frequentist and Bayesian chi square analysis will also feature, as will odds ratios, likelihoods and relative risks. 

######################
######################
# Important Websites #
######################
######################

# https://www.researchgate.net/post/What_is_the_minimum_number_of_observations_that_I_must_have_to_perform_a_regression_analysis

#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function for easier library loading
suppressPackageStartupMessages(
  libraries("data.table", # for set.names function
            "dplyr",
            "BayesFactor", # for contingencyTableBF function
            "ggplot2",
            "effsize", # for cohen.d () effect size test
            "BEST", # Bayesian hypothesis testing
            "gmodels", # for crosstable()
            "graphics", # for mosaicplot
            "corrplot", # for corplot
            "vcd", # for assocstats
            "fmsb", # oddsratio() function
            "MASS" # for loglinear analysis
            ))

###################
###################
#### Functions ####
###################
###################

# set seed #

seed <- 123

###############################
###############################
#### Set Working Directory ####
###############################
###############################

setwd("C:/Final Data Sets/Trinity College PhD 2021/GTD Data")

#####################
#####################
#### Obtain Data ####
#####################
#####################

GTD <- read.csv("globalterrorismdb_0919dist.csv")

########################
########################
#### Filtering Data ####
########################
########################

################################
# Filter Doubt Terrorism == No #
################################

# This removes all the non - terrorism violence as such incidents are coded yes
# Yes means it is doubted such violence is terrorism

GTDDT <- GTD %>% dplyr::filter(doubtterr == 0)
str(GTDDT)
dim(GTDDT)

###########################
# Filter Specificity == 1 #
###########################

# This removes all rows where the geographic coordinates have not been verified
# This is important because province and city variables are used in the modeling, so it is necessary to know exactly where each attack occurred.

GTDDTS <- GTDDT %>% dplyr::filter(specificity == 1)
str(GTDDTS)
dim(GTDDTS)

####################
# Filter Peru Data #
####################

# This selects only Peru Terrorism data 

Peru <- GTDDTS %>% dplyr::filter(country_txt == "Peru")
str(Peru)
dim(Peru)

##################
# Rename Columns #
##################

library(data.table) 

setnames(Peru, old = c("iyear", "imonth", "provstate", "city",    "attacktype1_txt", "gname", "targtype1_txt", "weaptype1_txt", "nkill", "nwound"), new = c("Year", "Month", "Province", "City", "Attack", "Group", "Target", "Weapon", "Dead", "Wounded"))

Peru[] <- lapply(Peru, factor) # convert data to factors

setwd("C:/R Portfolio/Categorical Analysis of Peru Terrorism") # save file
write.csv(Peru, file = "Peru_Raw.csv", row.names = F)
Peru_Raw <- read.csv("Peru_Raw.csv")

###############################
# Recoding Categorical Levels #
###############################

# Abancay district = Abancay
# Chorillos became Chorrillos

Peru_Raw <- read.csv("Peru_Raw.csv")

####################
####################
# One Hot Encoding # 
####################
####################

# Automatically converts each variable category into separate variables #

for(unique_value in unique(Peru_Raw$Year)){
  
  Peru_Raw[paste("Year", unique_value, sep = ".")] <- ifelse(Peru_Raw$Year == unique_value, 1, 0)
}

for(unique_value in unique(Peru_Raw$Decade)){
  
  Peru_Raw[paste("Decade", unique_value, sep = ".")] <- ifelse(Peru_Raw$Decade == unique_value, 1, 0)
}

for(unique_value in unique(Peru_Raw$Group)){
  
  Peru_Raw[paste("Group", unique_value, sep = ".")] <- ifelse(Peru_Raw$Group == unique_value, 1, 0)
}

for(unique_value in unique(Peru_Raw$Province)){
  
  Peru_Raw[paste("Province", unique_value, sep = ".")] <- ifelse(Peru_Raw$Province == unique_value, 1, 0)
}

for(unique_value in unique(Peru_Raw$City)){
  
  Peru_Raw[paste("City", unique_value, sep = ".")] <- ifelse(Peru_Raw$City == unique_value, 1, 0)
}

for(unique_value in unique(Peru_Raw$Target)){
  
  Peru_Raw[paste("Target", unique_value, sep = ".")] <- ifelse(Peru_Raw$Target == unique_value, 1, 0)
}

for(unique_value in unique(Peru_Raw$Attack)){
  
  Peru_Raw[paste("Attack", unique_value, sep = ".")] <- ifelse(Peru_Raw$Attack == unique_value, 1, 0)
}

for(unique_value in unique(Peru_Raw$Weapon)){
  
  Peru_Raw[paste("Weapon", unique_value, sep = ".")] <- ifelse(Peru_Raw$Weapon == unique_value, 1, 0)
}
str(Peru_Raw)

# create Lethal Variable

Peru_Raw$Lethal <- ifelse(Peru_Raw$Dead > 0, # more than zero is lethal
                          c(1), c(0))

# Move Lethal variable to immediately before Dead variable

which(colnames(Peru_Raw)== "Lethal") # Identify Lethal variable number
which(colnames(Peru_Raw)== "Dead") # Identify Dead variable number
Peru_Raw <- Peru_Raw %>% relocate( # move lethal before dead
  Lethal, 
  .before = Dead
)

# Create Decade Variable

Peru_Raw$Decade[Peru_Raw$Year >= 1970 & Peru_Raw$Year <= 1979] <- "1970"
Peru_Raw$Decade[Peru_Raw$Year >= 1980 & Peru_Raw$Year <= 1989] <- "1980"
Peru_Raw$Decade[Peru_Raw$Year >= 1990 & Peru_Raw$Year <= 1999] <- "1990"
Peru_Raw$Decade[Peru_Raw$Year >= 2000 & Peru_Raw$Year <= 2009] <- "2000"
Peru_Raw$Decade[Peru_Raw$Year >= 2010 & Peru_Raw$Year <= 2019] <- "2010"

# Move Decade variable to immediately before Year variable

Peru_Raw <- Peru_Raw %>% relocate( # move lethal before dead
  Decade, 
  .before = Year
)

setwd("C:/R Portfolio/Categorical Analysis of Peru Terrorism") # where to save file
write.csv(Peru_Raw, file = "Peru_Raw_Corrected.csv", row.names = F)

Peru_FC <- read.csv("Peru_Fully_Corrected_No_NA_Lethal.csv")
glimpse(Peru_FC)
Peru_FC[] <- lapply(Peru_FC, as.numeric)

Peru_FCS <- dplyr::select(Peru_FC, Decade, Year, Province, City, multiple, success, Attack, Target, Group, Weapon, Lethal)
colSums(is.na(Peru_FCS)) # sum NAs per column

###################
# Variable Counts #
###################

t <- Peru_FCS %>%
  count(Attack, sort = T)
View(t)

t <- Peru_FCS %>%
  count(Province, sort = T)
View(t)

t <- Peru_FCS %>%
  count(City, sort = T)
View(t)

t <- Peru_FCS %>%
  count(Target, sort = T)
View(t)

t <- Peru_FCS %>%
  count(Group, sort = T)
View(t)

t <- Peru_FCS %>%
  count(Weapon, sort = T)
View(t)

######################
######################
# Splitting the Data #
######################
######################

# Using the sample() function, let's create our training and test datasets using a 75% to 25% split.
# The set.seed() function is used to ensure that we can get the same result every time we run a random sampling process.
set.seed(seed)
sample_set <- sample(nrow(Peru_FC), round(nrow(Peru_FC)*.75), replace = FALSE)
Peru_FC_train <- Peru_FC[sample_set, ]
Peru_FC_test <- Peru_FC[ - sample_set, ]

#############################
#############################
# Decade Variable Selection #
#############################
#############################

# Identify city variable numbers

which(colnames(Peru_FC) == "Lethal") # 
which(colnames(Peru_FC) == "DecadesStartHere") # 
which(colnames(Peru_FC) == "YearsStartHere") # 

# To determine the useful city variables

Peru_DVS <- Peru_FC[ , c(100, # select Lethal variable
                               139:143)] # decade Variables
names(Peru_DVS)

##################################
# Bayes Factor Feature Selection #
##################################

DBFPV <- setdiff(colnames(Peru_DVS), "Lethal") # Remove Lethal from predictor variables

DBFPV_nlevels <- sapply(Peru_DVS[, DBFPV], function(i)length(unique(i))) #ensure all variables have at least one level
DBFPV <- DBFPV[DBFPV_nlevels > 1]
length(DBFPV)
results <- lapply(Peru_DVS[, DBFPV], function(i){
  CT <- table(i, Peru_DVS$Lethal)
  BA <- contingencyTableBF(CT, sampleType = "poisson") # my sample is random 
  list(CT = CT, BA = BA)
})

names(results) <- DBFPV
names(results)
results[[1]]$BA

setwd("C:/R Portfolio/Categorical Analysis of Peru Terrorism")

CBF <- read.csv("Decade BF Scores.csv")

# Evidence for H0 #

CBBF <- filter(CBF, BFScore <= 0.01) # extreme evidence for H0
CBBF
CBBF <- filter(CBF, BFScore >= 0.01 & BFScore <= 0.03333333)
CBBF <- filter(CBF, BFScore >= 0.03333333 & BFScore <= 0.1) 
CBBF <- filter(CBF, BFScore >= 0.1 & BFScore <= 0.3333333)
CBBF <- filter(CBF, BFScore >= 0.3333333 & BFScore <= 1)
CBBF

# Evidence for H1 #

CBBF <- filter(CBF, BFScore >= 30 & BFScore <= 100)
CBBF
CBBF <- filter(CBF, BFScore >= 100)
CBBF

# No decades are significant

###########################
###########################
# City Variable Selection #
###########################
###########################

# Identify city variable numbers

which(colnames(Peru_FC) == "Lethal") # 
which(colnames(Peru_FC) == "CitiesStartHere") # 
which(colnames(Peru_FC) == "TargetsStartHere") # 

# To determine the useful city variables

Peru_CVS <- Peru_FC[ , c(100, # Lethal variable
                       241:806)] # city Variables
names(Peru_CVS)

##################################
# Bayes Factor Feature Selection #
##################################

CBFPV <- setdiff(colnames(Peru_CVS), "Lethal") # Remove Lethal from predictor variables

CBFPV_nlevels <- sapply(Peru_CVS[, CBFPV], function(i)length(unique(i))) #ensure all variables have at least one level
CBFPV <- CBFPV[CBFPV_nlevels > 1]
length(CBFPV)
results <- lapply(Peru_CVS[, CBFPV], function(i){
  CT <- table(i, Peru_CVS$Lethal)
  BA <- contingencyTableBF(CT, sampleType = "poisson") # my sample is random 
  list(CT = CT, BA = BA)
})

names(results) <- CBFPV
names(results)
results[[39]]$BA
results[[5]]

results$BA

setwd("C:/Casualty Prediction")

CBF <- read.csv("Peru_City_BF.csv")

# Evidence for H0 #

CBBF <- filter(CBF, BFScore <= 0.01) # extreme evidence for H0
CBBF
CBBF <- filter(CBF, BFScore >= 0.01 & BFScore <= 0.03333333)
CBBF <- filter(CBF, BFScore >= 0.03333333 & BFScore <= 0.1) 
CBBF <- filter(CBF, BFScore >= 0.1 & BFScore <= 0.3333333)
CBBF <- filter(CBF, BFScore >= 0.3333333 & BFScore <= 1)
CBBF

# Evidence for H1 #

CBBF <- filter(CBF, BFScore >= 30 & BFScore <= 100)
CBBF
CBBF <- filter(CBF, BFScore >= 100)
CBBF

###############################
###############################
# Province Variable Selection #
###############################
###############################

# Identify city variable numbers

which(colnames(Peru_FC) == "Lethal") # 
which(colnames(Peru_FC) == "ProvincesStartHere") # 
which(colnames(Peru_FC) == "CitiesStartHere") # 

# To determine the useful province variables

Peru_PVS <- Peru_FC[ , c(100, # Lethal
                               216:239)] # province Variables
names(Peru_PVS)

##################################
# Bayes Factor Feature Selection #
##################################

PBFPV <- setdiff(colnames(Peru_PVS), "Lethal") # Remove Lethal from predictor variables

PBFPV_nlevels <- sapply(Peru_PVS[, PBFPV], function(i)length(unique(i))) #ensure all variables have at least one level
PBFPV <- PBFPV[PBFPV_nlevels > 1]
length(PBFPV)
results <- lapply(Peru_PVS[, PBFPV], function(i){
  CT <- table(i, Peru_PVS$Lethal)
  BA <- contingencyTableBF(CT, sampleType = "poisson") # my sample is random 
  list(CT = CT, BA = BA)
})

names(results) <- PBFPV
names(results)
results[[23]]$BA

setwd("C:/Casualty Prediction")

CBF <- read.csv("Province BF Scores.csv")

# Evidence for H0 #

CBBF <- filter(CBF, BFScore <= 0.01) # extreme evidence for H0
CBBF
CBBF <- filter(CBF, BFScore >= 0.01 & BFScore <= 0.03333333)
CBBF <- filter(CBF, BFScore >= 0.03333333 & BFScore <= 0.1) 
CBBF <- filter(CBF, BFScore >= 0.1 & BFScore <= 0.3333333)
CBBF <- filter(CBF, BFScore >= 0.3333333 & BFScore <= 1)
CBBF

# Evidence for H1 #

CBBF <- filter(CBF, BFScore >= 30 & BFScore <= 100)
CBBF
CBBF <- filter(CBF, BFScore >= 100)
CBBF

# Final Province Feature List

# Lima,	Ayacucho, Huancavelica, Apurimac, Huanuco, Puno, SanMartin 

#############################
#############################
# Target Variable Selection #
#############################
#############################

# Identify target variable numbers

which(colnames(Peru_FC) == "Lethal") # 
which(colnames(Peru_FC) == "TargetsStartHere") # 
which(colnames(Peru_FC) == "AttacksStartHere") # 

# To determine the useful target variables

Peru_TVS <- Peru_FC[ , c(100, # Lethal variable
                               808:826)] # target Variables
names(Peru_TVS)

##################################
# Bayes Factor Feature Selection #
##################################

TBFPV <- setdiff(colnames(Peru_TVS), "Lethal") # Remove Lethal from predictor variables

TBFPV_nlevels <- sapply(Peru_TVS[, TBFPV], function(i)length(unique(i))) #ensure all variables have at least one level
TBFPV <- TBFPV[TBFPV_nlevels > 1]
length(TBFPV)
results <- lapply(Peru_TVS[, TBFPV], function(i){
  CT <- table(i, Peru_TVS$Lethal)
  BA <- contingencyTableBF(CT, sampleType = "poisson") # my sample is random 
  list(CT = CT, BA = BA)
})

names(results) <- TBFPV
names(results)
results[[4]]$BA

setwd("C:/Casualty Prediction")

CBF <- read.csv("Target BF Scores.csv")

# Evidence for H0 #

CBBF <- filter(CBF, BFScore <= 0.01) # extreme evidence for H0
CBBF
CBBF <- filter(CBF, BFScore >= 0.01 & BFScore <= 0.03333333)
CBBF <- filter(CBF, BFScore >= 0.03333333 & BFScore <= 0.1) 
CBBF <- filter(CBF, BFScore >= 0.1 & BFScore <= 0.3333333)
CBBF <- filter(CBF, BFScore >= 0.3333333 & BFScore <= 1)
CBBF

# Evidence for H1 #

CBBF <- filter(CBF, BFScore >= 30 & BFScore <= 100)
CBBF
CBBF <- filter(CBF, BFScore >= 100)
CBBF

# Final Target Feature Selection

# GovernmentDiplomatic, Business, Food Water Supply, Utilities, JournalistsMedia,  Private, Transportation, Police, TerroristsNonStateMilitia

############################
############################
# Group Variable Selection #
############################
############################

# Identify target variable numbers

which(colnames(Peru_FC) == "Lethal") # 
which(colnames(Peru_FC) == "GroupsStartHere") # 
which(colnames(Peru_FC) == "ProvincesStartHere") # 

# To determine the useful group variables

Peru_GVS <- Peru_FC[ , c(100, # Lethal
                               187:214)] # group Variables
names(Peru_GVS)

# Remove Unknown Group

Peru_GVS <- dplyr::select(Peru_GVS, -c("UnknownGroup"))

##################################
# Bayes Factor Feature Selection #
##################################

GBFPV <- setdiff(colnames(Peru_GVS), "Lethal") # Remove Lethal from predictor variables

GBFPV_nlevels <- sapply(Peru_GVS[, GBFPV], function(i)length(unique(i))) #ensure all variables have at least one level
GBFPV <- GBFPV[GBFPV_nlevels > 1]
length(GBFPV)
results <- lapply(Peru_GVS[, GBFPV], function(i){
  CT <- table(i, Peru_GVS$Lethal)
  BA <- contingencyTableBF(CT, sampleType = "poisson") # my sample is random 
  list(CT = CT, BA = BA)
})

names(results) <- GBFPV
names(results)
results[[25]]$BA

setwd("C:/Casualty Prediction")

CBF <- read.csv("Group BF Scores.csv")

# Evidence for H0 #

CBBF <- filter(CBF, BFScore <= 0.01) # extreme evidence for H0
CBBF
CBBF <- filter(CBF, BFScore >= 0.01 & BFScore <= 0.03333333)
CBBF <- filter(CBF, BFScore >= 0.03333333 & BFScore <= 0.1) 
CBBF <- filter(CBF, BFScore >= 0.1 & BFScore <= 0.3333333)
CBBF <- filter(CBF, BFScore >= 0.3333333 & BFScore <= 1)
CBBF

# Evidence for H1 #

CBBF <- filter(CBF, BFScore >= 30 & BFScore <= 100)
CBBF
CBBF <- filter(CBF, BFScore >= 100)
CBBF

# Final Group List

# Shining Path and Tupac Amaru Revolutionary Movement  

#############################
#############################
# Attack Variable Selection #
#############################
#############################

# Identify target variable numbers

which(colnames(Peru_FC) == "Lethal") # 
which(colnames(Peru_FC) == "AttacksStartHere") # 
which(colnames(Peru_FC) == "WeaponsStartHere") # 

# To determine the useful city variables

Peru_AVS <- Peru_FC[ , c(100, # Lethal
                               828:836)] # group Variables
names(Peru_AVS)

# Remove Unknown Attack

Peru_AVS <- dplyr::select(Peru_AVS, -c("UnknownAttack"))

##################################
# Bayes Factor Feature Selection #
##################################

ABFPV <- setdiff(colnames(Peru_AVS), "Lethal") # Remove Lethal from predictor variables

ABFPV_nlevels <- sapply(Peru_AVS[, ABFPV], function(i)length(unique(i))) #ensure all variables have at least one level
ABFPV <- ABFPV[ABFPV_nlevels > 1]
length(ABFPV)
results <- lapply(Peru_AVS[, ABFPV], function(i){
  CT <- table(i, Peru_AVS$Lethal)
  BA <- contingencyTableBF(CT, sampleType = "poisson") # my sample is random 
  list(CT = CT, BA = BA)
})
glimpse(results)
View(results)

names(results) <- ABFPV
names(results)
results[[8]]$BA

setwd("C:/Casualty Prediction")

CBF <- read.csv("Attack BF Scores.csv")

# Evidence for H0 #

CBBF <- filter(CBF, BFScore <= 0.01) # extreme evidence for H0
CBBF
CBBF <- filter(CBF, BFScore >= 0.01 & BFScore <= 0.03333333)
CBBF <- filter(CBF, BFScore >= 0.03333333 & BFScore <= 0.1) 
CBBF <- filter(CBF, BFScore >= 0.1 & BFScore <= 0.3333333)
CBBF <- filter(CBF, BFScore >= 0.3333333 & BFScore <= 1)
CBBF

# Evidence for H1 #

CBBF <- filter(CBF, BFScore >= 30 & BFScore <= 100)
CBBF
CBBF <- filter(CBF, BFScore >= 100)
CBBF

# Final Attack Feature Selected List 

# Bomb, ArmedAssault, Assassination, InfrastructureAttack and Hostage Barricade
	
#############################
#############################
# Weapon Variable Selection #
#############################
#############################

# Identify target variable numbers

which(colnames(Peru_FC) == "Lethal") # 
which(colnames(Peru_FC) == "WeaponsStartHere") # 
which(colnames(Peru_FC) == "Melee") # 

# To determine the useful city variables

Peru_WVS <- Peru_FC[ , c(100, # Lethal
                               838:844)] # group Variables
names(Peru_WVS)

Peru_WVS <- dplyr::select(Peru_WVS, -c("UnknownWeapon"))

##################################
# Bayes Factor Feature Selection #
##################################

WBFPV <- setdiff(colnames(Peru_WVS), "Lethal") # Remove Lethal from predictor variables

WBFPV_nlevels <- sapply(Peru_WVS[, WBFPV], function(i)length(unique(i))) #ensure all variables have at least one level
WBFPV <- WBFPV[WBFPV_nlevels > 1]
length(WBFPV)
results <- lapply(Peru_WVS[, WBFPV], function(i){
  CT <- table(i, Peru_WVS$Lethal)
  BA <- contingencyTableBF(CT, sampleType = "poisson") # my sample is random 
  list(CT = CT, BA = BA)
})
glimpse(results)
View(results)

names(results) <- WBFPV
names(results)
results[[6]]$BA

setwd("C:/Casualty Prediction")

CBF <- read.csv("Weapon BF Scores.csv")

# Evidence for H0 #

CBBF <- filter(CBF, BFScore <= 0.01) # extreme evidence for H0
CBBF
CBBF <- filter(CBF, BFScore >= 0.01 & BFScore <= 0.03333333)
CBBF <- filter(CBF, BFScore >= 0.03333333 & BFScore <= 0.1) 
CBBF <- filter(CBF, BFScore >= 0.1 & BFScore <= 0.3333333)
CBBF <- filter(CBF, BFScore >= 0.3333333 & BFScore <= 1)
CBBF

# Evidence for H1 #

CBBF <- filter(CBF, BFScore >= 30 & BFScore <= 100)
CBBF
CBBF <- filter(CBF, BFScore >= 100)
CBBF

# Final Province Feature List

# Lima,	Ayacucho, Huancavelica, Apurimac, Huanuco, Puno, SanMartin 

# Final Target Feature Selection

# GovernmentDiplomatic, Business, Food Water Supply, Utilities, JournalistsMedia,  Private, Transportation, Police, TerroristsNonStateMilitia

# Final Group List

# Shining Path and Tupac Amaru Revolutionary Movement  

# Final Weapons List

# Explosives, Firearms, Incendiary, Melee

# Final Attack Feature Selected List 

# Bomb, ArmedAssault, Assassination, InfrastructureAttack and Hostage Barricade

######################################
######################################
# Bayes Factor Temporary Final Model #
######################################
######################################

# The below model contains all the extremely significant features when using attack type and province. Weapon Type and city variables are included in a different model

PeruFFS <- dplyr::select(Peru_FC, c("Lethal", "LimaProvince",	"Ayacucho", "Huancavelica", "Apurimac", "Huanuco", "Puno", "SanMartin", "GovernmentDiplomatic", "Business", "FoodWaterSupply", "Utilities", "JournalistsMedia",  "Private", "Transportation", "Police", "TerroristsNonStateMilitia", "ShiningPath", "TupacAmaruRevolutionaryMovement", "Bomb", "ArmedAssault", "Assassination", "InfrastructureAttack", "HostageBarricade"))

##################################
# Bayes Factor Feature Selection #
##################################

PeruFFSPV <- setdiff(colnames(PeruFFS), "Lethal") # Remove Lethal from predictor variables

PeruFFS_nlevels <- sapply(PeruFFS[, PeruFFSPV], function(i)length(unique(i))) #ensure all variables have at least one level
PeruFFSPV <- PeruFFSPV[PeruFFS_nlevels > 1]
length(PeruFFSPV)
results <- lapply(PeruFFS[, PeruFFSPV], function(i){
  CT <- table(i, PeruFFS$Lethal)
  BA <- contingencyTableBF(CT, sampleType = "poisson") # my sample is random 
  list(CT = CT, BA = BA)
})
glimpse(results)
View(results)

names(results) <- PeruFFSPV
names(results)
results[[19]]$BA

CBF <- read.csv("Final Model BF Scores.csv")

# Evidence for H0 #

CBBF <- filter(CBF, BFScore <= 0.01) # extreme evidence for H0
CBBF
CBBF <- filter(CBF, BFScore >= 0.01 & BFScore <= 0.03333333)
CBBF <- filter(CBF, BFScore >= 0.03333333 & BFScore <= 0.1) 
CBBF <- filter(CBF, BFScore >= 0.1 & BFScore <= 0.3333333)
CBBF <- filter(CBF, BFScore >= 0.3333333 & BFScore <= 1)
CBBF

# Evidence for H1 #

CBBF <- filter(CBF, BFScore >= 30 & BFScore <= 100)
CBBF
CBBF <- filter(CBF, BFScore >= 100)
CBBF

###########################################################
###########################################################
# Final Bayes Factor Model Ready for Categorical Analysis #
###########################################################
###########################################################

PeruFM <- dplyr::select(Peru_FC, c("Lethal", "LimaProvince",	"Ayacucho", "Huancavelica", "Apurimac", "Huanuco", "Puno", "SanMartin", "GovernmentDiplomatic", "Business", "FoodWaterSupply", "Utilities", "JournalistsMedia",  "Private", "Transportation", "Police", "TerroristsNonStateMilitia", "ShiningPath", "TupacAmaruRevolutionaryMovement", "Bomb", "ArmedAssault", "Assassination", "InfrastructureAttack", "HostageBarricade"))

####################
# Chi Square Tests #
####################

# Shining Path and Lethal Contingency Table #
TOR <- filter(PeruFM, Lethal != 1 & ShiningPath != 1)
ToR <- filter(COL, CundinamarcaDept != 1 & ArmedAssaultAttack != 1)
LA_SP <- matrix(c(1194, 130, 1704, 757), ncol = 2, byrow = T)
colnames(LA_SP) <- c("Shining Path", "Other Group")
rownames(LA_SP) <- c("Lethal", "Non Fatal")
str(LA_SP)
LA_SP <- as.table(LA_SP)
LA_SP

CrossTable(LA_SP, 
           prop.r = T, 
           prop.c = T,
           prop.t = T,
           prop.chisq = T, 
           chisq = T, 
           expected = T,
           fisher = T,
           mcnemar = T,
           resid = F, 
           sresid = T, 
           asresid = F,
           format = "SPSS"
)

# In the above code in the CrossTable() function, lines two to four instruct R to calculate the proportion of each cell value to the column and row it is part of and the overall table total. The prop.chisq = True instruction generates a value indicative of the contribution each cell makes to the overall chi square score. The next four lines generate outputs for various chi-square test and the following three generate information on residuals. The final line instructs R to display the output table in the style of SPSS statistical software.

#########################################
# Report of the above contingency table #
#########################################

# All expected counts are much higher than five and the data is independent. Therefore, both assumptions have been met, which means we can be confident in the output. 

# The expected cell values are what would be expected given that the two variables are independent. In other words, the expected cell totals indicate the values that would occur by chance alone.

# The p-value is very small at 1.124202e-47, indicative that lethality (or not) has has a significant effect on lethal attack by Shining Path. The significant p-value also indicates that it is very unlikely that the chi squared statistic of 210.3989 would be so large purely by chance if there was no association between the variables.

#The chi-square statistic is computed by taking the sum of the observed frequency  minus the expected frequency squared divided by the expected frequency in each of the four cells

# The computed chi-square value is compared to a tabled chi-square value for a given degree of freedom. The degrees of freedom are always determined by the number of rows minus one (r ??? 1) times the number of columns minus one (c ??? 1). This can be expressed as: df = (r ??? 1)(c ??? 1). Since there are two rows and two columns, the degree of freedom is: df = (2 ??? 1)(2 ??? 1) = 1. The tabled chi-square value for df = 1 and a .05 level of significance is 3.84. Since the computed chi-square value of 210.3989 is greater than the tabled chi-square value of 3.84, we reject the null hypothesis in favor of the alternative hypothesis that lethal or non lethal attack differs in the percent where Shining Path is responsible for an attack. The chi-square statistic will be small if there are small differences between the observed and the expected values, and it will be large if there large differences. The size of the difference indicates how closely the actual observed data values match what would be expected ig the null hypothesis were true. The large chi square statistic indicates it is not a good fit to the data for the null hypothesis to be true.  Since each cell value is greater than 3.84, (32.059, 104.742, 17.2476.67, and 56.351), we would conclude that each cross-tabulated cell significantly contributed to the overall chi-square. Since each value is above the 3.84, type I and type II errors are not applicable as it the chi square output is beyond a chance level of probability

qchisq(p = .95, df = 1) # determines the correct chi square value
pchisq(q = 210.3989, df = 1, lower.tail = FALSE) # calculate the actual p-value. # The lower tail instructs R to indicate the probabiity of getting a score of 210.3989 or higher score by chance
 
# The last number in each cell refers to the standarized residual. The standardised residual values indicate the impact on chi square. It is possible to determine the significance of each. If the value lies outside of plus or minus 1.96, then it is significant at p <.05, if it lies outside of plus or minus 2.58, then it is significant at p <.01, and if it lies outside of plus or minus 3.29, then it is significant at p <.001. 

# All such residuals are more than plus or minus 3.29, so each figure is significant at p <.001. Shining Path were unlikely to carry out non lethal attacks and collective other groups were unlikely to carry out lethal attacks. This is evident with the minus sign before these two figures.The residual values are smaller for non fatal attack than for lethal attack, which therefore means that the association between lethal attack and Shining Path Group is driven mainly by lethal attack rather than non fatal attack

################
################
# Effect Sizes #
################
################

# Association has been indicated between the variables. However, this doesn't indicate how strong the association. To determine this, effect size calculations must be completed. Cramers V is a frequent test

###########
# Cramers V
###########

# This determines the strength of the relationship - known as effect size
# A common technique is cramers V - as found in the lsr package

# A Rule of Thumb to interpret cramers V output:

# Small or weak effect size: v = .1
# Medium or moderate effect size: v = .3
# Large or strong effect size: v = .5

library(lsr)
cramersV(PeruFM$ShiningPath, PeruFM$Lethal)
cramersV(LA_SP)

# The effect size of 0.2351162 is weak to moderate as it is between the two

###############
# Odds Ratios #
###############

# Another means of determining effect size when both variables are binary are odds ratios. Odds ratios measure the odds of some event happening given a particular exposure compared to the odds of it occuring when the exposure is absent. In this context, the exposure is terrorist group responsbility of attack (Shining Path or other group) and the event is fatality of attack (lethal or non fatal attack). The odds ratio would measure the odds of lethal attack by Shining Path, compared to the odds of non fatal attack by Shining Path.

##########################
# Likelihood and estimates
##########################

# The purpose is to estimate probability of lethal or non fatal attacks per cell using a contingency table

rowSums(LA_SP)
TR <- LA_SP/rowSums(LA_SP)
TR

# Likelihood of lethal attack by Shining Path. The difference with 100% is the probability of lethal attack by collective other group.

0.90181269 * 100
round(0.90181269 * 100, 2)

# the purpose here is to calculate a confidence interval for the differences in the department probabilities given the attack outcomes.

alpha <- 0.05
TR1 <- TR[1,1]
TR2 <- TR[2,1]

# Wald confidence

var.wald <- TR1 * (1-TR1) / sum(LA_SP[1,]) + 
  TR2 * (1-TR2) / sum(LA_SP[1,])
TR1 - TR2 + qnorm(p = c(alpha/2, 1-alpha/2)) *
  sqrt(var.wald)

# Agresti - Caffo

var.ac1 <- (LA_SP[1,1] + 1) / (sum(LA_SP[1,]) + 2)
var.ac2 <- (LA_SP[2,1] + 1) / (sum(LA_SP[2,]) + 2)
VAR.AC <- var.ac1 * (1-var.ac1) / (sum(LA_SP[1,]) + 2) +
  var.ac2 * (1-var.ac2) / (sum(LA_SP[2,]) + 2)
var.ac1 - var.ac2 + qnorm(p = c(alpha/2, 1-alpha/2)) * 
  sqrt(VAR.AC)

# test for the difference of two probabilities:
prop.test(LA_SP, conf.level = 0.95, correct = F)
assocstats(LA_SP)

################
# Relative Risks 
################

# to determine risk of lethal attack compared to non fatal attacks

TR <- LA_SP/rowSums(LA_SP)
alpha <- 0.05
TR1 <- TR[1,1]
TR2 <- TR[2,1]
round(TR1/TR2, 4) # to determine risk of lethal attack compared to non fatal attacks. It's 1.3024 times more likely.

n1 <- sum(LA_SP[1,])
n2 <- sum(LA_SP[2,])
#wald confidence interval
var.log.rr <- (1-TR1)/(n1*TR1) +
  (1-TR2)/(n2*TR2)
ci <- exp(log(TR1/TR2) + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.log.rr))
round(ci,4)
rev(round(ci,4)) # inverted

#############
# Odds Ratios 
#############

# Odds are simply the probability of a success divided by the probability of a failure.

# The purpose is to estimate the odds ratio describing the relationship between lethal attack and Shining Path.

library(questionr)
odds.ratio(LA_SP)

OR.hat <- LA_SP[1,1] * LA_SP[2,2] / (LA_SP[2,1] * LA_SP[1,2])
round(OR.hat, 4) # lethal attack by Shining Path is is 4.0803
round(1/OR.hat, 4)

alpha <- 0.05

var.log.or <- 1/LA_SP[1,1] + 1/LA_SP[1,2] + 1/LA_SP[2,1] + 1/LA_SP[2,2]
OR.CI <- exp(log(OR.hat) + qnorm(p = c(alpha/2, 1-alpha/2)) +
               sqrt(var.log.or))

round(OR.CI, 2)

###################################
###################################
# Contingency Table Visualisation #
###################################
###################################

mosaicplot(LA_SP, 
           shade = T,
           main = "Lethal Attack - Shining Path")

# The width of each column indicates the volume in that column compared to the other column
# The volume of each cell in a particular column indicates th volume in that cell compared to the other cell in that particular column
# Each cell is coloured on a sliding scale from dark blue to dark red. Dark blue indicates the count in that cell is much more than expected if there was no association between that pair of variables, while dark red indicates that the count is much less.
# As evident, lethal attacks by Shining Path are much more than expected given independence, while non fatal attacks by Shining Path are much less than expected
# 

##################################
##################################
# Contingency Coefficient Values #
##################################
##################################

# This is another means to calculate effect size
# Understanding Contingency Coefficient Values
# The contingency coefficient helps us decide if variable b is 'contingent' on variable a. However, it is a rough measure and doesn't quantify the dependence exactly; It can be used as a rough guide:
# If C is near zero (or equal to zero) you can conclude that your variables are independent of each other; there is no association between them.
# If C is away from zero there is some relationship; C can only take on positive values.
# The larger the table your chi-squared coefficient is calculated from, the closer to 1 a perfect association will approach. That's why some statisticians suggest using the contingency coefficient only if you're working with a 5 by 5 table or larger.

Contingency <- function(x) {
  chi <- chisq.test(x)
  unname(sqrt(chi$statistic / (chi$statistic + sum(x))))
}

Contingency(LA_SP)

###################################
###################################
# Contingency Table Probabilities #
###################################
###################################

# Lethal Attack - Shining Path

LA_SP <- matrix(c(1194, 130, 1704, 757), ncol = 2, byrow = T)
colnames(LA_SP) <- c("Shining Path", "Other Group")
rownames(LA_SP) <- c("Lethal", "Non Fatal")
str(LA_SP)
LA_SP <- as.table(LA_SP)
LA_SP
margin.table(LA_SP) # This is the grand total of the table
margin.table(LA_SP, 1) # These are the marginal totals for rows
margin.table(LA_SP, 2) # These are the marginal totals for columns

# The probability below determines the probabilistic outcome for each cell when compared to all other cells

LA_SP_Probs <- round(LA_SP/margin.table(LA_SP) * 100, 2) # Calculate Probabilities
LA_SP_Probs

LA_SP_Probs_Col_SP <- round(2898/margin.table(LA_SP) * 100, 2) # Calculate Probabilities of Shining Path column total
LA_SP_Probs_Col_SP

LA_SP_Probs_Col_OG <- round(887/margin.table(LA_SP) * 100, 2) # Calculate Probabilities of Other Group column total
LA_SP_Probs_Col_OG

# The below table indicates the probability of Lethal attack type given that we know the terrorist violence happens by Shining Path

# the total probability of any attack by Shining Path is 76.57
# Therefore, each probability of any attack by Shining Path must be divided by this probability figure of 76.57

# Probability for Lethal attack given that we know Shining Path is responsible for terrorist violence 
round(31.55/76.57 * 100, 2) # = 62.27%

# Probability for Lethal attack given that we know Other Group is responsible for terrorist violence 
round(3.43/23.43 * 100, 2) # = 37.78%

#######################################
# Bayesian Contingency Table Analysis #
#######################################

# https://stats.libretexts.org/Bookshelves/Applied_Statistics/Book%3A_Learning_Statistics_with_R_-_A_tutorial_for_Psychology_Students_and_other_Beginners_(Navarro)/17%3A_Bayesian_Statistics/17.06%3A_Bayesian_Analysis_of_Contingency_Tables

# http://www.alexander-ly.com/wp-content/uploads/2014/09/JamilEtAlGunelDickeyinpress.pdf 
# The website directly above indicates the grading of Bayes Factor scores

# Lethal Attack - Shining Path

LA_SP <- matrix(c(1194, 130, 1704, 757), ncol = 2, byrow = T)
colnames(LA_SP) <- c("Shining Path", "Other Group")
rownames(LA_SP) <- c("Lethal", "Non Fatal")
str(LA_SP)
LA_SP <- as.table(LA_SP)
LA_SP

contingencyTableBF(LA_SP, sampleType = "poisson", posterior = F)
# sampleType poisson is used because the column and or row totals are not fixed, as we would expect in an experiment where we might want 100 respondents
# There is no specific target for the number of observations or proportions assigned to different categories

##################
# Results Output #
##################

# The Bayes Factor Score of 2.899436e+49 supports the hypothesis that there is association between the variables.
# The ±0% means that R has calculated an exact Bayes Factor score, so uncertainty about the Bayes Factor score is 0%

# A score of 2.899436e+49 is within the score category of at least 100 for Extreme evidence in favour of association

#############################################################################
# Is the proportion of lethal attack to non fatal attack different across the columns?
#############################################################################

LA_SP_S <- contingencyTableBF(LA_SP, sampleType = "poisson", posterior = T, iterations = 10000)
summary(LA_SP_S)

# To create the lethal-non-fatal ratio for Shining Path

# This creates a ratio of the Lethal:Shining Path Cell count to the Non Fatal:Shining Path Cell count

SPProp <- LA_SP_S[,"lambda[1,1]"] / LA_SP_S[,"lambda[2,1]"]
names(SPProp)
str(SPProp)
hist(SPProp, main = "Lethal-non fatal ratio for Shining Path", xlab = "Shining Path Proportion")
mean(SPProp)

# The average ratio is that Shining Path are 70% as likely to conduct a lethal attack as a non fatal attack

# To create the lethal-non-fatal ratio for Other Group

# This creates a ratio of the Lethal:Other Group Cell count to the Non Fatal:Other Group Cell count

OGProp <- LA_SP_S[,"lambda[1,2]"] / LA_SP_S[,"lambda[2,2]"]
names(OGProp)
str(OGProp)
hist(OGProp, main = "Lethal-non fatal ratio for Other Group", xlab = "Other Group Proportion")
mean(OGProp)

# The average ratio is that Other Group are 17% as likely to conduct a lethal attack as a non fatal attack

# Difference in proportion between columns:

diffprop <- SPProp - OGProp
hist(diffprop, main = "", xlab = "")
abline(v = quantile(diffprop, c(0.025), col = "black")) # lower bound of 95% HDI
abline(v = quantile(diffprop, c(0.975), col = "black")) # higher bound of 95% HDI
round(mean(diffprop) * 100, 2)
# This indicates how much the lethal:non fatal ratio decreases as we switch columns from Shining Path Group to collective other group. The average decrease is 52.82
# The two vertical lines indicate where 95% of the data is found. Data points within this interval have a higher probability density than those outside the intervals. The hdi function below indicates the exact percentages of the HDI intervals.

library(bayestestR)
hdi(diffprop, ci = 0.95)

######################
# Loglinear Analysis #
######################

LPSPBTLCT <- xtabs(~ Business + ShiningPath + Lethal, PeruFM)

LPSPBTLCT_LL_Model <- loglm(~ Business * ShiningPath * Lethal, LPSPBTLCT)
summary(LPSPBTLCT_LL_Model)

# The above model captures all the effects of the variable interactions. Therefore, it is a perfect fit for the data as evident with the  likelihood ratio of zero and p-value of one.

threeWay <- update(LPSPBTLCT_LL_Model, .~. - Business:ShiningPath:Lethal)
summary(threeWay)

# This model has a likelihood ratio of 0.4734802 and a p-value p <.001, so seems a poor fit to the data

anova(LPSPBTLCT_LL_Model, threeWay)

# The p-value is 1, so it is not significant, so we can remove this threeway interaction without making the model worse.

mosaicplot(LPSPBTLCT, shade = T)




###############################
# Bayesian Loglinear Analysis #
###############################

library(conting)
oh_ex <- bcct(formula = y ~ alc * hyp * obe, data = AOH,
              + n.sample = 1000, prior = "UIP")

set.seed(123)
test1 <- bcct(formula = Lethal ~ (Business + ShiningPath) ^ 2, data = PeruFM, n.sample = 100, prior = "UIP")
summary(test1)
sub_model(test1)
# Bayesian Hypothesis Testing
carsBEST <- BESTmcmc(PeruFM$Lethal, PeruFM$ShiningPath)
carsBEST
plot(carsBEST)


t.test(PeruFM$Lethal, PeruFM$ShiningPath)

# The t.test output merely tells us that the relationship between the variable means is sigificant, which means there is some relationship. However, it tells us nothing about the magnitude of this relationship. For that, we can explore effect size, and one way of measuring effect size is to use cohen.d test, part of the effsize library

cohen.d(mtcars$mpg[mtcars$am==0], mtcars$mpg[mtcars$am==1])






