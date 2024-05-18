#############
#############
#############
# Functions #
#############
#############
#############

# The GTD_Prep function prepares a dataset related to terrorism incidents by performing the following steps:
#   
#   Filter Data:
#   
#   Exclude incidents that are not confirmed as terrorism (doubtterr == 0).
# Exclude incidents with unverified geographic coordinates (specificity == 1).
# Rename Columns:
#   
#   Rename several columns to more readable names.
# Select Columns:
#   
#   Select a subset of relevant columns for further analysis.
# Handle Missing Values:
#   
#   Replace NA values in the Dead column with 0.
# Create New Variables:
#   
#   Create a binary Lethal variable indicating the presence of fatalities.
# Create categorical variables for Decade, Quarter, and Week based on the date of the incident.
# Convert Variables to Factors:
#   
#   Convert the Decade, Quarter, and Week variables into factor data types.
# Recode Variables:
#   
#   Recode several categorical variables (Attack, Target, Group, Province) to more concise or descriptive values.
# The function cleans, transforms, and enriches the dataset to make it more suitable for analysis and modeling.

############
############
# GTD Prep #
############
############

GTD_Prep <- function(data){
  
  ################################
  # Filter Doubt Terrorism == No #
  ################################
  
  # This removes all the non - terrorism violence as such incidents are coded yes
  # Yes means it is doubted such violence is terrorism
  
  GTDDT <- data %>% dplyr::filter(doubtterr == 0)
  
  ###########################
  # Filter Specificity == 1 #
  ###########################
  
  # This removes all rows where the geographic coordinates have not been verified
  # This is important because province and city variables are used in the modeling, so it is necessary to know exactly where each attack occurred.
  
  GTDDTS <- GTDDT %>% dplyr::filter(specificity == 1)
  setnames(GTDDTS, old = c("iyear", "imonth", "iday", "extended", "country_txt",  "region_txt", "provstate", "city", "success", "multiple", "suicide", "attacktype1_txt", "gname", "targtype1_txt", "natlty1_txt", "weaptype1_txt", "nkill", "nwound"), new = c("Year", "Month", "Day", "Extended", "Country",  "Region", "Province", "City", "Success", "Multiple", "Suicide", "Attack", "Group", "Target", "Nationality", "Weapon", "Dead", "Wounded"))
  
  # Select specific variables #
  
  GTTDS <- GTDDTS %>% dplyr::select("Year", "Month", "Day", "Country", "Region", "Province","City", "Multiple", "Success", "Suicide", "Attack", "Target", "Nationality", "Group", "Weapon", "Wounded", "Dead")
  
  # NA Treatment #
  
  # Here, we will convert NA values into zeros, so we don't lose info from the rest of the row
  
  GTTDS$Dead[is.na(GTTDS$Dead)] <- 0
  
  # Create Lethal variable as binary (zero and one for no/yes fatal casualties)  
  
  GTTDS <- GTTDS %>% dplyr::mutate(Lethal = if_else(Dead == 0, "0", "1"))
  
  # Create a Decade variable #
  
  GTTDS$Decade[GTTDS$Year >= 1970 & GTTDS$Year <= 1979] <- "1970"
  GTTDS$Decade[GTTDS$Year >= 1980 & GTTDS$Year <= 1989] <- "1980"
  GTTDS$Decade[GTTDS$Year >= 1990 & GTTDS$Year <= 1999] <- "1990"
  GTTDS$Decade[GTTDS$Year >= 2000 & GTTDS$Year <= 2009] <- "2000"
  GTTDS$Decade[GTTDS$Year >= 2010 & GTTDS$Year <= 2019] <- "2010"
  
  # Create a Quarter Variable #
  
  GTTDS$Quarter[GTTDS$Month >= 1 & GTTDS$Month <= 3] <- "FirstQuarter"
  GTTDS$Quarter[GTTDS$Month >= 4 & GTTDS$Month <= 6] <- "SecondQuarter"
  GTTDS$Quarter[GTTDS$Month >= 7 & GTTDS$Month <= 9] <- "ThirdQuarter"
  GTTDS$Quarter[GTTDS$Month >= 10 & GTTDS$Month <= 12] <- "FourthQuarter"
  
  # Create a week variable #
  
  GTTDS$Week[GTTDS$Day >= 1 & GTTDS$Day <= 7] <- "WeekOne"
  GTTDS$Week[GTTDS$Day >= 8 & GTTDS$Day <= 14] <- "WeekTwo"
  GTTDS$Week[GTTDS$Day >= 15 & GTTDS$Day <= 21] <- "WeekThree"
  GTTDS$Week[GTTDS$Day >= 22 & GTTDS$Day <= 31] <- "WeekFour"
  
  # Convert some variables into factor
  
  names <- c("Decade", "Quarter", "Week")
  GTTDS[,names] <- lapply(GTTDS[,names] , factor)

  # Recode Variables
  
  GTTDS <- dplyr::mutate(GTTDS, Attack = dplyr::recode(Attack, "Bombing/Explosion" = "BombAttack", 
                                         "Hostage Taking (Kidnapping)" = "HostageKidnapAttack", 
                                         "Facility/Infrastructure Attack" = "InfrastructureAttack",
                                         "Armed Assault" = "ArmedAssaultAttack",
                                         "Unarmed Assault" = "UnarmedAssaultAttack",
                                         "Hostage Taking (Barricade Incident)" = "HostageBarricadeAttack",
                                         "Unknown" = "UnknownAttack"),
                  Target = dplyr::recode(Target, "Private Citizens & Property" = "Private", 
                                  "Government (Diplomatic)" = "GovtDip", 
                                  "Journalists & Media" = "JournalistsMedia",
                                  "Government (General)" = "GovtGen",
                                  "Airports & Aircraft" = "AirportsAircraft",
                                  "Educational Institution" = "EduIns",
                                  "Violent Political Party" = "VPPTarget",
                                  "Religious Figures/Institutions" = "RelFigIns",
                                  "Unknown" = "UnknownTarget",
                                  "Food or Water Supply" = "FoodWaterSup",
                                  "Terrorists/Non-State Militia" = "TNSMTarget",
                                  "Abortion Related" = "Abortion"),
                  Group = dplyr::recode(Group, "Group.Islamic State of Iraq and the Levant (ISIL)" = "ISIS",
                                 "Tehrik-i-Taliban Pakistan (TTP)" = "TTP",
                                 "Revolutionary Armed Forces of Colombia (FARC)" = "FARC",
                                 "M-19 (Movement of April 19)" = "M19",
                                 "National Liberation Army of Colombia (ELN)" = "ELN",
                                 "Unknown" = "OtherGroup",
                                 "Tupac Amaru Revolutionary Movement (MRTA)" = "MRTA",
                                 "Shining Path (SL)" = "ShiningPath",
                                 "Salafist Group for Preaching and Fighting (GSPC)" = "GSPC",
                                 "Islamic Salvation Front (FIS)" = "FIS",
                                 "Algerian Islamic Extremists" = "Algerian_Islamic_Extremists",
                                 "Al-Qaida in the Islamic Maghreb (AQIM)" = "AQIM",
                                 "Armed Islamic Group (GIA)" = "GIA"),
                  Province = dplyr::recode(Province, "North-West Frontier Province" = "NWFP",
                                    "Federally Administered Tribal Areas" = "FATA",
                                    "Khyber Pakhtunkhwa" = "Khyber_Pakhtunkhwa",
                                    "Al Anbar" = "Al_Anbar",
                                    "Tizi Ouzou" = "Tizi_Ouzou"))
  
}

################
################
# Country Prep #
################
################

# The Country_Prep function processes a dataset to focus on incidents within a specified country, performing the following steps:
#   
#   Select Specific Columns:
#   
#   Selects a subset of columns, including Decade, Year, Quarter, Month, Week, Day, Country, Dead, Lethal, and all columns listed in factor_columns.
# Filter by Country:
#   
#   Filters the dataset to include only rows where the Country column matches the specified Country_Name.
# Lump Factor Levels:
#   
#   Uses the fct_lump_prop function to lump together factor levels in factor_columns that have a proportion less than 0.05 into a single level called 'Other'.
# Recode 'Other' Levels:
#   
#   Recodes the Target, Attack, Group, Weapon, Province, and City columns so that any 'Other' level is renamed to a more descriptive level (e.g., OtherTarget, OtherAttack, etc.).
# Drop the Country Column:
#   
#   Removes the Country column from the dataset, as it is no longer needed after filtering.
# This function prepares the dataset for a specified country, ensuring that minor factor levels are grouped together and making the data more manageable for analysis.

Country_Prep <- function(Data, Country_Name){
  
  Data <- Data %>%
    dplyr::select(c(Decade, Year, Quarter, Month, Week, Day, Country, Dead, Lethal, all_of(factor_columns))) %>%
    filter(Country == Country_Name) %>%
    dplyr::mutate(across(all_of(factor_columns), fct_lump_prop, prop = 0.05, other_level = 'Other')) %>%
    dplyr::mutate(Target = recode(Target, "Other" = "OtherTarget"),
           Attack = recode(Attack, "Other" = "OtherAttack"),
           Group = recode(Group, "Other" = "OtherGroup"),
           Weapon = recode(Weapon, "Other" = "OtherWeapon"),
           Province = recode(Province, "Other" = "OtherProvince"),
           City = recode(City, "Other" = "OtherCity")) %>%
    dplyr::select(-Country)
  
}

