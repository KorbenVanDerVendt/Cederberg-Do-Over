setwd('C:/Users/Korben/Desktop/Cederberg-Project')

install.packages("readxl")
library(readxl)
library(tidyr)
library(dplyr)
## CEDERBERG REDO SCRIPT 

#READING IN THERMOPHILUM AND STENOCARA DATA 
T.dec <- read_excel('thermophyllm data.xlsx')
#head(T.dec)
# Convert multiple columns to factors
cols_to_convert <- c("Slope", "Sex", "SexCode", "Year", "Site", "AltCode")
T.dec <- T.dec %>%
  mutate_at(vars(cols_to_convert), as.factor)  #dplyr
#head(T.dec)
# Get the names of columns with missing values
cols_with_missing_values <- names(which(colSums(is.na(T.dec)) > 0))
# Get the row number with missing 'EL' value (important variable)
row_with_missing_EL <- which(is.na(T.dec$EL))
#Now removing that row
T.dec <- T.dec[-row_with_missing_EL, ]
#missing values for altitude 
row_with_missing_ALT <- which(is.na(T.dec$Altitude))
#this code fills in altitude values depending on the value for site
T.dec <- T.dec %>%      ###dplyr and tidyr functions
  group_by(Site) %>% 
  fill(Altitude)
#getting row that still dont have values for altitude 
row_with_missing_ALT <- which(is.na(T.dec$Altitude))
altcode_with_missing_ALT<- T.dec$AltCode[row_with_missing_ALT]
# Calculate mean altitude for each AltCode
mean_altitudes <- T.dec %>%
  group_by(AltCode) %>%
  summarise(mean_altitude = mean(Altitude, na.rm = TRUE))
mean_altitudes
# Fill missing Altitude values with mean Altitude based on AltCode
T.dec <- T.dec %>%
  left_join(mean_altitudes, by = "AltCode") %>%
  mutate(Altitude = ifelse(is.na(Altitude), mean_altitude, Altitude)) %>%
  select(-mean_altitude)
#checking again which rows do not have altitude values
row_with_missing_ALT <- which(is.na(T.dec$Altitude))
### still need altitude values for site 15
  
S.den <- read_excel('stenacarra data.xlsx')




T.dec$Species<- "T.dec"
S.den$Species<- "S.den"





