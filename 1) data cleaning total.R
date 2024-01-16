library(openxlsx)
library(tidyverse)
library(dplyr)
library(skimr)
library(corrplot)

setwd("C:/Users/lrzwe/Desktop/Envision_nobackup/costs/Germany/GUF")
#read data
df <-read.xlsx( "./data/Cost_2020_2021_final_anonym.xlsx")

#colnames in one format
colnames(df)

colnames(df)[6] <- "ICU.length.of.stay.days"
colnames(df)[7] <-"ICU.length.of.stay.hours"
colnames(df)[8] <- "Mech_vent_dur"
colnames(df)[13] <- "Sum_IK"
colnames(df)[14] <- "Total_fee_all_services"
colnames(df)[15] <-"Age"
colnames(df)[16] <- "ECMO_dur"
#colnames(df)[19] <- "Sex_code"  #what is this?
colnames(df)[22] <- "Outcome"


##some exploration

skim(df)

#create factor variables or numerical when needed
df$Admission_year <- as.factor(df$Admission_year)

df$sex <- as.factor(df$sex)

df$ICU.length.of.stay.hours <- as.numeric(df$ICU.length.of.stay.hours)

df$Mech_vent_dur <- as.numeric(df$Mech_vent_dur)

df$ECMO_dur <- as.numeric(df$ECMO_dur)

df$sex_dummy <- case_when(df$sex == "m" ~ 1, df$sex == "w" ~ 0)


#change missing values into 0 for ecmo and mech vent

df <- df %>% 
  mutate_at(vars(contains("dur")), ~replace(., is.na(.), 0))
df <- df %>% 
  mutate_at(vars(contains("ICU")), ~replace(., is.na(.), 0))


#transform ICD and OPS codes into number of ICD codes and number of OPS codes 
#note might still contain duplicates
df$OPS_number <- count.fields(textConnection(df$OPS), sep = ",")
df$ICD_number <- count.fields(textConnection(df$ICD), sep = ",")

#vars <- names(df) %in% c("ICU.length.of.stay.hours", "ICU.length.of.stay.days", "Mech_vent_dur", "ECMO_dur")
#df <- df[!vars]

#create general ward variable 

df <- df %>%  
  mutate(General_ward = length.of.stay -  ICU.length.of.stay.days  ) 

df$General_ward[df$General_ward<0] <- 0

#create non mech days 

df <- df %>%  
  mutate(los_ICU_non_mech = (ICU.length.of.stay.hours - Mech_vent_dur)/24)

df <- df %>% 
  mutate(Mech_vent_dur_days = Mech_vent_dur/24 )

df$los_ICU_non_mech[df$los_ICU_non_mech<0] <- 0

skim(df)

df <- df %>%  filter(Age >17)

write.csv(df, "./data/cleaned_GUF_total.csv")

