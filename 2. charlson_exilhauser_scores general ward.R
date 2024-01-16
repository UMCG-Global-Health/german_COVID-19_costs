
df <- read.csv("./data/cleaned_GUF_general_ward.csv")
colnames(df)[1] <- "id"
df_copy <- df



library(comorbidity)
library(dplyr)
library(tidyr)
df <- df[c("id", "ICD")]


df <- separate_rows(df, ICD, convert = TRUE)

charlson <- comorbidity(x = df, id = "id", code = "ICD", score = "charlson", assign0 = FALSE)
colnames(charlson)[c(19,20)] <- c("charlson_score", "charlson_index")

elixhauser <- comorbidity(x = df, id = "id", code = "ICD", score = "elixhauser", assign0 = FALSE)
elixhauser_copy <- elixhauser

colnames(elixhauser)[c(33,34)] <- c("elixhauser_score", "elixhauser_index")
double_vars <- c("chf","pvd", "pud", "aids", "metacanc", "rheumd")

vars <- names(charlson) %in% double_vars
charlson<- charlson[!vars]

df <- left_join(elixhauser, charlson, by= "id")

cor(df$charlson_score, df$elixhauser_score)

df_final <- left_join(df_copy, df, by= "id")


df_final$intra_cerbral_bleeding <- grepl(("I60.x|I61.x|I62.x"), ignore.case = T , df_copy$ICD) 

df_final$stroke <- grepl("I63|I64", df_copy$ICD, ignore.case = T)

df_final$pe <- grepl("I26.x", df_copy$ICD, ignore.case =  T) 

df_final$embol_thromb <- grepl("I74.x", df_copy$ICD, ignore.case = T) 

df_final$myo_infar <- grepl("I21.|I22.|I24.", df_copy$ICD, ignore.case = T)  

df_final$cpr <- grepl("8-771|8-772|8-779", df_copy$OPS, ignore.case = T)

df_final$dialysis <- grepl("8-853.|8-854.|8-855.", df_copy$OPS, ignore.case = T ) 



write.csv(df_final,"./data/df_final_general_ward.csv")


write.csv(elixhauser_copy, "./data/elixahauser_general_ward.csv")

write.csv(charlson, "./data/charlson_general_ward.csv")


#pud.x and pud.y not equal, same for rheumd

###stats 
skim(df_final)
skim(elixhauser)

elix <- elixhauser %>%  mutate(hypertension = ifelse((hypc ==1 | hypunc ==1), 1,0)) 
skim(elix$hypertension)

elix <- elixhauser %>%  mutate(diabet = ifelse((diabc ==1 | diabunc ==1), 1,0)) 
skim(elix$diabet)

