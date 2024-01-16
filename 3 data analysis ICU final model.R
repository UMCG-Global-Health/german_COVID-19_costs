library(tidyverse)
library(CBPS)
library(cobalt)
library(sandwich)
library(DHARMa)
library(regclass)

set.seed(123)

df <- read.csv("./data/df_final_ICU.csv")
df$complication <- df$pe + df$myo_infar + df$stroke + df$intra_cerbral_bleeding + df$embol_thromb

prop <- CBPS(Mech_vent_dur_days ~ elixhauser_score + Age + sex , data = df, method = "exact")


bal.tab(prop)

bal.plot(prop, var.name = "elixhauser_score", which = "both")
bal.plot(prop, var.name = "Age", which= "both")
bal.plot(prop, "sex", which = "both")


glm_gamma_id <- glm(Sum_IK ~ General_ward + los_ICU_non_mech + 
                      Mech_vent_dur_days + ECMO_dur_days  + elixhauser_score +
                       Outcome + Age +sex   + complication + dialysis +cpr, 
                    data=df, 
                    family = Gamma("identity"),
                    maxit = 40, weights = prop$weights) 
summary(glm_gamma_id)


VIF(glm_gamma_id)



simout  <-  simulateResiduals(glm_gamma_id, n = 1000)
plotSimulatedResiduals(simout) 

out1 <- outliers(simout, lowerQuantile = 0, upperQuantile = 1,
         return = c("logical"))
out2 <- outliers(simout, lowerQuantile = 0.01, upperQuantile = 0.99,
                 return = c("logical"))
out3 <- outliers(simout, lowerQuantile = 0.025, upperQuantile = 0.975,
                 return = c("logical"))

df$drop1 <- out1
df_1 <- df %>%  
  filter(drop1 == FALSE)

prop <- CBPS(Mech_vent_dur_days ~ elixhauser_score + Age + sex , data = df_1, method = "exact")


bal.tab(prop)


bal.plot(prop, var.name = "elixhauser_score", which = "both")
bal.plot(prop, var.name = "Age", which= "both")
bal.plot(prop, "sex", which = "both")

love.plot(prop, thresholds = c(m = .1))

glm_gamma_id_new_1 <- glm(Sum_IK ~ General_ward + los_ICU_non_mech + 
                      Mech_vent_dur_days + ECMO_dur_days  + elixhauser_score +
                      Outcome + Age +sex   + complication + dialysis +cpr, 
                    data=df_1, 
                    family = Gamma("identity"),
                    maxit = 40, weights = prop$weights) # + sex + Age ??
summary(glm_gamma_id_new_1)#10294

VIF(glm_gamma_id_new_1)

simout  <-  simulateResiduals(glm_gamma_id_new_1, n = 1000)
plotSimulatedResiduals(simout) 
