library(CBPS)
library(lmtest) #for model specification
library(regclass)
library(cobalt) 
library(boot)
library(DHARMa)
library(SuppDists)

set.seed(123)

df <- read.csv("./data/df_final_general_ward.csv")
df$complication <- df$pe + df$myo_infar + df$stroke + df$intra_cerbral_bleeding + df$embol_thromb

prop <- CBPS(General_ward ~ elixhauser_score + Age, data = df, method = "exact")

bal.tab(prop)

bal.plot(prop, var.name = "elixhauser_score", which = "both")
bal.plot(prop, var.name = "Age", which= "both")

love.plot(prop, thresholds = c(m = .1))


glm_inverse_gaussian_id <- glm(Sum_IK ~ length.of.stay + Outcome + elixhauser_score + Age +sex + cpr +dialysis + complication  , 
                               data =df, family= inverse.gaussian("identity"), 
                               weights =  prop$weights)
summary(glm_inverse_gaussian_id)

simout  <-  simulateResiduals(glm_inverse_gaussian_id ,n=1000)
plotSimulatedResiduals(simout) 

out1 <- outliers(simout, lowerQuantile = 0, upperQuantile =1,
         return = c("logical"))
df$drop1 <- out1
df_1 <- df %>%  
  filter(drop1 == FALSE)


prop <- CBPS(General_ward ~ elixhauser_score  + Age , data = df_1, method = "exact")

bal.tab(prop)

bal.plot(prop, var.name = "elixhauser_score", which = "both")
bal.plot(prop, var.name = "Age", which= "both")

love.plot(prop, thresholds = c(m = .1))


glm_inverse_gaussian_id_1 <- glm(Sum_IK ~ length.of.stay + Outcome + elixhauser_score + Age +sex + cpr +dialysis + complication , 
                               data =df_1, family= inverse.gaussian("identity"), 
                               weights =  prop$weights)
summary(glm_inverse_gaussian_id_1)


simout  <-  simulateResiduals(glm_inverse_gaussian_id_1 ,n=1000)
plotSimulatedResiduals(simout) 
