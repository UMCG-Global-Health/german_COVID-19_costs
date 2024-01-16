library(tidyverse)
library(survey)
library(CBPS)
library(cobalt)

set.seed(123)

##estimated models
#propensity scores gbm boosted

df <- read.csv("./data/df_final_ICU.csv")

df$complication <- df$pe + df$myo_infar + df$stroke + df$intra_cerbral_bleeding + df$embol_thromb

##CBPS for cobalt https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html#using-cobalt-with-continuous-treatments

#CBPS propensity model 

prop <- CBPS(Mech_vent_dur_days ~ elixhauser_score + Age + sex, data = df, method = "exact")

bal.tab(prop)
bal.plot(prop, var.name = "elixhauser_score", which= "both")
bal.plot(prop, var.name = "Age", which= "both")
bal.plot(prop, "sex", which = "both")
love.plot(prop, thresholds = c(m = .1))


glm_gaus_id <- glm(Sum_IK ~ General_ward + los_ICU_non_mech + 
                         Mech_vent_dur_days + ECMO_dur_days +
                         elixhauser_score + Outcome  + sex + Age +  complication +dialysis +cpr,
                       data=df, 
                       family = gaussian("identity"), weights = prop$weights)
summary(glm_gaus_id) 

AIC(glm_gaus_id)
BIC(glm_gaus_id)

#GLM with link log and Gaussian
glm_gaus_log <- glm(Sum_IK ~ General_ward + los_ICU_non_mech + 
                      Mech_vent_dur_days + ECMO_dur_days +
                      elixhauser_score + Outcome  + sex + Age +  complication +dialysis +cpr,
                    data=df, 
                    family = gaussian("log"), weights =  prop$weights)

summary(glm_gaus_log)

AIC(glm_gaus_log) 
BIC(glm_gaus_log)

#GLM with gamma and link is id


glm_gamma_id <- glm(Sum_IK ~ General_ward + los_ICU_non_mech + 
                           Mech_vent_dur_days + ECMO_dur_days +
                           elixhauser_score + Outcome  + sex + Age +  complication +dialysis +cpr , 
                    data=df, 
                    family = Gamma("identity"),
                    maxit = 40, weights= prop$weights)  

summary(glm_gamma_id)

AIC(glm_gamma_id)
BIC(glm_gamma_id)

glm_gamma_log <- glm(Sum_IK ~ General_ward + los_ICU_non_mech + 
                       Mech_vent_dur_days + ECMO_dur_days +
                       elixhauser_score + Outcome  + sex + Age +  complication +dialysis +cpr, data = df,
                     family = Gamma("log"), weights = prop$weight)

summary(glm_gamma_log)

AIC(glm_gamma_log) 
BIC(glm_gamma_log)


glm_inverse_gaussian_id <- glm(Sum_IK ~ General_ward + los_ICU_non_mech + 
                                 Mech_vent_dur_days + ECMO_dur_days +
                                 elixhauser_score + Outcome  + sex + Age +  complication +dialysis +cpr,
                        data=df, 
                        family = inverse.gaussian("identity"), weights = prop$weight)

summary(glm_inverse_gaussian_id)
AIC(glm_inverse_gaussian_id) 
BIC(glm_inverse_gaussian_id) 


glm_inverse_gaussian_log <- glm(Sum_IK ~ General_ward + los_ICU_non_mech + 
                                  Mech_vent_dur_days + ECMO_dur_days +
                                  elixhauser_score + Outcome  + sex + Age +  complication +dialysis +cpr,
                               data=df, 
                               family = inverse.gaussian("log"), weights = prop$weight)

summary(glm_inverse_gaussian_log)
AIC(glm_inverse_gaussian_log) 
BIC(glm_inverse_gaussian_log)

#simulations dharma resiudals

library(DHARMa)


# DHARMa residuals
plot(simulateResiduals(glm_gaus_id, n= 1000))

plot(simulateResiduals(glm_gaus_log, n = 1000))

plot(simulateResiduals(glm_inverse_gaussian_id, n= 1000))
simout <- simulateResiduals(glm_inverse_gaussian_id, n=1000)
plotQQunif(simout, testUniformity = F, testDispersion =  F, testOutliers = F, main = 'c')

ggsave("./figures/icu_inv_gaus_id.tiff", width= 9, height = 6, dpi= 600)

plot(simulateResiduals(glm_inverse_gaussian_log, n = 1000))
simout <- simulateResiduals(glm_inverse_gaussian_log, n=1000)
plotQQunif(simout, testUniformity = F, testDispersion =  F, testOutliers = F, main = 'd')

ggsave("./figures/icu_inv_gaus_log.tiff", width= 9, height = 6, dpi= 600)

plot(simulateResiduals(glm_gamma_id, n=1000))
simout <- simulateResiduals(glm_gamma_id, n=1000)
plotQQunif(simout, testUniformity = F, testDispersion =  F, testOutliers = F, main = 'a')

ggsave("./figures/icu_gamma_id.tiff", width= 9, height = 6, dpi= 600)

plot(simulateResiduals(glm_gamma_log, n = 1000))
simout <- simulateResiduals(glm_gamma_log, n=1000)
plotQQunif(simout, testUniformity = F, testDispersion =  F, testOutliers = F, main = 'b')

ggsave("./figures/icu_gamma_log.tiff", width= 9, height = 6, dpi= 600)

