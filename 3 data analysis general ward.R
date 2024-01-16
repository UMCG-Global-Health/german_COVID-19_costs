library(CBPS)
library(lmtest) 
library(regclass)
library(cobalt) 
library(boot)

set.seed(123)

df <- read.csv("./data/df_final_general_ward.csv")

df$complication <- df$pe + df$myo_infar + df$stroke + df$intra_cerbral_bleeding + df$embol_thromb


prop <- CBPS(General_ward ~ elixhauser_score + Age  , data = df, method = "exact")

bal.tab(prop)

bal.plot(prop, var.name = "elixhauser_score", which = "both")
bal.plot(prop, var.name = "Age", which= "both")

love.plot(prop, thresholds = c(m = .1))


glm_gaus_id <- glm(Sum_IK ~ length.of.stay + Outcome + elixhauser_score + Age +sex + cpr +dialysis +complication  ,
                       data=df, 
                       family = gaussian("identity"),
                       weights =  prop$weights)
summary(glm_gaus_id) 

AIC(glm_gaus_id) 
BIC(glm_gaus_id) 

#GLM with link log and Gaussian
glm_gaus_log <- glm(Sum_IK ~ length.of.stay + Outcome + elixhauser_score + Age +sex + cpr +dialysis + complication ,
                    data=df, 
                    family = gaussian("log"),
                    weights =  prop$weights)

summary(glm_gaus_log)


AIC(glm_gaus_log) 
BIC(glm_gaus_log) 

#GLM with gamma and link is id

glm_gamma_id <- glm(Sum_IK ~ length.of.stay + Outcome + elixhauser_score + Age +sex + cpr +dialysis + complication  ,
                    data=df, 
                    family = Gamma("identity"),
                    maxit = 40, 
                    weights =  prop$weights)


summary(glm_gamma_id)

AIC(glm_gamma_id) 
BIC(glm_gamma_id) 

library(regclass)
VIF(glm_gamma_id)

glm_gamma_log <- glm(Sum_IK ~ length.of.stay + Outcome + elixhauser_score + Age +sex + cpr +dialysis + complication ,
                     data=df, 
                     family = Gamma("log"), 
                     weights =  prop$weights)

summary(glm_gamma_log)

AIC(glm_gamma_log) 
BIC(glm_gamma_log) 

glm_inverse_gaussian_id <- glm(Sum_IK ~ length.of.stay + Outcome + elixhauser_score + Age +sex + cpr +dialysis + complication  , 
                               data =df, family= inverse.gaussian("identity"), 
                               weights =  prop$weights)
summary(glm_inverse_gaussian_id)

AIC(glm_inverse_gaussian_id) 
BIC(glm_inverse_gaussian_id) 

glm_inverse_gaussian_log <- glm(Sum_IK ~ length.of.stay + Outcome + elixhauser_score + Age +sex + cpr +dialysis + complication  , 
                               data =df, family= inverse.gaussian("log"), 
                               weights =  prop$weights)
summary(glm_inverse_gaussian_log)

AIC(glm_inverse_gaussian_log) 
BIC(glm_inverse_gaussian_log)

library(DHARMa)

simout  <-  simulateResiduals(glm_gaus_id, n=1000)
plotSimulatedResiduals(simout) 

simout  <-  simulateResiduals(glm_gaus_log ,n=1000)
plotSimulatedResiduals(simout) 

simout  <-  simulateResiduals(glm_gamma_log ,n=1000)
plotSimulatedResiduals(simout) 
plotQQunif(simout,testUniformity =  F , testDispersion = F , testOutliers = F, main = 'b')

ggsave("./figures/nonicu_gamma_log.tiff", width= 14, height = 17, dpi= 600)

simout  <-  simulateResiduals(glm_gamma_id ,n=1000)
plotSimulatedResiduals(simout) 
plotQQunif(simout, testUniformity =  F , testDispersion = F , testOutliers = F, main = 'a')

ggsave("./figures/nonicu_gamma_id.tiff", width= 14, height = 17, dpi= 600)

simout  <-  simulateResiduals(glm_inverse_gaussian_log ,n=1000)
plotSimulatedResiduals(simout) 
plotQQunif(simout, testUniformity =  F , testDispersion = F , testOutliers = F, main= 'd')
ggsave("./figures/nonicu_inverse_gaussian_log.tiff", width= 14, height = 17, dpi= 600)

simout  <-  simulateResiduals(glm_inverse_gaussian_id ,n=1000)
plotSimulatedResiduals(simout) 
plotQQunif(simout, testUniformity =  F , testDispersion = F , testOutliers = F, main = 'c')
ggsave("./figures/nonicu_inverse_gaussian_id.tiff", width= 14, height = 17, dpi= 600)