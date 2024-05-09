#---- Info -----
# Goal: corrborate result of the contingency beh study with
# science beliefs study (i.e. a series of linear regression analyses with s
# science beliefs (climate change, GMO, homeopathy as a DV and cultural ideology as a predictor 
# + interation with sophistication)
#
# write 11-04-2024 by Iwona
#--------------------------------------------------------------------------------
#---- Load packages ----
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggeffects)
options(scipen = 999)

#---- Load data in a long format ----
# use the script called '[main study] contingency beh study 01 clean data.R"
# or saved data
#data.long <- read.csv(data contingency beh study clean long data.csv")


#topic
data.long$topic <- as.factor(data.long$topic)
data.long <- data.long %>% 
  mutate(topic = factor(topic, levels = c("hom", "clim", "gmo")))
levels(data.long$topic)


# Remove responses below X seconds
x <- data.long %>% 
  filter(rt >= 20) 
nrow(x)
# >=10 seconds: 1362
# >=15 seconds: 1312
# >=20 seconds: 1256

# remove responses below 10 seconds
data.long <- data.long %>% filter(rt >= 10)


#----Main effect of ideology 
#Climat
r1.climate <- lm(data = data.long, prior.climate.pos ~ age_s_c + gender + educ_s_c + ideology)    
summary(r1.climate)
r1.climate.means <- ggemmeans(r1.climate, terms = "ideology")
plot(r1.climate.means)
car::vif(r1.climate)

#GMO
r1.gmo <- lm(data = data.long, prior.gmo.pos ~ age_s_c + gender + educ_s_c + ideology)    
summary(r1.gmo)
r1.gmo.means <- ggemmeans(r1.gmo, terms = "ideology")
plot(r1.gmo.means)
car::vif(r1.gmo)

#homeopathy 
r1.hom <- lm(data = data.long, prior.hom.pos ~ age_s_c + gender + educ_s_c + ideology)    
summary(r1.hom)
r1.hom.means <- ggemmeans(r1.hom, terms = "ideology")
plot(r1.hom.means)
car::vif(r1.hom)


#Interaction: ideology x num

#Climat
r2.climate <- lm(data = data.long, prior.climate.pos ~ age_s_c + gender + educ_s_c + ideology * num_c)    
summary(r2.climate)
r2.climate %>% ggemmeans(terms = c("num_c", "ideology")) %>% plot()
car::vif(r2.climate)

#GMO
r2.gmo <- lm(data = data.long, prior.gmo.pos ~ age_s_c + gender + educ_s_c + ideology * num_c)    
summary(r2.gmo)
r2.gmo %>% ggemmeans(terms = c("num_c", "ideology")) %>% plot()
car::vif(r2.gmo)

#homeopathy
r2.hom <- lm(data = data.long, prior.hom.pos ~ age_s_c + gender + educ_s_c + ideology * num_c)    
summary(r2.hom)
r2.hom %>% ggemmeans(terms = c("num_c", "ideology")) %>% plot()
car::vif(r2.hom)

#summary
#for polit cult 
sjPlot::tab_model(r1.climate, r1.gmo, r1.hom) 
#for polit cult x num
sjPlot::tab_model(r2.climate, r2.gmo, r2.hom)
