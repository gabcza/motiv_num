#---- Info -----
# Goal: analyze data from contingency beh study 1
##Q2. Do people engage in effortful information processing to reach desirable 
#conclusions in the contingency table task?
#
# write 15-03-2024 by Iwona
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

#---- Load, clean, filter and create long format of the data ----
# use the script called 'contingency beh study 01 clean data.R"

#----ideology----
#----Q2.1. Do people invest more effort when correct response is discordant ----
#with their ideology/priors and when the task is easy vs. difficult? ----
# null model
m0.q2.1.ef.ideology <- lmer(data = data.long, 
                       eff.index ~ 1 + (1|subj.id)) 
summary(m0.q2.1.ef.ideology)
VarCorr(m0.q2.1.ef.ideology) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc) #.71 due to subj

# add argument ideology concordance
m1.q2.1.ef.ideology <- lmer(data = data.long, 
                            eff.index  ~ 1 + ideology.conc + topic + order +
                            (1|subj.id)) 
summary(m1.q2.1.ef.ideology)
anova(m1.q2.1.ef.ideology) #order, topic sig

# add difficulty      
m2.q2.1.ef.ideology <- lmer(data = data.long, 
                  eff.index ~ 1 + ideology.conc + condition.binary + topic +
                  order + (1|subj.id)) 
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
summary(m2.q2.1.ef.ideology)
anova(m3.q2.1.ef.ideology)
m2.q2.1.ef.ideology %>% ggemmeans(terms = "ideology.conc") %>% plot()
m2.q2.1.ef.ideology %>% ggemmeans(terms = "condition.binary") %>% plot()

#add interaction ideology concordence * difficulty  
m3.q2.1.ef.ideology <- lmer(data = data.long %>% filter(ideology.conc != "neutr"), 
                            eff.index ~ 1 + ideology.conc + condition.binary + 
                            ideology.conc*condition.binary + topic +
                            order + (1|subj.id)) 
summary(m3.q2.1.ef.ideology)
anova(m3.q2.1.ef.ideology)
m3.q2.1.ef.ideology  %>% ggemmeans(terms = c("ideology.conc", "condition.binary")) %>% plot()


#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a contingency table? Does it depend on a task difficulty and concordance? ----

#Analysis: We will add cognitive sophistication as main 
#effect and interaction with concordance as well as difficult
m4.q2.2.ef.ideology <- lmer(data = data.long,
                            eff.index ~ 1 + num_c +
                            topic + order + (1|subj.id))

summary(m4.q2.2.ef.ideology) #numeracy istotne (order też)
anova(m4.q2.2.ef.ideology)

#+ int. with ideology
m5.q2.2.ef.ideology <- lmer(data = data.long,
                            eff.index ~ 1 + ideology.conc + condition.binary + num_c +
                              ideology.conc*condition.binary*num_c + 
                              topic + order + (1|subj.id))
                              
summary(m5.q2.2.ef.ideology) #order, numeracy istotne
print(x, correlation = TRUE)
anova(m5.q2.2.ef.ideology)

#----prior corcondance----
#----Q2.1. Do people invest more effort when correct response is discordant 
#with their ideology/priors and when the task is easy vs. difficult? ----
# null model
m0.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                        eff.index ~ 1 + (1|subj.id)) 
summary(m0.q2.1.ef.prior)
VarCorr(m0.q2.1.ef.prior) %>% # get variance components (these are SDs)
  as_tibble() %>%
  mutate(icc = vcov / sum(vcov)) %>%
  dplyr::select(grp, icc) #subj: .71

# add argument ideology concordence
m1.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc  != "neutr"),
                        eff.index  ~ 1 + prior.conc + topic + order + (1|subj.id))
summary(m1.q2.1.ef.prior)
anova(m1.q2.1.ef.prior) #order

# add difficulty
m2.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                            eff.index ~ 1 + prior.conc + condition.binary + topic +
                            order + (1|subj.id))
summary(m2.q2.1.ef.prior)
anova(m3.q2.1.ef.prior)
m2.q2.1.ef.prior %>% ggemmeans(terms = "prior.conc") %>% plot()
m2.q2.1.ef.prior %>% ggemmeans(terms = "condition.binary") %>% plot()

#add interaction ideology concordence * difficulty  
m3.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                        eff.index ~ 1 + prior.conc + condition.binary +
                        prior.conc*condition.binary + topic + order + 
                        (1|subj.id))
summary(m3.q2.1.ef.prior)
anova(m3.q2.1.ef.prior)
m3.q2.1.ef.prior  %>% ggemmeans(terms = c("prior.conc", "condition.binary")) %>% plot()

#Q2.2. Do people high vs. low on cognitive sophistication invest more effort 
#in a contingency table? Does it depend on a task difficulty and concordance? 

#Analysis: We will add cognitive sophistication as main 
#effect and interacting with concordance as well as difficult
m4.q2.2.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         eff.index ~ 1 +  num_c +
                         topic + order + (1|subj.id))
                           
summary(m4.q2.2.ef.prior) #order, numeracy istotne
anova(m4.q2.2.ef.prior)


m5.q2.2.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         eff.index ~ 1 + prior.conc + condition.binary + num_c +
                           prior.conc*condition.binary*num_c + 
                           topic + order + (1|subj.id))
                          
summary(m5.q2.2.ef.prior) #order, numeracy istotne
anova(m5.q2.2.ef.prior)
m5.q2.2.ef.prior  %>% ggemmeans(terms = c("prior.conc", "condition.binary", "num_c")) %>% plot()
#przy większym num, mniejszy effort

#+ add rt as a measure of effort
