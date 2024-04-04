#---- Info -----
# Goal: analyze data from contingency beh study 1
#Q3. What are the consequences of investing effort for accuracy? 
#
# write 20-03-2024 by Iwona
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
# use the script called 'contingency beh study 01 clean data.R"
# or saved data
#data.long <- read.csv(data contingency beh study pilot clean long data.csv")

#---- Q3a. Does effort investment lead to more accurate responding? Does it depend on concordance?----
#---- Concordance with priors----
# main effect of effort on accuracy of responding
m1.q3.ef_acc.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                           resp ~ eff.index +
                           topic + order + (1|subj.id)) #boundary (singular) fit:
summary(m1.q3.ef_acc.prior)
anova(m1.q3.ef_acc.prior)
m1.q3.ef_acc.prior %>%
  ggemmeans(terms = "eff.index") %>%
  plot()

#add interaction with concordance
m2.q3.ef_acc.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                           resp ~ eff.index * prior.conc +
                             topic + order + (1|subj.id)) #boundary (singular) fit:
summary(m2.q3.ef_acc.prior)
anova(m2.q3.ef_acc.prior) #prior istotny, interakcja trochę też
m2.q3.ef_acc.prior  %>% ggemmeans(terms = c("prior.conc", "eff.index")) %>% plot()

#+difficulty
m3.q3.ef_acc.prior<- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                          resp ~ eff.index * prior.conc * condition.binary +
                            topic + order + (1|subj.id)) #boundary (singular) fit + rescaling
summary(m3.q3.ef_acc.prior)
anova(m3.q3.ef_acc.prior) 
m3.q3.ef_acc.prior  %>% ggemmeans(terms = c("prior.conc", "eff.index", "condition.binary")) %>% plot()

#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m4.q3.ef_acc.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                           resp ~ 1 +  eff.index + prior.conc + condition.binary + 
                           eff.index*prior.conc*condition.binary:num_c +
                           topic + order + (1|subj.id)) #boundary (singular) fit + rescaling
summary(m4.q3.ef_acc.prior)
anova(m4.q3.ef_acc.prior) #diff

#---- Concordance with ideology----
m1.q3.ef_acc.ideology <- lmer(data = data.long %>% filter(ideology != 4), # GC: dodajemy filtrowanie ideology != 4 (?)
                              resp ~ 1 +  eff.index +
                              topic + order + (1|subj.id)) 
#boundary (singular) fit: see help('isSingular')
summary(m1.q3.ef_acc.ideology)
anova(m1.q3.ef_acc.ideology) 
m2.q3.ef_acc.ideology %>%
  ggemmeans(terms = "eff.index") %>%
  plot()

#add interaction
m2.q3.ef_acc.ideology <- lmer(data = data.long %>% filter(ideology != 4),
                              resp ~ eff.index * ideology.conc +
                              topic + order + (1|subj.id)) 
#boundary (singular) fit: see help('isSingular')
summary(m1.q3.ef_acc.ideology)
anova(m1.q3.ef_acc.ideology) 
m2.q3.ef_acc.ideology  %>% ggemmeans(terms = c("ideology.conc", "eff.index")) %>% plot()

#+difficulty
m3.q3.ef_acc.ideology<- lmer(data = data.long %>% filter(ideology != 4), 
                             resp ~ eff.index * ideology.conc * condition.binary +
                               topic + order + (1|subj.id)) 
summary(m3.q3.ef_acc.ideology) 
anova(m3.q3.ef_acc.ideology) #effort sig
m2.q3.ef_acc.ideology  %>% ggemmeans(terms = c("ideology.conc", "eff.index", "diff")) %>% plot()

#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m4.q3.ef_acc.ideology <- lmer(data = data.long %>% filter(ideology != 4),
                              resp ~ eff.index * ideology.conc * condition.binary * num_c +
                              topic + order + (1|subj.id)) 
#boundary (singular) fit: see help('isSingular')
summary(m4.q3.ef_acc.ideology)
anova(m4.q3.ef_acc.ideology)


