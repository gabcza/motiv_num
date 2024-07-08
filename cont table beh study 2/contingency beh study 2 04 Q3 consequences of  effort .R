#---- Info -----
# Goal: analyze data from contingency beh study 2
#Q3. What are the consequences of investing effort for accuracy? 
#
# write 20-03-2024 by Iwona/rewrite 18.06.2024
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
library(multcompView)
library(emmeans)
library(multcomp)


#---- Load data in a long format ----
# use the script called "contingency beh study 2 01 clean data.R"
# or saved data
#data.long <- read.csv("data contingency beh study 2 clean long data.csv")


#topic
data.long$topic <- as.factor(data.long$topic)
data.long <- data.long %>% 
  mutate(topic = factor(topic, levels = c("hom", "clim", "gmo")))
levels(data.long$topic)



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
                             topic + order + (1|subj.id)) 
summary(m2.q3.ef_acc.prior)
anova(m2.q3.ef_acc.prior) 
m2.q3.ef_acc.prior  %>% ggemmeans(terms = c("eff.index", "prior.conc")) %>% plot()


#+difficulty
m3.q3.ef_acc.prior<- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                          resp ~ eff.index * prior.conc * condition.binary +
                            topic + order + (1|subj.id)) 
summary(m3.q3.ef_acc.prior)
anova(m3.q3.ef_acc.prior) 
m3.q3.ef_acc.prior  %>% ggemmeans(terms = c("eff.index", "prior.conc",  "condition.binary")) %>% plot()


#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m4.q3.ef_acc.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                           resp ~ 1 +  eff.index + prior.conc + condition.binary + 
                           eff.index*prior.conc*condition.binary*num_c +
                           topic + order + (1|subj.id)) 
summary(m4.q3.ef_acc.prior)
anova(m4.q3.ef_acc.prior) #diff
#ggemmeans(m4.q3.ef_acc.prior, terms = c("eff.index", "num_c")) %>%
#  plot()

#simple slopes
em_trends <- cld(emtrends(m4.q3.ef_acc.prior, ~ num_c, var = "eff.index",
             at = list(num_c = c(-0.25,0.25))), details = TRUE)
print(em_trends)

m4.q3.ef_acc.prior %>% ggemmeans(c("eff.index", "num_c[meansd]")) %>% plot() 

#---- Concordance with ideology----
#Ideology: self-identification in terms of cultural ideology (progressive vs. conservative)

#Models in which participants with ideology = 4 were excluded are not converge, 
#so models for ideology are calculated in two ways: a) taking into account all 
#observations and b) without the topic of homeopathy and without pp with ideology = 4

#----a) all observations----
m1.q3a.ef_acc.ideology <- lmer(data = data.long, #%>% filter(ideology != 4), # GC: dodajemy filtrowanie ideology != 4 (?)
                              resp ~ 1 +  eff.index +
                              topic + order + (1|subj.id)) 

summary(m1.q3a.ef_acc.ideology)
anova(m1.q3a.ef_acc.ideology) 
m1.q3a.ef_acc.ideology %>%
  ggemmeans(terms = "eff.index") %>%
  plot()

#add interaction
m2.q3a.ef_acc.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                              resp ~ eff.index * ideology.conc +
                              topic + order + (1|subj.id)) 

summary(m2.q3a.ef_acc.ideology)
anova(m2.q3a.ef_acc.ideology) 
m2.q3a.ef_acc.ideology  %>% ggpredict(terms = c("eff.index", "ideology.conc")) %>% plot()

#+difficulty
m3.q3a.ef_acc.ideology<- lmer(data = data.long, #%>% filter(ideology != 4), 
                             resp ~ eff.index * ideology.conc * condition.binary +
                               topic + order + (1|subj.id)) 
summary(m3.q3a.ef_acc.ideology) 
anova(m3.q3a.ef_acc.ideology) 
m3.q3a.ef_acc.ideology  %>% ggpredict(terms = c("eff.index","ideology.conc", "condition.binary")) %>% plot()

#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m4.q3a.ef_acc.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                              resp ~ eff.index * ideology.conc * condition.binary * num_c +
                              topic + order + (1|subj.id)) 

summary(m4.q3a.ef_acc.ideology)
anova(m4.q3a.ef_acc.ideology)
#ggemmeans(m4.q3.ef_acc.ideology, terms = c("eff.index", "num_c")) %>%
  #plot()
m4.q3a.ef_acc.ideology %>% ggemmeans(c("eff.index", "num_c[meansd]")) %>% plot() 

#ggpredict(m4.q3a.ef_acc.ideology, terms = c("num_c", "ideology.conc")) %>%
#  plot()

#simple slopes
em_trends <- cld(emtrends(m4.q3a.ef_acc.ideology, ~ num_c, var = "eff.index",
                          at = list(num_c = c(-0.25,0.25))), details = TRUE)
print(em_trends)

#---- b)without the homeopathy topic and without pp with ideology = 4 ----

m1.q3b.ef_acc.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                               %>% filter(topic != "hom"),
                               resp ~ 1 +  eff.index +
                                 topic + order + (1|subj.id)) 

summary(m1.q3b.ef_acc.ideology)
anova(m1.q3b.ef_acc.ideology) 
m1.q3b.ef_acc.ideology %>%
  ggemmeans(terms = "eff.index") %>%
  plot()

#add interaction
m2.q3b.ef_acc.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                               %>% filter(topic != "hom"),
                               resp ~ eff.index * ideology.conc +
                                 topic + order + (1|subj.id)) 

summary(m2.q3b.ef_acc.ideology)
anova(m2.q3b.ef_acc.ideology) 
m2.q3b.ef_acc.ideology  %>% ggpredict(terms = c("eff.index", "ideology.conc")) %>% plot()

#+difficulty
m3.q3b.ef_acc.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                               %>% filter(topic != "hom"),
                              resp ~ eff.index * ideology.conc * condition.binary +
                                topic + order + (1|subj.id)) 
summary(m3.q3b.ef_acc.ideology) 
anova(m3.q3b.ef_acc.ideology) 
m3.q3b.ef_acc.ideology  %>% ggpredict(terms = c("eff.index","ideology.conc", "condition.binary")) %>% plot()

#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m4.q3b.ef_acc.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                               %>% filter(topic != "hom"),
                               resp ~ eff.index * ideology.conc * condition.binary * num_c +
                                topic + order + (1|subj.id)) 

summary(m4.q3b.ef_acc.ideology)
anova(m4.q3b.ef_acc.ideology)
#ggemmeans(m4.q3.ef_acc.ideology, terms = c("eff.index", "num_c")) %>%
#plot()
m4.q3b.ef_acc.ideology %>% ggemmeans(c("eff.index", "num_c[meansd]")) %>% plot() 

ggpredict(m4.q3b.ef_acc.ideology, terms = c("num_c", "ideology.conc")) %>%
  plot()

#----Effort as RT for priors----

#add log RT as a variable (if it is not added)
data.long <- data.long %>% mutate(rt_log = log(rt))

#---- Q3a. Does effort investment lead to more accurate responding? Does it depend on concordance?----
#---- Concordance with priors----
# main effect of effort on accuracy of responding
m1.q3.ef_acc.prior_rt <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                              resp ~ rt_log +
                                topic + order + (1|subj.id)) #boundary (singular) fit:
summary(m1.q3.ef_acc.prior_rt)
anova(m1.q3.ef_acc.prior_rt)
m1.q3.ef_acc.prior_rt %>%
  ggemmeans(terms = "rt_log") %>%
  plot()

#add interaction with concordance
m2.q3.ef_acc.prior_rt <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                              resp ~ rt_log * prior.conc +
                                topic + order + (1|subj.id)) 
summary(m2.q3.ef_acc.prior_rt)
anova(m2.q3.ef_acc.prior_rt) 
m2.q3.ef_acc.prior_rt  %>% ggemmeans(terms = c("rt_log", "prior.conc")) %>% plot()

#simple_effects <- emmeans(m2.q3.ef_acc.prior_rt, ~ rt_log | prior.conc)
simple_effects <- cld(emtrends(m2.q3.ef_acc.prior_rt, ~ prior.conc, var ="rt_log"), details = T)
# Print the simple effects
print(simple_effects)


#+difficulty
m3.q3.ef_acc.prior_rt<- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                             resp ~ rt_log * prior.conc * condition.binary +
                               topic + order + (1|subj.id)) 
summary(m3.q3.ef_acc.prior_rt)
anova(m3.q3.ef_acc.prior_rt) 
m3.q3.ef_acc.prior_rt  %>% ggemmeans(terms = c("rt_log", "prior.conc",  "condition.binary")) %>% plot()



#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m4.q3.ef_acc.prior_rt <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                              resp ~ 1 +  rt_log + prior.conc + condition.binary + 
                                rt_log*prior.conc*condition.binary*num_c +
                                topic + order + (1|subj.id)) 
summary(m4.q3.ef_acc.prior_rt)
anova(m4.q3.ef_acc.prior_rt) #diff
#ggemmeans(m4.q3.ef_acc.prior, terms = c("rt_log", "num_c")) %>%
#  plot()

m4.q3.ef_acc.prior_rt %>% ggemmeans(c("rt_log", "num_c[meansd]")) %>% plot() 


simple_effects <- cld(emtrends(m4.q3.ef_acc.prior_rt, ~ num_c, var = "rt_log", details = T,
                               at = list(num_c = c(-0.25,0.25))), details = TRUE)

print(simple_effects)

#----Effort as RT for ideology----
#----a) all observations----
m1.q3a.ef_acc.ideology_rt <- lmer(data = data.long, #%>% filter(ideology != 4), # GC: dodajemy filtrowanie ideology != 4 (?)
                                   resp ~ 1 +  rt_log +
                                     topic + order + (1|subj.id)) 

summary(m1.q3a.ef_acc.ideology_rt)
anova(m1.q3a.ef_acc.ideology_rt) 
m1.q3a.ef_acc.ideology_rt %>%
  ggemmeans(terms = "rt_log") %>%
  plot()

#add interaction
m2.q3a.ef_acc.ideology_rt <- lmer(data = data.long, #%>% filter(ideology != 4),
                                   resp ~ rt_log * ideology.conc +
                                     topic + order + (1|subj.id)) 

summary(m2.q3a.ef_acc.ideology_rt)
anova(m2.q3a.ef_acc.ideology_rt) 
 
#+difficulty
m3.q3a.ef_acc.ideology_rt<- lmer(data = data.long, #%>% filter(ideology != 4), 
                                  resp ~ rt_log * ideology.conc * condition.binary +
                                    topic + order + (1|subj.id)) 
summary(m3.q3a.ef_acc.ideology_rt) 
anova(m3.q3a.ef_acc.ideology_rt) 
m3.q3a.ef_acc.ideology_rt  %>% ggpredict(terms = c("rt_log","ideology.conc", "condition.binary")) %>% plot()

#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m4.q3a.ef_acc.ideology_rt <- lmer(data = data.long, #%>% filter(ideology != 4),
                                   resp ~ rt_log * ideology.conc * condition.binary * num_c +
                                     topic + order + (1|subj.id)) 

summary(m4.q3a.ef_acc.ideology_rt)
anova(m4.q3a.ef_acc.ideology_rt)
#ggemmeans(m4.q3.ef_lacc.ideology_rt, terms = c("rt_log", "num_c")) %>%
#plot()
m4.q3a.ef_acc.ideology_rt %>% ggemmeans(c("rt_log", "num_c[meansd]")) %>% plot() 

#----b) without homeopathy and without ideology = 4----

m1.q3b.ef_acc.ideology_rt <- lmer(data = data.long %>% filter(ideology != 4) 
                                 %>% filter(topic != "hom"),
                                  resp ~ 1 +  rt_log +
                                    topic + order + (1|subj.id)) 

summary(m1.q3b.ef_acc.ideology_rt)
anova(m1.q3b.ef_acc.ideology_rt) 
m1.q3b.ef_acc.ideology_rt %>%
  ggemmeans(terms = "rt_log") %>%
  plot()

#add interaction
m2.q3b.ef_acc.ideology_rt <- lmer(data = data.long %>% filter(ideology != 4) 
                                  %>% filter(topic != "hom"),
                                  resp ~ rt_log * ideology.conc +
                                    topic + order + (1|subj.id)) 

summary(m2.q3b.ef_acc.ideology_rt)
anova(m2.q3b.ef_acc.ideology_rt) 

#+difficulty
m3.q3b.ef_acc.ideology_rt<- lmer(data = data.long %>% filter(ideology != 4) 
                                %>% filter(topic != "hom"),
                                 resp ~ rt_log * ideology.conc * condition.binary +
                                   topic + order + (1|subj.id)) 
summary(m3.q3b.ef_acc.ideology_rt) 
anova(m3.q3b.ef_acc.ideology_rt) 
m3.q3b.ef_acc.ideology_rt  %>% ggpredict(terms = c("rt_log","ideology.conc", "condition.binary")) %>% plot()

#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m4.q3b.ef_acc.ideology_rt <- lmer(data = data.long %>% filter(ideology != 4) 
                                 %>% filter(topic != "hom"),
                                  resp ~ rt_log * ideology.conc * condition.binary * num_c +
                                    topic + order + (1|subj.id)) 

summary(m4.q3b.ef_acc.ideology_rt)
anova(m4.q3b.ef_acc.ideology_rt)
#ggemmeans(m4.q3.ef_lacc.ideology_rt, terms = c("rt_log", "num_c")) %>%
#plot()
m4.q3b.ef_acc.ideology_rt %>% ggemmeans(c("rt_log", "num_c[meansd]")) %>% plot() 



