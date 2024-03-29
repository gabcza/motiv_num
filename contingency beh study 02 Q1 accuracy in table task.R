#---- Info -----
# Goal: analyze data from contingency beh study 1
##Q1. What is the accuracy of responding in the contingency table task?
#
# write 07-03-2024 by Iwona
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

#----#Q1.1. Are people less accurate when conclusions are discordant with their ideology?
#Ideology: self-identification in terms of cultural ideology (progressive vs. conservative)
m0.q1.1.ideology <- lmer(data = data.long,
                        resp ~ 1 + (1|subj.id)) #boundary (singular) fit:

summary(m0.q1.1.ideology)
VarCorr(m0.q1.1.ideology) %>% 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc)

#add add ideology concordence
m1.q1.1.ideology <- lmer(data = data.long, resp ~ 1 + 
                        ideology.conc + topic + order + (1|subj.id))
summary(m1.q1.1.ideology) #boundary (singular) fit:
anova(m1.q1.1.ideology) #ideology.concneutr sig.

#Q1.2. Are people more accurate when high (vs. low) on cognitive sophistication?
m2.q1.2.ideology <- lmer(data = data.long, 
                        resp ~ 1 + num_c + topic + order + (1|subj.id))
summary(m2.q1.2.ideology) #boundary (singular) fit
anova(m2.q1.2.ideology) #ideology.concneutr sig
m2.q1.2.ideology %>% ggemmeans(terms = "num_c") %>% plot()



# Q1.3. What are the interactive effects of ideology/priors and cognitive sophistication on accuracy?
#topic & order as main effect
data.long$topic <- as.factor(data.long$topic)
data.long$order <- as.factor(data.long$order)

m3.q1.3.ideology <-lmer(data = data.long, 
                                    resp ~ 1 + ideology.conc + num_c + ideology.conc*num_c +
                                    topic + order + (1|subj.id))
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
#boundary (singular) fit: see help('isSingular') -> topic and order as factor does not help
        
summary(m3.q1.3.ideology)
anova(m3.q1.3.ideology)
#m3.q1.3.ideology  %>% ggemmeans(terms = c("ideology.conc", "num_c")) %>% plot()

#Q1.4. Are these effects further moderated by task difficulty?
#ideology +  difficulty interaction 
m4.q1.4.ideology <-lmer(data = data.long,
                        resp ~1 + ideology.conc + num_c + ideology.conc*num_c*condition.binary + 
                        topic + order + (1|subj.id))
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
summary(m4.q1.4.ideology) #ideology.conc and interaction sig
anova(m4.q1.4.ideology)
#m4.q1.4.ideology %>% ggemmeans(terms = c("ideology.conc", "num_c", "condition.binary")) %>% plot()

#+topic interaction
m5.q1.5.ideology <-lmer(data = data.long,
                        resp ~1 + ideology.conc + num_c + topic + 
                        ideology.conc*num_c*condition.binary*topic + 
                        order + (1|subj.id))
#fixed-effect model matrix is rank deficient so dropping 5 columns / coefficients
summary(m5.q1.5.ideology)
anova(m5.q1.5.ideology) #topic gmo, ideology.concneutr and int. sig


#polit cult +  difficulty interaction
m1.q1.6 <-lmer(data = data.long, resp ~1 + ideology.conc + num_c + 
                 ideology.conc*num_c*condition.binary + 
                 topic + order + (1|subj.id))

#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
#boundary (singular) fit: see help('isSingular')

summary(m1.q1.6)
anova(m1.q1.6)

#polit cult +  difficulty interaction + topic interaction
m1.q1.7 <-lmer(data = data.long, resp ~1 + ideology.conc + num_c +  topic + 
                 ideology.conc*num_c*condition.binary*topic + 
                 order + (1|subj.id))
#fixed-effect model matrix is rank deficient so dropping 5 columns / coefficients
#boundary (singular) fit: see help('isSingular')

summary(m1.q1.7)
anova(m1.q1.7)

#----prior position corcondance----
#----Q1. Are people less accurate when conclusions are discordant with their prior?----
#prior pos
m0.q1.1.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                  resp ~ 1 + (1|subj.id)) #boundary (singular) fit
summary(m0.q1.1.prior)
VarCorr(m0.q1.1.prior) %>%
  as_tibble() %>%
  mutate(icc = vcov / sum(vcov)) %>%
  dplyr::select(grp, icc)

#add add prior concordence
m1.q1.1.prior <-
  lmer(data = data.long %>% filter(prior.conc != "neutr"),
       resp ~ 1 + prior.conc + topic + order + (1|subj.id)) #boundary (singular) fit: see help('isSingular')

summary(m1.q1.1.prior)
anova(m1.q1.1.prior)

#Q1.2. Are people more accurate when high (vs. low) on cognitive sophistication?
m2.q1.2.prior <-
  lmer(data = data.long %>% filter(prior.conc != "neutr"),
       resp ~ 1 + num_c + topic + order + (1|subj.id)) #boundary (singular) fit:

summary(m2.q1.2.prior)
anova(m2.q1.2.prior)
m2.q1.2.prior %>% ggemmeans(terms = "num_c") %>% plot()


# Q1.3. What are the interactive effects of prior/priors and cognitive sophistication on accuracy?
#topic & order as main effect
m3.q1.3.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                      resp ~ 1 + prior.conc + num_c + prior.conc*num_c + 
                      topic + order + (1|subj.id)) #boundary (singular) fit: see help('isSingular')

summary(m3.q1.3.prior)
anova(m3.q1.3.prior)
m3.q1.3.prior  %>% ggemmeans(terms = c("prior.conc", "num_c")) %>% plot()
#przy niezgodnych tym lepsze odp, im lepsze num, bez znaczenia dla zgodnych

#Q1.4. Are these effects further moderated by task difficulty?
#prior +  difficulty interaction
m4.q1.4.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                      resp ~ 1 + prior.conc + num_c + prior.conc*num_c*condition.binary +
                      topic + order + (1|subj.id)) #boundary (singular) fit: see help('isSingular')

summary(m4.q1.4.prior)
anova(m4.q1.4.prior)
m4.q1.4.prior %>% ggemmeans(terms = c("prior.conc", "num_c", "condition.binary")) %>% plot()

#+topic interaction (ten model do zastanawienia)
m5.q1.5.prior <-lmer(data = data.long %>% filter(prior.conc != "neutr"),
                    resp ~ 1 + prior.conc + num_c + topic +
                    prior.conc*num_c*condition.binary*topic +
                    order + (1 | subj.id)) #tu nie ma komunitaktu ostrzegawczego w końcu
    
summary(m5.q1.5.prior)
anova(m5.q1.5.prior) #topic istotny i int 4 st. istotna
#m5.q1.5.prior %>% ggemmeans(terms = c("prior.cult"+"num_c"+"contition.binary"+"topic")) %>% plot()
    







