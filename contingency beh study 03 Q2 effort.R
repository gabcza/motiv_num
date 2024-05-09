#---- Info -----
# Goal: analyze data from contingency beh study 1
#Q2. Do people engage in effortful information processing to reach desirable 
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


#---- Q2.1. Do people invest more effort when correct response is discordant ----
#with their ideology/priors and when the task is easy vs. difficult? ----

#---- Concordance with ideology----
# null model
#install.packages("Matrix", dependencies = TRUE)
library(Matrix)

m0.q2.1.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4),# GC: tu do dodania ta ideologia != 4 (?)
                            eff.index ~ 1 + (1|subj.id)) 
summary(m0.q2.1.ef.ideology)

VarCorr(m0.q2.1.ef.ideology) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc) # due to subj = .72


# add argument ideology concordance
m1.q2.1.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4),
                            eff.index  ~ 1 + ideology.conc + 
                              topic + order +
                            (1|subj.id)) 
summary(m1.q2.1.ef.ideology)
anova(m1.q2.1.ef.ideology) 

# add difficulty      
m2.q2.1.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4),
                  eff.index ~ 1 + ideology.conc + condition.binary + topic +
                  order + (1|subj.id)) 
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
summary(m2.q2.1.ef.ideology)
anova(m2.q2.1.ef.ideology)
#m2.q2.1.ef.ideology %>% ggemmeans(terms = "ideology.conc") %>% plot()
m2.q2.1.ef.ideology %>%  ggpredict(terms = "ideology.conc") %>% plot()
m2.q2.1.ef.ideology %>% ggemmeans(terms = "condition.binary") %>% plot()


#add interaction ideology concordance * difficulty  
m3.q2.1.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4),
          # GC: tu nie wiem dlaczego jest filtrowany neutralny -- wtedy wyrzucony będzie
          # cały dopic homeo?
          eff.index ~ ideology.conc * condition.binary + 
            topic + order + (1|subj.id)) 
summary(m3.q2.1.ef.ideology)
anova(m3.q2.1.ef.ideology)
m3.q2.1.ef.ideology  %>% ggpredict(terms = c("ideology.conc", "condition.binary")) %>% plot()

#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a contingency table? Does it depend on a task difficulty and concordance? ----

#Analysis: We will add cognitive sophistication as main 
#effect and interaction with concordance as well as difficult
m4.q2.2.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4),
                            eff.index ~ num_c +
                            topic + order + (1|subj.id))

summary(m4.q2.2.ef.ideology) 
anova(m4.q2.2.ef.ideology)

#+ int. with ideology
m5.q2.2.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4),
                            eff.index ~ ideology.conc * condition.binary * num_c + 
                              topic + order + (1|subj.id))
summary(m5.q2.2.ef.ideology) 
anova(m5.q2.2.ef.ideology)
m5.q2.2.ef.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "condition.binary")) %>% plot()

#---- Concordance with priors ----
#----Q2.1. Do people invest more effort when correct response is discordant 
#with their ideology/priors and when the task is easy vs. difficult? ----
# null model
m0.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                        eff.index ~ 1 + (1|subj.id)) 
summary(m0.q2.1.ef.prior)
VarCorr(m0.q2.1.ef.prior) %>% # get variance components (these are SDs)
  as_tibble() %>%
  mutate(icc = vcov / sum(vcov)) %>%
  dplyr::select(grp, icc) #subj: .72

# add argument ideology concordance
m1.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc  != "neutr"),
                         eff.index  ~ prior.conc + 
                           topic + order + (1|subj.id))
summary(m1.q2.1.ef.prior)
anova(m1.q2.1.ef.prior) 
m1.q2.1.ef.prior %>% ggemmeans(terms = "prior.conc") %>% plot()

# add difficulty
m2.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         eff.index ~ prior.conc + condition.binary + 
                           topic + order + (1|subj.id))
summary(m2.q2.1.ef.prior)
anova(m3.q2.1.ef.prior)
m2.q2.1.ef.prior %>% ggemmeans(terms = "prior.conc") %>% plot()
m2.q2.1.ef.prior %>% ggemmeans(terms = "condition.binary") %>% plot()

#add interaction ideology concordance * difficulty  
m3.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         eff.index ~ 1 + prior.conc * condition.binary +
                           topic + order + (1|subj.id))
summary(m3.q2.1.ef.prior)
anova(m3.q2.1.ef.prior)
m3.q2.1.ef.prior  %>% ggemmeans(terms = c("prior.conc", "condition.binary")) %>% plot()

#Q2.2. Do people high vs. low on cognitive sophistication invest more effort 
#in a contingency table? Does it depend on a task difficulty and concordance? 

#Analysis: We will add cognitive sophistication as main 
#effect and interacting with concordance as well as difficult
# main effect of numeracy
m4.q2.2.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         eff.index ~ num_c +
                         topic + order + (1|subj.id))
                           
summary(m4.q2.2.ef.prior) 
anova(m4.q2.2.ef.prior)
m4.q2.2.ef.prior %>% ggemmeans(terms = "num_c") %>% plot()
# interaction priors x condition x numeracy
m5.q2.2.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         eff.index ~ prior.conc * condition.binary * num_c + 
                           topic + order + (1|subj.id))
summary(m5.q2.2.ef.prior) 
anova(m5.q2.2.ef.prior)
m5.q2.2.ef.prior  %>% ggemmeans(terms = c("num_c", "prior.conc", "condition.binary")) %>% plot()


#+ add rt as a measure of effort
# GC: to już na razie zostamy - na właściwych danych podmienimy wskaźnik effort na rt 
hist(data.long$rt, main = "Histogram of Response Times", xlab = "Response Time")
y <- data.long %>% 
  filter(rt <= 800) 
nrow(y)
# <= 500 seconds: 1346
# <= 800 seconds: 1357

# remove responses above 500 seconds
data.long.rt.filtered <- data.long %>% filter(rt <= 500)

#Corcondeance with ideology
m0.q2.1.rt.ideology <- lmer(data = data.long.rt.filtered %>% filter(ideology != 4),# GC: tu do dodania ta ideologia != 4 (?)
                            log(rt) ~ 1 + (1|subj.id)) 
summary(m0.q2.1.rt.ideology)

VarCorr(m0.q2.1.rt.ideology) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc) # due to subj = .59


# add argument ideology concordance
m1.q2.1.rt.ideology <- lmer(data = data.long.rt.filtered %>% filter(ideology != 4),
                            log(rt)  ~ 1 + ideology.conc + 
                              topic + order +
                              (1|subj.id)) 
summary(m1.q2.1.rt.ideology)
anova(m1.q2.1.rt.ideology) 
m1.q2.1.rt.ideology %>%  ggpredict(terms = "topic") %>% plot()
m1.q2.1.rt.ideology %>%  ggpredict(terms = "order") %>% plot()

# add difficulty      
m2.q2.1.rt.ideology <- lmer(data = data.long.rt.filtered %>% filter(ideology != 4),
                            log(rt) ~ 1 + ideology.conc + condition.binary + topic +
                              order + (1|subj.id)) 
#fixed-rtfect model matrix is rank drticient so dropping 1 column / cortficient
summary(m2.q2.1.rt.ideology)
anova(m2.q2.1.rt.ideology)
#m2.q2.1.rt.ideology %>% ggemmeans(terms = "ideology.conc") %>% plot()
m2.q2.1.rt.ideology %>%  ggpredict(terms = "ideology.conc") %>% plot()
m2.q2.1.rt.ideology %>% ggemmeans(terms = "condition.binary") %>% plot()


#add interaction ideology concordance * difficulty  
m3.q2.1.rt.ideology <- lmer(data = data.long.rt.filtered %>% filter(ideology != 4),
                            # GC: tu nie wiem dlaczego jest filtrowany neutralny -- wtedy wyrzucony będzie
                            # cały dopic homeo?
                            log(rt) ~ ideology.conc * condition.binary + 
                              topic + order + (1|subj.id)) 
summary(m3.q2.1.rt.ideology)
anova(m3.q2.1.rt.ideology)
m3.q2.1.rt.ideology  %>% ggpredict(terms = c("ideology.conc", "condition.binary")) %>% plot()

#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a contingency table? Does it depend on a task difficulty and concordance? ----

#Analysis: We will add cognitive sophistication as main 
#effect and interaction with concordance as well as difficult
m4.q2.2.rt.ideology <- lmer(data = data.long.rt.filtered %>% filter(ideology != 4),
                            log(rt) ~ num_c +
                              topic + order + (1|subj.id))

summary(m4.q2.2.rt.ideology) 
anova(m4.q2.2.rt.ideology)

#+ int. with ideology
m5.q2.2.rt.ideology <- lmer(data = data.long.rt.filtered %>% filter(ideology != 4),
                            log(rt) ~ ideology.conc * condition.binary * num_c + 
                              topic + order + (1|subj.id))
summary(m5.q2.2.rt.ideology) 
anova(m5.q2.2.rt.ideology)
m5.q2.2.rt.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "condition.binary")) %>% plot()

#---- Concordance with priors ----
#----Q2.1. Do people invest more time when correct response is discordant 
#with their ideology/priors and when the task is easy vs. difficult? ----
# null model
m0.q2.1.rt.prior <- lmer(data = data.long.rt.filtered %>% filter(prior.conc != "neutr"), 
                         log(rt) ~ 1 + (1|subj.id)) 
summary(m0.q2.1.rt.prior)
VarCorr(m0.q2.1.rt.prior) %>% # get variance components (these are SDs)
  as_tibble() %>%
  mutate(icc = vcov / sum(vcov)) %>%
  dplyr::select(grp, icc) #subj: .38

# add argument ideology concordance
m1.q2.1.rt.prior <- lmer(data = data.long.rt.filtered %>% filter(prior.conc  != "neutr"),
                         log(rt)  ~ prior.conc + 
                           topic + order + (1|subj.id))
summary(m1.q2.1.rt.prior)
anova(m1.q2.1.rt.prior) 
m1.q2.1.rt.prior %>% ggemmeans(terms = "prior.conc") %>% plot()
# add difficulty
m2.q2.1.rt.prior <- lmer(data = data.long.rt.filtered %>% filter(prior.conc != "neutr"),
                         log(rt) ~ prior.conc + condition.binary + 
                           topic + order + (1|subj.id))
summary(m2.q2.1.rt.prior)
anova(m3.q2.1.rt.prior)
m2.q2.1.rt.prior %>% ggemmeans(terms = "prior.conc") %>% plot()
m2.q2.1.rt.prior %>% ggemmeans(terms = "condition.binary") %>% plot()

#add interaction ideology concordance * difficulty  
m3.q2.1.rt.prior <- lmer(data = data.long.rt.filtered %>% filter(prior.conc != "neutr"),
                         log(rt) ~ 1 + prior.conc * condition.binary +
                           topic + order + (1|subj.id))
summary(m3.q2.1.rt.prior)
anova(m3.q2.1.rt.prior)
m3.q2.1.rt.prior  %>% ggemmeans(terms = c("prior.conc", "condition.binary")) %>% plot()
#m3.q2.1.rt.prior  %>% ggemmeans(terms = "order") %>% plot()
#Q2.2. Do people high vs. low on cognitive sophistication invest more rtfort 
#in a contingency table? Does it depend on a task difficulty and concordance? 

#Analysis: We will add cognitive sophistication as main 
#effect and interacting with concordance as well as difficult
# main rtfect of numeracy
m4.q2.2.rt.prior <- lmer(data = data.long.rt.filtered %>% filter(prior.conc != "neutr"),
                         log(rt) ~ num_c +
                           topic + order + (1|subj.id))

summary(m4.q2.2.rt.prior) 
anova(m4.q2.2.rt.prior)

# interaction priors x condition x numeracy
m5.q2.2.rt.prior <- lmer(data = data.long.rt.filtered %>% filter(prior.conc != "neutr"),
                         log(rt) ~ prior.conc * condition.binary * num_c + 
                           topic + order + (1|subj.id))
summary(m5.q2.2.rt.prior) 
anova(m5.q2.2.rt.prior)
m5.q2.2.rt.prior  %>% ggemmeans(terms = c("num_c", "prior.conc", "condition.binary")) %>% plot()

