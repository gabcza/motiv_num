#---- Info -----
# Goal: additional analyses of data from contingency beh study 1
# Analysis of perception of success/accuracy/ effort for easy vs difficult condition
#
# write 30-04-2024 by Iwona
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

#---- Effects on perception of accuracy ----
m0.q4 <- lmer(data = data.long,
                percc ~ condition.binary * topic + 
                  order + topic + (1|subj.id)) 
summary(m0.q4)
anova(m0.q4)
m0.q4 %>% ggemmeans(c("topic", "condition.binary")) %>% plot() +
  labs(title = "Perceived accuracy", y = "Perceived accuracy", #x = "Numeracy", 
       color = "Difficulty")

# add numeracy
m0.q4.1 <- lmer(data = data.long,
                percc ~ condition.binary * num_c + 
                  order + topic + (1|subj.id)) 
summary(m0.q4.1)
anova(m0.q4.1)
m0.q4.1 %>% ggemmeans(c("num_c", "condition.binary")) %>% plot() +
  labs(title = "Perceived accuracy", y = "Perceived accuracy", x = "Numeracy", 
       color = "Difficulty")

#numeracy
m0.q4.2 <- lmer(data = data.long,
                percc ~ num_c + 
                  order + topic + (1|subj.id)) 
summary(m0.q4.2)

#numeracy * condition 

m0.q4.3 <- lmer(data = data.long,
                percc ~ condition.binary * num_c + 
                  order + topic + (1|subj.id)) 
summary(m0.q4.3)

m0.q4.3 %>% ggemmeans(c("num_c", "condition.binary")) %>% plot() 


#---- #Effects of numeracy in easy condition ---- 
#perc. corectness 
m.easy.percc <- lmer(data = data.long %>% filter(condition.binary != "1"),
                percc ~ num_c + 
                  order + topic + (1|subj.id)) 
summary(m.easy.percc)
anova(m.easy.percc)
m.easy.percc %>% ggemmeans(terms = "num_c") %>% plot()

#accuracy
m.easy.resp <- lmer(data = data.long %>% filter(condition.binary != "1"),
                     resp ~ num_c + 
                       order + topic + (1|subj.id)) 
summary(m.easy.resp)
anova(m.easy.resp)
m.easy.resp %>% ggemmeans(terms = "num_c") %>% plot()
m.easy.resp %>% ggemmeans(c("num_c", "topic")) %>% plot()

#effort 
m.easy.eff <- lmer(data = data.long %>% filter(condition.binary != "1"),
                    eff.index ~ num_c + 
                      order + topic + (1|subj.id)) 
summary(m.easy.eff)
anova(m.easy.eff)
m.easy.eff %>% ggemmeans(terms = "num_c") %>% plot()

##---- #Effects of numeracy in hard condition ---- 
#perc. corectness 
m.hard.percc <- lmer(data = data.long %>% filter(condition.binary != "0"),
                     percc ~ num_c + 
                       order + topic + (1|subj.id)) 
summary(m.hard.percc)
anova(m.hard.percc)
m.hard.percc %>% ggemmeans(terms = "num_c") %>% plot()

#accuracy
m.hard.resp <- lmer(data = data.long %>% filter(condition.binary != "0"),
                    resp ~ num_c + 
                      order + topic + (1|subj.id)) 
summary(m.hard.resp)
anova(m.hard.resp)
m.hard.resp %>% ggemmeans(terms = "num_c") %>% plot()


#effort 
m.hard.eff <- lmer(data = data.long %>% filter(condition.binary != "0"),
                   eff.index ~ num_c + 
                     order + topic + (1|subj.id)) 
summary(m.hard.eff)
anova(m.hard.eff)
m.hard.eff %>% ggemmeans(terms = "num_c") %>% plot()
m.hard.eff %>% ggemmeans(c("num_c", "topic")) %>% plot()





