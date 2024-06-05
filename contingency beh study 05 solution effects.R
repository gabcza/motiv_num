#---- Info -----
# Goal: additional analyses of data from contingency beh study 1
# the effects of providing the correct information
# Effects of: priors, numeracy, effort, difficulty manipulation
#
# write 14-05-2024 by Iwona
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


#---- Load data ----
# use the script called '[main study] contingency beh study 01 clean data.R"
# or saved data
#data <- read.csv("data contingency beh study clean data.csv")

#---- Create long dataset ----
# select data for argument evaluation & restructure data

# GC: create dataset with version of tables
data.versions <- data.clean %>%
  select(subj.id, table.clim, table.gmo, table.hom) %>% 
  gather(key = "topic", "version", -subj.id) %>%
  separate(topic, c("t", "topic")) %>% select(-t) %>%
  filter(version != "") # keep only cells with data

names(data.clean)

#Changing the names of the variables so that they do not clash with the names 
#of the answers to the task
rename_columns <- function(names) {
  gsub("sol.resp$", "sol_resp", 
            gsub("sol.rt_Page_Submit$", "sol_rt", names))
}

# Rename columns using the defined function for all columns
data.clean <- data.clean %>%
  rename_with(rename_columns, .cols = everything())

names(data.clean)

# create long data
data.long.sol <- data.clean %>%
  select(subj.id,
         starts_with("t.hard"), starts_with("t.easy")) %>% 
  select(-ends_with("_First_Click"), -ends_with("_Last_Click"), 
         -ends_with("_Click_Count")) %>%
  select(-contains("_DO_"), #-contains("sol")
         ) %>% 
  gather(key = "question", value = "resp", -subj.id) %>% #create long format
  mutate(question = str_replace_all(question, "rt_Page_Submit", "rt")) %>%
  separate(question, into = c("t", "condition", "topic", "question", "version"), 
           sep = "\\.") %>% 
  select(-t, -version) %>%
  filter(!is.na(resp)) %>%
  left_join(data.versions, by = c("subj.id", "topic")) %>%
  spread(question, resp)

# recode levels for condition
data.long.sol <- data.long.sol %>% 
  mutate(condition = factor(condition, levels = c("easy", "hard")))
head(data.long.sol)

# select individual-level data to add to the long data
data.ind <- data.clean %>% 
  dplyr::select(subj.id, gender, age_s_c, educ_s_c, 
                ideology, 
                num_c, crt_c,
                Duration__in_seconds_, 
                starts_with("prior"))
names(data.ind)

# add ind diff data 
data.long.sol <- data.long.sol %>% left_join(data.ind, by = "subj.id") %>%
  dplyr::select(subj.id, age_s_c, gender, educ_s_c, starts_with("prior"),
                everything())
# remove temp data 
#rm(data.pre.post.long)

#---- add info on concordance of task version with initial issue position
# (concordance: version vs. position --> -1 = discordant, 0 = neutral, 1 = concordant)
data.long.sol <- data.long.sol %>% 
  mutate(
    prior.conc = case_when(
      prior.climate.pos == 0 ~ 0, # initial pos neutral
      version == "s" & prior.climate.pos < 0 ~ -1, # discordant with prior
      version == "s" & prior.climate.pos > 0 ~ 1, # concordant with prior
      version == "ns" & prior.climate.pos > 0 ~ -1, # discordant with prior
      version == "ns" & prior.climate.pos < 0 ~ 1,
      prior.gmo.pos == 0 ~ 0, # initial pos neutral
      version == "s" & prior.gmo.pos < 0 ~ -1, # discordant with prior
      version == "s" & prior.gmo.pos > 0 ~ 1, # concordant with prior
      version == "ns" & prior.gmo.pos > 0 ~ -1, # discordant with prior
      version == "ns" & prior.gmo.pos < 0 ~ 1, # concordant with prior
      prior.hom.pos == 0 ~ 0, # initial pos neutral
      # homeopathy opposite
      version == "s" & prior.hom.pos < 0 ~ 1, # concordant with prior
      version == "s" & prior.hom.pos > 0 ~ -1, # discordant with prior
      version == "ns" & prior.hom.pos > 0 ~ 1, # concordant with prior
      version == "ns" & prior.hom.pos < 0 ~ -1 #discordant with prior
    ),
    prior.conc = factor(prior.conc, levels = c(-1, 0, 1), labels = c("disc", "neutr", "conc"))
    # arg.side.f = factor(arg.side, levels = c(1, 7), labels = c("against", "pro"))
  )

summary(as.factor(data.long.sol$prior.conc))
#disc neutr  conc --> WRONG CODING (using 50 as middle) 
#456   174   477

#disc neutr  conc --> CORRECT CODING (using 0 as middle) BUT ALSO INCL. PEOPLE WITH MISSING DATA
#774    12   771 

#---- add info on concordance of task version with ideology
#clim ns - concordant right-wing, gmo ns - concordant right with, homeo = 0 
data.long.sol <- data.long.sol %>% 
  mutate(
    ideology.conc = case_when(
      topic == "clim" & ideology == 4 ~ 0, # ideology middle
      topic == "clim" & version == "s" & ideology < 4 ~ 1, # concordant with ideology (science + L)
      topic == "clim" & version == "ns" & ideology < 4 ~ - 1, #discordant with ideology (no science + L)
      topic == "clim" & version == "s" & ideology > 4 ~ -1, # discordant with ideology (science + P)
      topic == "clim" & version == "ns" & ideology > 4 ~ 1, #concordant with ideology (no science + P)
      topic == "gmo" & ideology == 4 ~ 0, # ideology middle
      topic == "gmo" & version == "s" & ideology < 4 ~ 1, # concordant with ideology
      topic == "gmo" & version == "ns" & ideology < 4 ~ - 1, #discordant with ideology
      topic == "gmo" & version == "s" & ideology > 4 ~ -1, # discordant with ideology
      topic == "gmo" & version == "ns" & ideology > 4 ~ 1, #concordant with ideology 
      #przy homeopatii ideologia nie ma znaczenia
      topic == "hom" ~ 0),
    ideology.conc = factor(ideology.conc, levels = c(-1, 0, 1), labels = c("disc", "neutr", "conc"))
  )

# check number of observations per priors and topics
data.long.sol %>% group_by(topic, condition, version) %>%
  summarize(n = n()) # fairly balanced s and ns across topics
# check N per condition and priors
x <- data.long.sol %>% group_by(topic, condition, version, prior.conc) %>%
  summarize(n = n()) 
y <- data.long.sol %>% group_by(topic, condition, version, ideology.conc) %>%
  summarize(n = n()) 

## recode condition variable
data.long.sol$condition.binary <- ifelse(data.long.sol$condition == "hard", 1, 0)
data.long.sol %>%
  group_by(condition.binary) %>%
  summarize(count = n()) %>%
  print()
#easy = 546/3 = 182 ok, hard =561/3 = 187 ok
data.long.sol$condition.binary <- factor(data.long.sol$condition.binary)

# effort & difficulty
cor(data.long.sol$eff, data.long.sol$diff, use = "na.or.complete") # .78
#effort index
#eff.index <- mean(c(data.long.sol$eff, data.long.sol$diff))
#print(paste("eff.index:", eff.index))
data.long.sol$eff.index <- rowMeans(data.long.sol[, c("eff", "diff")], na.rm = TRUE)

# Print the first few rows of the dataset to verify the new variable
head(data.long.sol)



#GC: check resp per person 
#x <- data.long.sol %>% group_by(subj.id) %>% summarize(n = n())

#topic
data.long.sol$topic <- as.factor(data.long.sol$topic)
data.long.sol <- data.long.sol %>% 
  mutate(topic = factor(topic, levels = c("hom", "clim", "gmo")))
levels(data.long.sol$topic)

# Remove responses below X seconds
x <- data.long.sol %>% 
  filter(rt >= 20) 
nrow(x)


# remove responses below 10 seconds
data.long.sol <- data.long.sol %>% filter(rt >= 10)

#remove NS Remove observations with missing responses (each person had one of 
#the three solution blocks, so two to be removed)

data.long.sol <- data.long.sol[complete.cases(data.long.sol$sol_resp), , drop = FALSE] #N = 377


#topic
data.long.sol$topic <- as.factor(data.long.sol$topic)
data.long.sol <- data.long.sol %>% 
  mutate(topic = factor(topic, levels = c("hom", "clim", "gmo")))
levels(data.long.sol$topic)

#----Question 1 (accurtacy)----
##---- Q1.2: Are people think provided answer is more correct when high on numeracy?  ----
m0.sol <- lm(data = data.long.sol,
                sol_resp ~ condition.binary +
                 + topic)
summary(m0.sol)

nlevels(data.long.sol$topic)

m0.sol %>% ggemmeans("condition.binary") %>% plot() 


emm_topic <- ggemmeans(m0.sol.q1.1, terms = "topic")
plot(emm_topic)

emm <- emmeans(m0.sol, ~ topic)

# Perform pairwise comparisons
pairwise_comparisons <- contrast(emm, method = "pairwise")
summary(pairwise_comparisons)


#add num * condition interaction
m1.sol <- lm(data = data.long.sol,
             sol_resp~ condition.binary * num_c + 
                topic)
summary(m1.sol)
anova(m1.sol)
m1.sol %>% ggemmeans(c("num_c", "condition.binary")) %>% plot() +
  labs(title = "", y = "Perceived conclusion as accurate", x = "Numeracy", color = "Difficulty")

#add num * condition * topic interaction 
m2.sol <- lm(data = data.long.sol,
             sol_resp ~ condition.binary * num_c * topic)
summary(m2.sol)
anova(m2.sol)
m2.sol %>% ggemmeans(c("num_c", "condition.binary", "topic")) %>% plot() +
  labs(title = "Perceived conclusion as accurate", y = "Perceived conclusion as accurate", x = "Numeracy", color = "Difficulty")


#separate models for topic
m2.sol.clim <- lm(data = data.long.sol %>% filter(topic == "clim"),
                   sol_resp ~ condition.binary * num_c) 
summary(m2.sol.clim)
# gmo
m2.sol.gmo <- lm(data = data.long.sol %>% filter(topic == "gmo"),
                  sol_resp ~ condition.binary * num_c) 
summary(m2.sol.gmo)
# homeopathy
m2.sol.hom <- lm(data = data.long.sol %>% filter(topic == "hom"),
                  sol_resp ~ condition.binary * num_c) 
summary(m2.sol.hom)

sjPlot::tab_model(m2.sol.clim, m2.sol.gmo, m2.sol.hom)



#---- Q1.2: Are people think provided answer is more correct when concordant with their ideology?  ----
m3.sol.ideology <- lm(data = data.long.sol, #%>% filter(ideology != 4) 
                          sol_resp ~ ideology.conc + 
                          topic)
summary(m3.sol.ideology)
anova(m3.sol.ideology)


# Q1.3. What are the interactive effects of ideology and cognitive sophistication on accuracy of solution?
m4.sol.ideology  <-lm(data = data.long.sol, #%>% filter(ideology != 4), 
                         sol_resp ~ ideology.conc * num_c +
                           topic)
summary(m4.sol.ideology)
anova(m4.sol.ideology)
m4.sol.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc")) %>% plot() 

#Q1.4. Are these effects further moderated by task difficulty?
#ideology +  difficulty interaction 
m5.sol.ideology  <-lm(data = data.long.sol, #%>% filter(ideology != 4), 
                      sol_resp ~ ideology.conc * num_c * condition.binary +
                        topic)
summary(m5.sol.ideology)
anova(m5.sol.ideology)
m5.sol.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc")) %>% plot() 

m5.sol.ideology  <- ggpredict(m4.q1.4a.ideology, terms = c("num_c", "ideology.conc", "condition.binary"))
plot(m5.sol.ideology)

#---- Q1.2: Are people think provided answer is more correct when concordant with their prior?  ----
m1.sol.prior <- lm(data = data.long.sol %>% filter(prior.conc != "neutr"),
                      sol_resp ~ prior.conc + 
                        topic)
summary(m1.sol.prior)
anova(m1.sol.prior)


# Q1.3. What are the interactive effects of ideology and cognitive sophistication on accuracy of solution?
m2.sol.prior  <-lm(data = data.long.sol %>% filter(prior.conc != "neutr"),
                      sol_resp ~ ideology.conc * num_c +
                        topic)
summary(m2.sol.prior)
anova(m2.sol.prior)
m2.sol.prior  %>% ggpredict(terms = c("num_c", "ideology.conc")) %>% plot() 

#Q1.4. Are these effects further moderated by task difficulty?
#ideology +  difficulty interaction 
m3.sol.prior  <-lm(data = data.long.sol %>% filter(prior.conc != "neutr"),
                      sol_resp ~ ideology.conc * num_c * condition.binary +
                        topic)
summary(m3.sol.prior)
anova(m3.sol.prior )
m3.sol.prior %>% ggpredict(terms = c("num_c", "ideology.conc")) %>% plot() 


#----Question 2 (Effort as RT) ----
#----concordeance with ideology----
m0.sol <- lm(data = data.long.sol, 
                             log(sol_rt) ~ 1 + topic) 
summary(m0.sol)
m0.sol %>% ggemmeans(terms = "topic") %>% plot()


pairwise_comparisons <- pairs(m0.sol)
summary(m0.sol)


# add ideology concordance
m1.sol.ideology <- lm(data = data.long.sol,
             log(sol_rt) ~ ideology.conc + topic) 
summary(m1.sol.ideology)
anova(m1.sol.ideology)
m1.sol.ideology %>% ggemmeans(terms = "topic") %>% plot()
m1.sol.ideology %>% ggemmeans(terms = "ideology.conc") %>% plot()


#add difficulty

m2.sol.ideology <- lm(data = data.long.sol,
                      log(sol_rt) ~ ideology.conc + condition.binary + topic) 
summary(m2.sol.ideology)
anova(m2.sol.ideology)
m2.sol.ideology %>% ggemmeans(terms = "condition.binary") %>% plot()



#add interaction ideology concordance * difficulty  

m3.sol.ideology <- lm(data = data.long.sol,
                      log(sol_rt) ~ ideology.conc * condition.binary + topic) 
summary(m3.sol.ideology)
anova(m3.sol.ideology)
m3.sol.ideology %>% ggemmeans(terms = c("ideology.conc", "condition.binary")) %>% plot()



#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a solution task of contingency table? Does it depend on a task difficulty and concordance? ----

#Analysis: We will add cognitive sophistication as main 
#effect and interaction with concordance as well as difficult
m4.sol.ideology <- lm(data = data.long.sol, #%>% filter(ideology != 4),
                        log(sol_rt) ~ num_c +
                            topic)

summary(m4.sol.ideology) 
anova(m4.sol.ideology)
m4.sol.ideology %>% ggemmeans(terms = "num_c") %>% plot()


#+ int. with ideology
m5.sol.ideology <- lm(data = data.long.sol, #%>% filter(ideology != 4),
                      log(sol_rt) ~ num_c * ideology.conc +
                        topic)

summary(m5.sol.ideology) 
anova(m5.sol.ideology)

#+ int. with difficulty
m6.sol.ideology <- lm(data = data.long.sol, #%>% filter(ideology != 4),
                      log(sol_rt) ~ num_c * ideology.conc * condition.binary +
                        topic)

summary(m6.sol.ideology) 
anova(m6.sol.ideology)

m6.sol.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "condition.binary")) %>% plot()
m6.sol.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "topic")) %>% plot()

#---- Concordance with priors prior ----
m0.sol <- lm(data = data.long.sol %>% filter(prior.conc != "neutr"), 
             log(sol_rt) ~ 1 + topic) 
summary(m0.sol)
m0.sol %>% ggemmeans(terms = "topic") %>% plot()



# add prior concordance
m1.sol.prior <- lm(data = data.long.sol %>% filter(prior.conc != "neutr"), 
                   log(sol_rt) ~ prior.conc + topic) 
summary(m1.sol.prior)
anova(m1.sol.prior)
m1.sol.prior %>% ggemmeans(terms = "topic") %>% plot()
m1.sol.prior %>% ggemmeans(terms = "prior.conc") %>% plot()


#add difficulty

m2.sol.prior <- lm(data = data.long.sol %>% filter(prior.conc != "neutr"), 
                   log(sol_rt) ~ prior.conc + condition.binary + topic) 
summary(m2.sol.prior)
anova(m2.sol.prior)
m2.sol.prior %>% ggemmeans(terms = "condition.binary") %>% plot()



#add interaction prior concordance * difficulty  

m3.sol.prior <- lm(data = data.long.sol %>% filter(prior.conc != "neutr"), 
                   log(sol_rt) ~ prior.conc * condition.binary + topic) 
summary(m3.sol.prior)
anova(m3.sol.prior)
m3.sol.prior %>% ggemmeans(terms = c("prior.conc", "condition.binary")) %>% plot()



#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a solution task of contingency table? Does it depend on a task difficulty and concordance? ----

#Analysis: We will add cognitive sophistication as main 
#effect and interaction with concordance as well as difficult
m4.sol.prior <- lm(data = data.long.sol %>% filter(prior.conc != "neutr"), 
                   log(sol_rt) ~ num_c +
                     topic)

summary(m4.sol.prior) 
anova(m4.sol.prior)
m4.sol.prior %>% ggemmeans(terms = "num_c") %>% plot()


#+ int. with prior
m5.sol.prior <- lm(data = data.long.sol  %>% filter(prior.conc != "neutr"),  
                   log(sol_rt) ~ num_c * prior.conc +
                     topic)

summary(m5.sol.prior) 
anova(m5.sol.prior)

#+ int. with difficulty
m6.sol.prior <- lm(data = data.long.sol  %>% filter(prior.conc != "neutr"), 
                   log(sol_rt) ~ num_c * prior.conc * condition.binary +
                     topic)

summary(m6.sol.prior) 
anova(m6.sol.prior)

m6.sol.prior  %>% ggpredict(terms = c("num_c", "condition.binary")) %>% plot()

#----Question 3 (consequences of effort) ----
#---- Concordance with ideology----
#Ideology: self-identification in terms of cultural ideology (progressive vs. conservative)

m0.sol.eff_acc <- lm(data = data.long.sol,
                               sol_resp ~ 1 + log(sol_rt) +
                                 topic) 
summary(m0.sol.eff_acc)
anova(m0.sol.eff_acc) 
m0.sol.eff_acc %>%
  ggemmeans(terms = "sol_rt") %>%
  plot()

#add interaction
m1.sol.eff_acc.ideology <- lm(data = data.long.sol,
                              sol_resp ~ log(sol_rt) * ideology.conc +
                                topic) 

summary(m1.sol.eff_acc.ideology)
anova(m1.sol.eff_acc.ideology) 


#+difficulty
m2.sol.eff_acc.ideology <- lm(data = data.long.sol,
                              sol_resp ~ log(sol_rt) * ideology.conc * condition.binary +
                                topic) 

summary(m2.sol.eff_acc.ideology)
anova(m2.sol.eff_acc.ideology) 
sjPlot::tab_model(m2.sol.eff_acc.ideology)

m2.sol.eff_acc.ideology %>% ggemmeans(c("sol_rt", "ideology.conc", "condition.binary")) %>% plot() 

#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m3.sol.eff_acc.ideology <- lm(data = data.long.sol, #%>% filter(ideology != 4),
                               sol_resp ~ log(sol_rt) * ideology.conc * condition.binary * num_c +
                                 topic)

summary(m3.sol.eff_acc.ideology)
anova(m3.sol.eff_acc.ideology)
#ggemmeans(m4.q3.ef_acc.ideology, terms = c("eff.index", "num_c")) %>%
#plot()
m3.sol.eff_acc.ideology %>% ggemmeans(c("sol_rt", "num_c[meansd]", "ideology.conc")) %>% plot() 

ggpredict(m3.sol.eff_acc.ideolog, terms = c("num_c", "ideology.conc")) %>%
  plot()

#----Concordance with priors----
m0.sol <- lm(data = data.long.sol %>% filter(prior.conc != "neutr"), 
             log(sol_rt) ~ 1 + topic) 
summary(m0.sol)
m0.sol %>% ggemmeans(terms = "topic") %>% plot()


pairwise_comparisons <- pairs(m0.sol)
summary(m0.sol)


m1.sol.eff_acc.prior <- lm(data = data.long.sol,
                              sol_resp ~ log(sol_rt) * prior.conc +
                                topic) 

summary(m1.sol.eff_acc.prior)
anova(m1.sol.eff_acc.prior) 


#+difficulty
m2.sol.eff_acc.prior <- lm(data = data.long.sol,
                              sol_resp ~ log(sol_rt) * prior.conc * condition.binary +
                                topic) 

summary(m2.sol.eff_acc.prior)
anova(m2.sol.eff_acc.prior) 
sjPlot::tab_model(m2.sol.eff_acc.prior)

m2.sol.eff_acc.prior %>% ggemmeans(c("sol_rt", "prior.conc", "condition.binary")) %>% plot() 

#Q3b: Does the relationship between effort and accuracy depend on cognitive sophistication?
#Does increased effort investment among the sophisticated leads to more or less accurate 
#responding? Does it depend on concordance?
m3.sol.eff_acc.prior <- lm(data = data.long.sol, #%>% filter(prior != 4),
                              sol_resp ~ log(sol_rt) * prior.conc * condition.binary * num_c +
                                topic)

summary(m3.sol.eff_acc.prior)
anova(m3.sol.eff_acc.prior)
#ggemmeans(m4.q3.ef_acc.prior, terms = c("eff.index", "num_c")) %>%
#plot()
m3.sol.eff_acc.prior %>% ggemmeans(c("sol_rt", "num_c[meansd]", "prior.conc")) %>% plot() 

ggpredict(m3.sol.eff_acc.prior, terms = c("num_c", "prior.conc")) %>%
  plot()

sjPlot::tab_model(m3.sol.eff_acc.prior)


















