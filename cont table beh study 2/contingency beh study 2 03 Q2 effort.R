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
library(ggeffects)
library(lme4)
library(lmerTest)
library(ggeffects)
options(scipen = 999)
library(effects)
library(multcomp) #for simple slopes
library(multcompView)

#---- Load data in a long format ----
# use the script called "contingency beh study 2 01 clean data.R"
# or saved data
#data.long <- read.csv(data contingency beh study 2 clean long data.csv")

#topic
data.long$topic <- as.factor(data.long$topic)
data.long <- data.long %>% 
  mutate(topic = factor(topic, levels = c("hom", "clim", "gmo")))
levels(data.long$topic)

#---- Q2.1. Do people invest more effort when correct response is discordant ----
#with their ideology/priors and when the task is easy vs. difficult? ----

#---- Concordance with ideology----
#Ideology: self-identification in terms of cultural ideology (progressive vs. conservative)

#Models in which participants with ideology = 4 were excluded are not converge, 
#so models for ideology are calculated in two ways: a) taking into account all 
#observations and b) without the topic of homeopathy and without pp with ideology = 4

#-----a) all observations----
# null model
#install.packages("Matrix", dependencies = TRUE)
library(Matrix)

m0.q2.1a.ef.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),# GC: tu do dodania ta ideologia != 4 (?)
                            eff.index ~ 1 + (1|subj.id)) 
summary(m0.q2.1a.ef.ideology)

VarCorr(m0.q2.1a.ef.ideology) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc) # due to subj = .72


# add argument ideology concordance
m1.q2.1a.ef.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                            eff.index  ~ 1 + ideology.conc + 
                              topic + order +
                            (1|subj.id)) 
summary(m1.q2.1a.ef.ideology)
anova(m1.q2.1a.ef.ideology) 

# add difficulty      
m2.q2.1a.ef.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                  eff.index ~ 1 + ideology.conc + condition.binary + topic +
                  order + (1|subj.id)) 

summary(m2.q2.1a.ef.ideology)
anova(m2.q2.1a.ef.ideology)
#m2.q2.1.ef.ideology %>% ggemmeans(terms = "ideology.conc") %>% plot()
m2.q2.1a.ef.ideology %>%  ggpredict(terms = "ideology.conc") %>% plot()
m2.q2.1a.ef.ideology %>% ggemmeans(terms = "condition.binary") %>% plot()


#add interaction ideology concordance * difficulty  
m3.q2.1a.ef.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
          # GC: tu nie wiem dlaczego jest filtrowany neutralny -- wtedy wyrzucony będzie
          # cały dopic homeo?
          eff.index ~ ideology.conc * condition.binary + 
            topic + order + (1|subj.id)) 
summary(m3.q2.1a.ef.ideology)
anova(m3.q2.1a.ef.ideology)
m3.q2.1a.ef.ideology  %>% ggpredict(terms = c("ideology.conc", "condition.binary")) %>% plot()
m3.q2.1a.ef.ideology  %>% ggpredict(terms = c("condition.binary", "ideology.conc","topic")) %>% plot()

# Compute estimated marginal means (Simple effects)
simple_effects <- emmeans(m3.q2.1a.ef.ideology, ~ ideology.conc * condition.binary)
print(simple_effects)

# pairwise comparisons
pairwise_comparisons <- pairs(simple_effects, adjust = "tukey")
print(pairwise_comparisons)
# Convert the pairwise comparisons to a data frame for ggplot2
pairwise_df <- as.data.frame(pairwise_comparisons)
# plot
ggplot(pairwise_df, aes(x = contrast, y = estimate, ymin = estimate - SE, ymax = estimate + SE, color = p.value < 0.05)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Pairwise Comparisons of Estimated Marginal Means",
       x = "Comparison",
       y = "Estimate",
       color = "Significant") +
  theme_minimal()


#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a contingency table? Does it depend on a task difficulty and concordance? ----

#Analysis: We will add cognitive sophistication as main 
#effect and interaction with concordance as well as difficult
m4.q2.2a.ef.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                            eff.index ~ num_c +
                            topic + order + (1|subj.id))

summary(m4.q2.2a.ef.ideology) 
anova(m4.q2.2a.ef.ideology)

#+ int. with ideology
m5.q2.2a.ef.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                            eff.index ~ ideology.conc * condition.binary * num_c + 
                              topic + order + (1|subj.id))
summary(m5.q2.2a.ef.ideology) 
anova(m5.q2.2a.ef.ideology)
m5.q2.2a.ef.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "condition.binary")) %>% plot()
m5.q2.2a.ef.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "topic")) %>% plot()


#----b) without the homeopathy topic and without pp with ideology = 4

m0.q2.1b.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4) # remove people with ideology = 4
                             %>% filter(topic != "hom"),
                             eff.index ~ 1 + (1|subj.id)) 
summary(m0.q2.1b.ef.ideology)

VarCorr(m0.q2.1b.ef.ideology) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc) # due to subj = .71


# add argument ideology concordance
m1.q2.1b.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                             %>% filter(topic != "hom"),
                             eff.index  ~ 1 + ideology.conc + 
                               topic + order +
                               (1|subj.id)) 
summary(m1.q2.1b.ef.ideology)
anova(m1.q2.1b.ef.ideology) 

# add difficulty      
m2.q2.1b.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                             %>% filter(topic != "hom"),
                             eff.index ~ 1 + ideology.conc + condition.binary + topic +
                               order + (1|subj.id)) 

summary(m2.q2.1b.ef.ideology)
anova(m2.q2.1b.ef.ideology)
#m2.q2.1.ef.ideology %>% ggemmeans(terms = "ideology.conc") %>% plot()
m2.q2.1b.ef.ideology %>%  ggpredict(terms = "ideology.conc") %>% plot()
m2.q2.1b.ef.ideology %>% ggemmeans(terms = "condition.binary") %>% plot()


#add interaction ideology concordance * difficulty  
m3.q2.1b.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                             %>% filter(topic != "hom"),
                             eff.index ~ ideology.conc * condition.binary + 
                               topic + order + (1|subj.id)) 
summary(m3.q2.1b.ef.ideology)
anova(m3.q2.1b.ef.ideology)
m3.q2.1b.ef.ideology  %>% ggpredict(terms = c("ideology.conc", "condition.binary")) %>% plot()







#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a contingency table? Does it depend on a task difficulty and concordance? ----

#Analysis: We will add cognitive sophistication as main 
#effect and interaction with concordance as well as difficult
m4.q2.2b.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                             %>% filter(topic != "hom"),
                             eff.index ~ num_c +
                             topic + order + (1|subj.id))

summary(m4.q2.2b.ef.ideology) 
anova(m4.q2.2b.ef.ideology)

#+ int. with ideology
m5.q2.2b.ef.ideology <- lmer(data = data.long %>% filter(ideology != 4) 
                             %>% filter(topic != "hom"),
                             eff.index ~ ideology.conc * condition.binary * num_c + 
                             topic + order + (1|subj.id))

summary(m5.q2.2b.ef.ideology) 
anova(m5.q2.2b.ef.ideology)
m5.q2.2b.ef.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "condition.binary")) %>% plot()


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

# add argument prior concordance
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
anova(m2.q2.1.ef.prior)
m2.q2.1.ef.prior %>% ggemmeans(terms = "prior.conc") %>% plot()
m2.q2.1.ef.prior %>% ggemmeans(terms = "condition.binary") %>% plot()

#add interaction ideology concordance * difficulty  
m3.q2.1.ef.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         eff.index ~ 1 + prior.conc * condition.binary +
                           topic + order + (1|subj.id))
summary(m3.q2.1.ef.prior)
anova(m3.q2.1.ef.prior)
m3.q2.1.ef.prior  %>% ggemmeans(terms = c("prior.conc", "condition.binary")) %>% plot()


# Compute estimated marginal means (Simple effects)
#emm <- emmeans(m3.q2.1.ef.prior, ~ prior.conc | condition.binary | topic)
#print(emm)

emm <- emmeans(m3.q2.1.ef.prior, ~ prior.conc | condition.binary)
print(emm)

# Perform pairwise comparisons within each condition.binary and topic
contrasts <- contrast(emm, method = "pairwise", adjust = "tukey")
print(contrasts)
# Convert the pairwise comparisons to a data frame for ggplot2
contrasts_df <- as.data.frame(contrasts)
head(contrasts_df)
contrasts_df$significance <- ifelse(contrasts_df$p.value < 0.05, "Significant", "Not Significant")

# Create the plot
ggplot(contrasts_df, aes(x = contrast, y = estimate, color = significance)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2) +
  coord_flip() +
  labs(title = "Simple Effects of Estimated Marginal Means",
       x = "Comparison",
       y = "Estimate",
       color = "Significance") +
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "black")) +
  theme_minimal()

sjPlot::tab_model(m3.q2.1.ef.prior)




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


predicted_values <- ggemmeans(m5.q2.2.ef.prior, terms = c("num_c", "prior.conc", "condition.binary"))

# Create the plot
plot <- ggplot(predicted_values, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(x = "Numeracy", y = "Subjective Effort", color = "Concordance", fill = "Concordance") +
  theme_classic() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18),  # Enlarge x-axis title
    axis.title.y = element_text(size = 18)   # Enlarge y-axis title
  ) +
  facet_wrap(~ facet, ncol = 2)  # Create separate plots for each condition

# Print the plot
print(plot)




m5.q2.2.ef.prior_slopes <- cld(emtrends(
  m5.q2.2.ef.prior, ~ prior.conc * condition.binary, var = "num_c"), 
  details = TRUE)

print(m5.q2.2.ef.prior_slopes)
summary(m5.q2.2.ef.prior_slopes)

#----RT as a measure of effort----
# GC: to już na razie zostamy - na właściwych danych podmienimy wskaźnik effort na rt 
hist(data.long$rt, main = "Histogram of Response Times", xlab = "Response Time")
y <- data.long %>% 
  filter(rt <= 800) 
nrow(y)
# <= 500 seconds: 1346
# <= 800 seconds: 1357
mean(data.long$rt)

# remove responses above 500 seconds
#data.long.rt.filtered <- data.long %>% filter(rt <= 500) -> we do not remove them

#----Corcondeance with ideology----
#-----a) all observations----
m0.q2.1a.rt.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),# GC: tu do dodania ta ideologia != 4 (?)
                            log(rt) ~ 1 + (1|subj.id)) 
summary(m0.q2.1a.rt.ideology)

VarCorr(m0.q2.1a.rt.ideology) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc) # due to subj = .58


# add argument ideology concordance
m1.q2.1a.rt.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                            log(rt)  ~ 1 + ideology.conc + 
                              topic + order +
                              (1|subj.id)) 
summary(m1.q2.1a.rt.ideology)
anova(m1.q2.1a.rt.ideology) 
m1.q2.1a.rt.ideology %>%  ggpredict(terms = "topic") %>% plot()
m1.q2.1a.rt.ideology %>%  ggpredict(terms = "order") %>% plot()

# add difficulty      
m2.q2.1a.rt.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                            log(rt) ~ 1 + ideology.conc + condition.binary + topic +
                              order + (1|subj.id)) 

summary(m2.q2.1a.rt.ideology)
anova(m2.q2.1a.rt.ideology)
#m2.q2.1.rt.ideology %>% ggemmeans(terms = "ideology.conc") %>% plot()
m2.q2.1a.rt.ideology %>%  ggpredict(terms = "ideology.conc") %>% plot()
m2.q2.1a.rt.ideology %>% ggemmeans(terms = "condition.binary") %>% plot()


#add interaction ideology concordance * difficulty  
m3.q2.1a.rt.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                            # GC: tu nie wiem dlaczego jest filtrowany neutralny -- wtedy wyrzucony będzie
                            # cały dopic homeo?
                            log(rt) ~ ideology.conc * condition.binary + 
                              topic + order + (1|subj.id)) 
summary(m3.q2.1a.rt.ideology)
anova(m3.q2.1a.rt.ideology)
m3.q2.1a.rt.ideology  %>% ggpredict(terms = c("ideology.conc", "condition.binary")) %>% plot()

#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a contingency table? Does it depend on a task difficulty and concordance? ----

#Analysis: We will add cognitive sophistication as main 
#effect and interaction with concordance as well as difficult
m4.q2.2a.rt.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                            log(rt) ~ num_c +
                              topic + order + (1|subj.id))

summary(m4.q2.2a.rt.ideology) 
anova(m4.q2.2a.rt.ideology)

#+ int. with ideology
m5.q2.2a.rt.ideology <- lmer(data = data.long, #%>% filter(ideology != 4),
                            log(rt) ~ ideology.conc * condition.binary * num_c + 
                              topic + order + (1|subj.id))
summary(m5.q2.2a.rt.ideology) 
anova(m5.q2.2a.rt.ideology)
m5.q2.2a.rt.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "condition.binary")) %>% plot()

#----b) without the homeopathy topic and without pp with ideology = 4 ----
m0.q2.1b.rt.ideology <- lmer(data = data.long %>% filter(ideology != 4) # remove people with ideology = 4
                             %>% filter(topic != "hom"),
                             log(rt) ~ 1 + (1|subj.id)) 
summary(m0.q2.1b.rt.ideology)

VarCorr(m0.q2.1b.rt.ideology) %>% # get variance components (these are SDs) 
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  dplyr::select(grp, icc) # due to subj = .58


# add argument ideology concordance
m1.q2.1b.rt.ideology <- lmer(data = data.long %>% filter(ideology != 4) # remove people with ideology = 4
                             %>% filter(topic != "hom"),
                             log(rt)  ~ 1 + ideology.conc + 
                               topic + order +
                               (1|subj.id)) 
summary(m1.q2.1b.rt.ideology)
anova(m1.q2.1b.rt.ideology) 
m1.q2.1b.rt.ideology %>%  ggpredict(terms = "topic") %>% plot()
m1.q2.1b.rt.ideology %>%  ggpredict(terms = "order") %>% plot()

# add difficulty      
m2.q2.1b.rt.ideology <- lmer(data = data.long %>% filter(ideology != 4) # remove people with ideology = 4
                             %>% filter(topic != "hom"),
                             log(rt) ~ 1 + ideology.conc + condition.binary + topic +
                               order + (1|subj.id)) 

summary(m2.q2.1b.rt.ideology)
anova(m2.q2.1b.rt.ideology)
#m2.q2.1.rt.ideology %>% ggemmeans(terms = "ideology.conc") %>% plot()
m2.q2.1b.rt.ideology %>%  ggpredict(terms = "ideology.conc") %>% plot()
m2.q2.1b.rt.ideology %>% ggemmeans(terms = "condition.binary") %>% plot()


#add interaction ideology concordance * difficulty  
m3.q2.1b.rt.ideology <- lmer(data = data.long %>% filter(ideology != 4) # remove people with ideology = 4
                             %>% filter(topic != "hom"),
                             log(rt) ~ ideology.conc * condition.binary + 
                               topic + order + (1|subj.id)) 
summary(m3.q2.1b.rt.ideology)
anova(m3.q2.1b.rt.ideology)
m3.q2.1b.rt.ideology  %>% ggpredict(terms = c("ideology.conc", "condition.binary")) %>% plot()

#----Q2.2. Do people high vs. low on cognitive sophistication invest more effort ----
#in a contingency table? Does it depend on a task difficulty and concordance? ----

m4.q2.2b.rt.ideology <- lmer(data = data.long %>% filter(ideology != 4) # remove people with ideology = 4
                             %>% filter(topic != "hom"),
                             log(rt) ~ num_c +
                             topic + order + (1|subj.id))

summary(m4.q2.2b.rt.ideology) 
anova(m4.q2.2b.rt.ideology)

#+ int. with ideology
m5.q2.2b.rt.ideology <- lmer(data = data.long %>% filter(ideology != 4) # remove people with ideology = 4
                             %>% filter(topic != "hom"),
                             log(rt) ~ ideology.conc * condition.binary * num_c + 
                               topic + order + (1|subj.id))
summary(m5.q2.2b.rt.ideology) 
anova(m5.q2.2b.rt.ideology)
m5.q2.2b.rt.ideology  %>% ggpredict(terms = c("num_c", "ideology.conc", "condition.binary")) %>% plot()

#---- Concordance with priors ----
#----Q2.1. Do people invest more time when correct response is discordant 
#with their ideology/priors and when the task is easy vs. difficult? ----
# null model
m0.q2.1.rt.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"), 
                         log(rt) ~ 1 + (1|subj.id)) 
summary(m0.q2.1.rt.prior)
VarCorr(m0.q2.1.rt.prior) %>% # get variance components (these are SDs)
  as_tibble() %>%
  mutate(icc = vcov / sum(vcov)) %>%
  dplyr::select(grp, icc) #subj: .58

# add argument concordance
m1.q2.1.rt.prior <- lmer(data = data.long %>% filter(prior.conc  != "neutr"),
                         log(rt)  ~ prior.conc + 
                           topic + order + (1|subj.id))
summary(m1.q2.1.rt.prior)
anova(m1.q2.1.rt.prior) 
m1.q2.1.rt.prior %>% ggemmeans(terms = "prior.conc") %>% plot()
# add difficulty
m2.q2.1.rt.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         log(rt) ~ prior.conc + condition.binary + 
                           topic + order + (1|subj.id))
summary(m2.q2.1.rt.prior)
anova(m2.q2.1.rt.prior)
m2.q2.1.rt.prior %>% ggemmeans(terms = "prior.conc") %>% plot()
m2.q2.1.rt.prior %>% ggemmeans(terms = c("condition.binary", "prior.conc", "topic")) %>% plot()


# Generate the tabular summary
sjPlot::tab_model(m2.q2.1.rt.prior)


#add interaction ideology concordance * difficulty  
m3.q2.1.rt.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
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
# main effect of numeracy
m4.q2.2.rt.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         log(rt) ~ num_c +
                           topic + order + (1|subj.id))

summary(m4.q2.2.rt.prior) 
anova(m4.q2.2.rt.prior)

#interaction num x priors with condition (difficulty) control
m5.q2.2.rt.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         log(rt) ~ num_c * prior.conc + condition.binary +
                           topic + order + (1|subj.id))

summary(m5.q2.2.rt.prior) 
anova(m5.q2.2.rt.prior)

sjPlot::tab_model(m5.q2.2.rt.prior)


m5.q2.2.rt.prior  %>% ggemmeans(terms = c("num_c", "prior.conc","condition.binary", "topic")) %>% plot()
m5.q2.2.rt.prior  %>% ggemmeans(terms = c("num_c", "prior.conc","condition.binary")) %>% plot()

emm <- emmeans(m5.q2.2.rt.prior, ~ num_c * prior.conc | topic)
summary(emm)

emmeans_df <- as.data.frame(emm)
emmeans_df$num_c <- as.numeric(emmeans_df$num_c)


# Create the plot with facet grid for topics 
#to jest do poprawy
ggplot(emmeans_df, aes(x = num_c, y = emmean, color = prior.conc, group = prior.conc)) +
  geom_line() +  # Add lines for numeracy
  geom_point() +  # Add points
  facet_wrap(~ topic) +  # Separate plots for each topic
  labs(title = "Estimated Marginal Means",
       x = "Numeracy (num_c)",
       y = "Predicted log(rt)",
       color = "Prior Conc") +
  theme_minimal()



library(ggplot2)


# Generate the tabular summary
sjPlot::tab_model(m5.q2.2.rt.prior)



# interaction priors x condition x numeracy
m6.q2.2.rt.prior <- lmer(data = data.long %>% filter(prior.conc != "neutr"),
                         log(rt) ~ prior.conc * condition.binary * num_c + 
                           topic + order + (1|subj.id))
summary(m6.q2.2.rt.prior) 
anova(m6.q2.2.rt.prior)
m6.q2.2.rt.prior  %>% ggemmeans(terms = c("num_c", "prior.conc", "condition.binary")) %>% plot()

predicted_values <- ggemmeans(m6.q2.2.rt.prior, terms = c("num_c", "prior.conc", "condition.binary"))

# Create the plot
plot <- ggplot(predicted_values, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(x = "Numeracy", y = "RT", color = "Concordance", fill = "Concordance") +
  theme_classic() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18),  # Enlarge x-axis title
    axis.title.y = element_text(size = 18)   # Enlarge y-axis title
  ) +
  facet_wrap(~ facet, ncol = 2)  # Create separate plots for each condition

# Print the plot
print(plot)


