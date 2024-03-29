
#---------------------------------------------------------------------------------------------------------------------
#---- Info -----
# Goal: clean and prep of the data from pilot contingency beh. study
# 
# written 14-03-2024 by Iwonka
#---------------------------------------------------------------------------------------------------------------------

#---- Load packages ----
#library(tidyverse)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble) # for the codebook (after use, mark with a hash mark)

#---- Load data ----
data <- read_sav("Contingency+table+%5Bbeh+study+1+--+PILOT%5D_March+14%2C+2024_08.17.sav") # load data with viewing order 14.03
#summary(data) 
nrow(data) # N = 220
glimpse(data) 

#---- Get codebook [RUN ONCE; DONE] ----
library(sjlabelled) #dealing with labelled data
library(tibble)
library(psych)

# see labels
#get_label(data)
#options(max.print=1000000) # if some lines are ommited 
#get_labels(data$W3)

# get codebook
data.cdbk <- enframe(get_label(data))

# use more informative column names
colnames(data.cdbk) <- c("variable", "question")

# get descriptive statistics
data.desc <- data %>% describe() %>% as_tibble() %>% dplyr::select("n","min","max","mean","sd") %>%
  mutate(mean = round(mean, 2),
         sd = round(sd, 2))
# add stats to codebook 
data.cdbk <- cbind(data.cdbk,  data.desc)
data.cdbk <- data.cdbk %>% mutate(question = stringr::str_replace_all(question, "[\r\n]" , " ")) # remove "enter" with a space

# save codebook (add # if it will be done)
#write.csv2(data.cdbk, "pilot contingency beh. study.csv", 
#        fileEncoding = "UTF-16LE")

# View data
sjPlot::view_df(data, encoding = "UTF-8") # THIS IS JUST GREAT TO SEE THE WHOLE DATASET WITH VAR LABELS


#---- Clean data ----
# add IDs
data <- data %>%
  mutate(subj.id = row_number()) %>% # add ID
  select(subj.id, everything()) # reorder the columns - subj.id is the first column

#----- attention checks ----
# summarize correct responses in attention checks -- we have three in this study
ncol(data %>% dplyr::select(., starts_with("att."))) #3

#----- rename vars ----
# Qualtrics added some suffixes to the var names and it got messy + some var. have typos
data <- data %>% 
  rename(prior.climate.pos  = prior.climate.pos_1,
         prior.climate.cert =  prior.climate.cert_1,
         prior.climate.imp = prior.climate.imp_1,
         prior.climate.diff = prior.climate.diff_1,
         prior.gmo.pos  = prior.gmo.pos_1,
         prior.gmo.cert =  prior.gmo.cert_1,
         prior.gmo.imp = prior.gmo.imp_1,
         prior.gmo.diff = prior.gmo.diff_1,
         prior.hom.pos  = prior.hom.pos_1,
         prior.hom.cert =  prior.hom.cert_1,
         prior.hom.imp = prior.hom.imp_1,
         prior.hom.diff = prior.hom.diff_1,
         t.hard.clim.eff = t.hard.clim.eff_1,
         t.hard.clim.diff = t.hard.clim.diff_1,
         t.hard.clim.frustr = t.hard.clim.frust_1,
         t.hard.clim.percc = t.hard.clim.percc_1,
         t.hard.gmo.eff = t.hard.gmo.eff_1,
         t.hard.gmo.diff = t.hard.gmo.diff_1,
         t.hard.gmo.frustr = t.hard.gmo.frustr_1,
         t.hard.gmo.percc = t.hard.gmo.percc_1,
         t.hard.hom.eff = t.hard.hom.mentdem_1,
         t.hard.hom.diff = t.hard.hom.diff_1,
         t.hard.hom.frustr =  t.hard.hom.frustr_1,
         t.hard.hom.percc = t.hard.hom.perc_1,
         t.easy.clim.eff = t.easy.clim.eff_1,
         t.easy.clim.diff = t.easy.clim.diff_1,
         t.easy.clim.frustr = t.easy.clim.frustr_1,
         t.easy.clim.percc = t.easy.clim.percc_1,
         t.easy.gmo.eff = t.easy.gmo.eff_1,
         t.easy.gmo.diff = t.easy.gmo.diff_1,
         t.easy.gmo.frustr = t.easy.gmo.frustr_1,
         t.easy.gmo.percc = t.easy.gmo.percc_1,
         t.easy.hom.eff = t.easy.hom.eff_1,
         t.easy.hom.diff = t.easy.hom.diff_1,
         t.easy.hom.frustr = t.easy.hom.frustr_1,
         t.easy.hom.percc = t.easy.hom.percc_1,
         nfc1 = nfc1_,
         nfc2 = nfc2_,
         nfc3 = nfc3_)
      
educ_values <- unique(data$educ)

# Print the unique values of the 'educ' variable
print(educ_values)
#----- recode demographic data ----
#educ is ok
#educ_values <- unique(data$educ)
#print(educ_values)

data <- data %>%
  mutate(
    # code 99 = I prefer not to say
    income = case_when(income <= 10 ~ income,
                       income == 99 ~ NA_real_),
    relig = case_when(relig <= 7 ~ relig,
                      relig == 99 ~ NA_real_),
    polit.lr = case_when(polit.lr <= 7 ~ polit.lr,
                         polit.lr == 99 ~ NA_real_),
    # political party - UPDATE 2024
    # recode political party into right- vs. left-wing
    polit.party.econ = case_when(polit.party %in% c(2, 5) ~ 1, # KO, Konfederecja
                                 polit.party %in% c(1, 3, 4, 5, 6) ~ 0, # Lewica, Trzecia Droga, PIS, Suwerenna
                                 TRUE ~ NA_real_), # other, don't know, don't vote 
    polit.party.cult = case_when(polit.party %in% c(4, 5, 6) ~ 1, # PIS, Konfederacja, Suwerenna
                                 polit.party %in% c(1, 2, 3) ~ 0, # Lewica, KO, Trzecia Droga
                                 TRUE ~ NA_real_), # other, don't know, don't vote 
    # add political party names 
    polit.party.name = case_when(polit.party == 1 ~ "1. Left", # 1 = Lewica (Razem, SLD, Wiosna)
                                 polit.party == 2 ~ "2. The Civic Coalition", # 2 = Koalicja Obywatelska
                                 polit.party == 3 ~ "3. The Third Way", # 3 = Trzecia Droga
                                 polit.party == 4 ~ "4. Law & Justice", # 4 = Prawo i Sprawiedliwość
                                 polit.party == 5 ~ "5. Conf. Liberty & Ind.",  # 5 = Konfederacja Wolność i Niepodległość (Konfederacja, KORWiN, Ruch Narodowy))
                                 polit.party == 6 ~ "6. Sovereign Poland", # 6 = Suwerenna Polska
                                 polit.party == 97 |  polit.party == 98 |  polit.party == 99 ~ "IDK & Other"), # 97 = Inna (Jaka?), 98 = Nie wiem, 99 = Nie głosował(a)bym
    # ideology
    # Left vs. Right
    polit.lr3 = case_when(polit.lr %in% c(1, 2) ~ 0, # Left
                          polit.lr %in% c(3, 4, 5) ~ 0.5, # Moderate
                          polit.lr %in% c(6, 7) ~ 1, # Right
                          TRUE ~ NA_real_), # don't know 
    # moral progressivism vs. conservatism
    polit.cult3 = case_when(polit.cult %in% c(1, 2) ~ 0, # Progressive
                            polit.cult %in% c(3, 4, 5) ~ 0.5, # Moderate
                            polit.cult %in% c(6, 7) ~ 1), # Conservative
    # economic political ideology
    polit.econ3 = case_when(polit.econ %in% c(1, 2) ~ 0, # Social democracy
                            polit.econ %in% c(3, 4, 5) ~ 0.5, # Moderate
                            polit.econ %in% c(6, 7) ~ 1) # Free-market
  )

#check the recoded variables
names(data)
hist(data$educ)
polit.party_counts <- table(data$polit.party.name)
print(polit.party_counts)

#ideology index
#polit.cult must be reversed
data$immigr.pos.reversed <- 8 - data$immigr.pos
data$gay.pos.reversed <- 8 - data$gay.pos

hist(data$immigr.pos.reversed)
hist(data$gay.pos.reversed)
ideology <- c("polit.cult", "immigr.pos.reversed", "gay.pos.reversed")
psych::alpha(data[ideology]) # alpha = .76 (we can use it as a measure of ideology)
data$ideology <- rowMeans(data[c("polit.cult", "immigr.pos.reversed", "gay.pos.reversed")], na.rm = TRUE)
hist(data$ideology)
summary(data$ideology) #M = 3.27
#----- create indices of ind. differences ----
# num sum of correct resp
# nfc mean

#numeracy correct responses
#order in Qualtric was not as in Stagnaro; in the main study we can use order as above
#data <- data %>%
#  mutate(
#    num1.cor = case_when(num1 == 500 ~ 1, TRUE ~ 0), # resp = 500 (times) is correct
#    num2.cor = case_when(num2 == 10 ~ 1, TRUE ~ 0), # resp = 10 (people) is correct
#    num3.cor = case_when(num3 == 0.1 ~ 1, TRUE ~ 0), #resp = 0.1 (%) is correct
#    num4.cor = case_when(num4 == 20 ~ 1, TRUE ~ 0), # resp = 20 (%) is correct
#    num5.cor = case_when(num5 == 100 ~ 1, TRUE ~ 0), # resp = 100 (people) is correct
#    num6.cor = case_when(num6_4 == 9 & num6_5 == 19 ~ 1,
#                         TRUE ~ 0),# resp = 9/19 is correct
#    num7.cor = case_when(num7 == 4 ~ 1, TRUE ~ 0), # resp = 4 (days) is correct
#    num8.cor = case_when(num8 == 25 ~ 1, TRUE ~ 0), # resp = 25 (groszy) is correct
#    num9.cor = case_when(num9 == 27 ~ 1, TRUE ~ 0) # resp = 27 (days) is correct
#  )


data <- data %>%
  mutate(
    num1.cor = case_when(num1 == 100 ~ 1, TRUE ~ 0),
    num2.cor = case_when(num2 == 20 ~ 1, TRUE ~ 0), #ok
    num3.cor = case_when(num3 == 500 ~ 1, TRUE ~ 0), #ok
    num4.cor = case_when(num4 == 10 ~ 1, TRUE ~ 0), #ok
    num5.cor = case_when(num5 == 0.1 ~ 1, TRUE ~ 0), #ok
    num6.cor = case_when(num6_4 == 9 & num6_5 == 19 ~ 1, TRUE ~ 0),
    num7.cor = case_when(num7 == 4 ~ 1, TRUE ~ 0),
    num8.cor = case_when(num8 == 25 ~ 1, TRUE ~ 0),
    num9.cor = case_when(num9 == 27 ~ 1, TRUE ~ 0)
  )


num <- c("num1.cor", "num2.cor", "num3.cor", "num4.cor", "num5.cor", 
         "num6.cor", "num7.cor", "num8.cor", "num9.cor")
psych::alpha(data[num]) # alpha = .92
data$num = rowMeans(data[num], na.rm = TRUE) 
hist(data$num) # 1 - easy, rest rather diff
paste0("Numeracy scoress: M = ", round(mean(data$num, na.rm = TRUE), 2), ", SD = ", round(sd(data$num, na.rm = TRUE), 2))
#create 0 vs 1 scores
data <- data %>% mutate(crt01 = case_when(num == 0 ~ 0, num > 0 ~ 1))# create 0-1 NUMERACY score

#NFC mean
data$NFC.mean <- rowMeans(data[c("nfc1", "nfc2", "nfc3")], na.rm = TRUE)
hist(data$NFC.mean)

#----- recode argument presentation order ----
data %>% select(starts_with("FL_")) %>% names() # show vars coding order 
# those that are important for us (coding the vars of tables with topic (clim, gmo, hom) and 
#their solution 

# "FL_18_DO_FL_54"                    "FL_18_DO_FL_55"                   
# "FL_19_DO_FL_52"                    "FL_19_DO_FL_53"                   
# "FL_20_DO_FL_50"                    "FL_20_DO_FL_51"                   
# "FL_21_DO_FL_48"                    "FL_21_DO_FL_49"                   
# "FL_44_DO_TABLE.HARD.CLIM"          "FL_44_DO_TABLE.HARD.GMO"          
# "FL_44_DO_TABLE.HARD.HOM"           "FL_40_DO_TABLE.EASY.CLIM"         
# "FL_40_DO_TABLE.EASY.GMO"           "FL_40_DO_TABLE.EASY.HOM"          
# "FL_36_DO_TABLE.HARD.CLIM.SOLUTION" "FL_36_DO_TABLE.HARD.GMO.SOLUTION" 
# "FL_36_DO_TABLE.HARD.HOM.SOLUTION"  "FL_32_DO_TABLE.EASY.CLIM.SOLUTION"
# "FL_32_DO_TABLE.EASY.GMO.SOLUTION"  "FL_32_DO_TABLE.EASY.HOM.SOLUTION" 


data <- data %>% 
  rename(
    t.hard.clim.order = FL_44_DO_TABLE.HARD.CLIM,
    t.hard.gmo.order = FL_44_DO_TABLE.HARD.GMO,
    t.hard.hom.order = FL_44_DO_TABLE.HARD.HOM,
    t.easy.clim.order = FL_40_DO_TABLE.EASY.CLIM,
    t.easy.gmo.order = FL_40_DO_TABLE.EASY.GMO,
    t.easy.hom.order = FL_40_DO_TABLE.EASY.HOM,
    t.hard.clim.sol.order = FL_36_DO_TABLE.HARD.CLIM.SOLUTION,
    t.hard.gmo.sol.order = FL_36_DO_TABLE.HARD.GMO.SOLUTION,
    t.hard.hom.sol.order = FL_36_DO_TABLE.HARD.HOM.SOLUTION,
    t.easy.clim.sol.order = FL_32_DO_TABLE.EASY.CLIM.SOLUTION,
    t.easy.gmo.sol.order = FL_32_DO_TABLE.EASY.GMO.SOLUTION,
    t.easy.hom.sol.order = FL_32_DO_TABLE.EASY.HOM.SOLUTION)

names(data)
#----- remove original ids and qualtrics data ---- 
# and other not informative vars
data <- data %>% select(
  -ResponseId,
  -Progress, -Status, 
  -StartDate, -EndDate, -Finished, 
  -DistributionChannel, #-Q_RecaptchaScore,
  -UserLanguage,
  -FL_18_DO_FL_54,
  -FL_18_DO_FL_55,
  -FL_19_DO_FL_52,                    
  -FL_19_DO_FL_53,                   
  -FL_20_DO_FL_50,
  -FL_20_DO_FL_51,                   
  -FL_21_DO_FL_48,
  -FL_21_DO_FL_49)



#---- Rescale and grand-mean center vars ----

# rescale vars between 0-1
data <- data %>% 
  mutate(
    across(
      c(
        age, educ, 
        relig,
        polit.cult3, polit.econ3, polit.lr,
      ), 
      .fns = ~scales::rescale(as.numeric(.)),
      .names = "{.col}_s"
    )
  )



# check vars
hist(data$age_s)
plot(data$age, data$age_s)
data %>% ggplot(aes(polit.lr, polit.lr_s)) + 
  geom_point() +
  #geom_jitter() + 
  geom_smooth(color = "red") + geom_smooth(method = "lm", color = "blue") + theme_classic()


# center individual differences
data <- data %>% 
  mutate(
    across(c(
      age_s, educ_s,
      relig_s,
      polit.cult3_s, polit.econ3_s,
      num), # CRT is already between 0-1
      .fns = ~c(scale(., center = TRUE, scale = FALSE)),
      .names = "{.col}_c"))
# check vars
hist(data$relig_s_c)
plot(data$num_c, data$num)
hist(data$age_s_c)

#---- Save full data data ----
write.csv(data,"data contingency beh study pilot full data.csv")
write.csv2(data,"data contingency beh study pilot full data.csv")

#---- Filter data ----
nrow(data) # N = 220

## remove people who failed att chceks
#att.check.birthday - correct ans = 1   
#att.memory1 - correct ans = nie, Qualtrics coding: Yes = 5, No = 4, so 4 is correct ans
#att.memory2 - correct ans - "kot"
data <- data %>%
  mutate(
    att.check1.cor = case_when(att.check.birthday == 1 ~ 1, TRUE ~ 0),
    att.check2.cor = case_when(att.memory1 == 4 ~ 1, TRUE ~ 0),
    att.check3.cor = case_when(tolower(att.memory2) == "kot" ~ 1, TRUE ~ 0)
  )

data <- data %>%
  mutate(
    total_att_check = att.check1.cor + att.check2.cor + att.check3.cor
  ) %>%
  filter(total_att_check == 3) #N = 72

#Remove the 'total_att_check' column if no longer needed
data <- select(data, -total_att_check)

# remove people who failed serious chcek
summary(as.factor(data$serious.check))
data <- data %>% filter(serious.check == 1) 
nrow(data) # N = 72

#remove people who failed instruction check
data <- data %>% filter(instruction.check == 1)
nrow(data)

#RT > 300
data <- data %>% filter (Duration__in_seconds_ >300) #N = 72

#---- save cleaned data----
data.cleaned <- data
write.csv(data.cleaned,"data contingency beh study pilot clean data.csv")
write.csv2(data.cleaned,"data contingency beh study pilot clean data.csv")


#---- Create long dataset ----

# select data for argument evaluation & restructure data
names(data.cleaned)
# GC: create dataset with version of tables
data.versions <- data.cleaned %>%
  select(subj.id, table.clim, table.gmo, table.hom) %>% 
  gather(key = "topic", "version", -subj.id) %>%
  separate(topic, c("t", "topic")) %>% select(-t) %>%
  filter(version != "") # keep only cells with data

# create long data

data.long <- data.cleaned %>%
  select(subj.id,
         starts_with("t.hard"), starts_with("t.easy")) %>% 
  select(-ends_with("_First_Click"), -ends_with("_Last_Click"), -ends_with("_Click_Count")) %>%
  select(-contains("_DO_"), -contains("sol")) %>%  #GC: trzeba kolejność będzie jednak uwzględnić --ok, to dołączam koeljność tabelek
  gather(key = "question", value = "resp", -subj.id) %>% #create long format
  mutate(question = str_replace_all(question, "rt_Page_Submit", "rt")) %>%
  separate(question, into = c("t", "condition", "topic", "question", "version"), sep = "\\.") %>% 
  select(-t, -version) %>%
  filter(!is.na(resp)) %>%
  left_join(data.versions, by = c("subj.id", "topic")) %>%
  spread(question, resp)

# Print the first few rows of the resulting data frame to check for any issues
head(data.long)

data.ind <- data.cleaned %>% dplyr::select(subj.id, gender, age_s_c, educ_s_c, ideology,
                                   num_c, #recoded education = educ now
                                   starts_with("prior"))
names(data.ind)


# add ind diff data 
data.long <- data.long %>% left_join(data.ind, by = "subj.id") %>%
  dplyr::select(subj.id, age_s_c, gender, educ_s_c,
                starts_with("prior"),
                everything())
# remove temp data 
rm(data.pre.post.long)



#---- add info on concordance of task version with initial issue position
# (concordance: version vs. position --> -1 = discordant, 0 = neutral, 1 = concordant)
data.long <- data.long %>% 
  mutate(
    prior.conc = case_when(
      prior.climate.pos == 50 ~ 0, # initial pos neutral
      version == "s" & prior.climate.pos < 50 ~ -1, # disconcordant with prior
      version == "s" & prior.climate.pos > 50 ~ 1, # concordant with prior
      version == "ns" & prior.climate.pos > 50 ~ -1, # disconcordant with prior
      version == "ns" & prior.climate.pos < 50 ~ 1,
      prior.gmo.pos == 50 ~ 0, # initial pos neutral
      version == "s" & prior.gmo.pos < 50 ~ -1, # disconcordant with prior
      version == "s" & prior.gmo.pos > 50 ~ 1, # concordant with prior
      version == "ns" & prior.gmo.pos > 50 ~ -1, # disconcordant with prior
      version == "ns" & prior.gmo.pos < 50 ~ 1, # concordant with prior
      prior.hom.pos == 500 ~ 0, # initial pos neutral
      #przy homeopatii odwrotnie
      version == "s" & prior.hom.pos < 50 ~ 1, # concordant with prior
      version == "s" & prior.hom.pos > 50 ~ -1, # disconcordant with prior
      version == "ns" & prior.hom.pos > 50 ~ 1, # concordant with prior
      version == "ns" & prior.hom.pos < 50 ~ -1 #disconcordant with prior
    ),
    prior.conc = factor(prior.conc, levels = c(-1, 0, 1), labels = c("disc", "neutr", "conc"))
    # arg.side.f = factor(arg.side, levels = c(1, 7), labels = c("against", "pro"))
  )

summary(as.factor(data.long$prior.conc))

#---- add info on concordance of task version with ideology
#clim ns - concordant right-wing, gmo ns - concordant right with, homeo = 0 
data.long <- data.long %>% 
  mutate(
    ideology.conc = case_when(
      topic == "clim" & ideology == 4 ~ 0, # initial pos neutral
      topic == "clim" & version == "s" & ideology < 4 ~ 1, # concordant with ideology (science + L)
      topic == "clim" & version == "ns" & ideology < 4 ~ - 1, #disconcordant with ideology (no science + L)
      topic == "clim" & version == "s" & ideology > 4 ~ -1, # disncordant with ideology (science + P)
      topic == "clim" & version == "ns" & ideology > 4 ~ 1, #concondant with ideology (no science + P)
      topic == "gmo" & ideology == 4 ~ 0, # initial pos neutral
      topic == "gmo" & version == "s" & ideology < 4 ~ 1, # concordant with ideology
      topic == "gmo" & version == "ns" & ideology < 4 ~ - 1, #disconcordant with ideology
      topic == "gmo" & version == "s" & ideology > 4 ~ -1, # disconcordant with ideology
      topic == "gmo" & version == "ns" & ideology > 4 ~ 1, #concordant with ideology 
      #przy homeopatii ideologia nie ma znaczenia
      topic == "hom" ~ 0 
    ),
    ideology.conc = factor(ideology.conc, levels = c(-1, 0, 1), labels = c("disc", "neutr", "conc"))
  )

names(data.long)

## recode condition variable
data.long$condition.binary <- ifelse(data.long$condition == "hard", 1, 0)

data.long %>%
  group_by(condition.binary) %>%
  summarize(count = n()) %>%
  print()
#easy = 102/3 = 34 ok, hard =114/3 = 38 ok
data.long$condition.binary <- factor(data.long$condition.binary)

#GC: check resp per person 
x <- data.long %>% group_by(subj.id) %>% summarize(n = n())

#---- save cleaned long data----
data.long.cleaned <- data.long
write.csv(data.long.cleaned,"data contingency beh study pilot clean long data.csv")
write.csv2(data.long.cleaned,"data contingency beh study pilot clean long data.csv")
