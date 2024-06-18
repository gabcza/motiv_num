#---------------------------------------------------------------------------------------------------------------------
#---- Info -----
# Goal: clean and prep of the data from contingency beh. study 2
# 
# written 18-06-2024 by Iwonka
#---------------------------------------------------------------------------------------------------------------------

#---- Load packages ----
#library(tidyverse)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble) # for the codebook (after use, mark with a hash mark)
library(stringr)

#---- Load data ----
#data <- read_sav("_Contingency+table+%5Bbeh+study+2%5D_June+18%2C+2024_02.32.sav") # data with viewing order 18.06 time: 10:40
data <- read_sav("_Contingency+table+%5Bbeh+study+2%5D_June+18%2C+2024_08.24.sav") #  # data with viewing order 18.06 time: 16:20

#summary(data) 
nrow(data) # N = 1654
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
#write.csv2(data.cdbk, "contingency beh. study 2.csv", 
#       fileEncoding = "UTF-16LE")

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
names(data)
# Qualtrics added some suffixes to the var names and it got messy + some var. have typos
data <- data %>% 
  rename(prior.climate.pos  = prior.climate.pos_1,
         prior.climate.cert =  prior.climate.cert_1,
         prior.climate.imp = prior.climate.imp_1,
         prior.climate.diff = prior.climate.diff_1,
         prior.climate.act = prior.climate.act_1,
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
         t.hard.clim.frustr = t.hard.clim.frustr_1,
         t.hard.clim.percc = t.hard.clim.percc_1,
         t.hard.gmo.eff = t.hard.gmo.eff_1,
         t.hard.gmo.diff = t.hard.gmo.diff_1,
         t.hard.gmo.frustr = t.hard.gmo.frustr_1,
         t.hard.gmo.percc = t.hard.gmo.percc_1,
         t.hard.hom.eff = t.hard.hom.eff_1,
         t.hard.hom.diff = t.hard.hom.diff_1,
         t.hard.hom.frustr =  t.hard.hom.frustr_1,
         t.hard.hom.percc = t.hard.hom.percc_1,
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
         nfc2 = nfc2_,
         nfc3 = nfc3_)

names(data)

#in the solution variables in Qualtrics could not enter the entire condition name (easy, hard), instead there were e (easy) or h (hand). 
# Replace "t.h." with "t.hard." in column names

names(data) <- gsub("^t\\.h\\.", "t.hard.", names(data))
names(data) <- gsub("^t\\.e\\.", "t.easy.", names(data))


# Remove trailing underscores from column names
names(data) <- sub("_$", "", names(data))

names(data)
#educ_values <- unique(data$educ)
#educ_values

#----- recode demographic data ----
data <- data %>%
  mutate(
    polit.lr = case_when(polit.lr <= 7 ~ polit.lr,
                         polit.lr == 99 ~ NA_real_),
    # political party - UPDATE 2024
    # recode political party into right- vs. left-wing
    polit.party.econ = case_when(polit.party %in% c(2, 5) ~ 1, 
                                 polit.party %in% c(1, 3, 4, 5, 6) ~ 0,
                                 TRUE ~ NA_real_), 
    polit.party.cult = case_when(polit.party %in% c(4, 5, 6) ~ 1, 
                                 polit.party %in% c(1, 2, 3) ~ 0, 
                                 TRUE ~ NA_real_),
    # add political party names 
    polit.party.name = case_when(polit.party == 1 ~ "1. Left", # 1 = Lewica (Razem, SLD, Wiosna)
                                 polit.party == 2 ~ "2. The Civic Coalition", # 2 = Koalicja Obywatelska
                                 polit.party == 3 ~ "3. The Third Way", # 3 = Trzecia Droga
                                 polit.party == 4 ~ "4. Law & Justice", # 4 = Prawo i Sprawiedliwość
                                 polit.party == 5 ~ "5. Conf. Liberty & Ind.",  # 5 = Konfederacja Wolność i Niepodległość (Konfederacja, KORWiN, Ruch Narodowy))
                                 #polit.party == 6 ~ "6. Sovereign Poland", # 6 = Suwerenna Polska
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
psych::alpha(data[ideology]) #std alpha = .71/raw = .70 (we can use it as a measure of ideology)
data$ideology <- rowMeans(data[c("polit.cult", "immigr.pos.reversed", "gay.pos.reversed")], na.rm = TRUE)
hist(data$ideology)
summary(data$ideology) #M = 4.51

#----- create indices of ind. differences ----
# num sum of correct resp
# nfc mean

#numeracy correct responses
#order as in Stagnaro
data <- data %>%
  mutate(
  # GC: this I'd change into 0s being someone responding wrong and not NA
  num1.cor = case_when(num1 == 500 ~ 1, num1 != 500 ~ 0, is.na(num1) ~ NA_real_), # resp = 500 (times) is correct
  num2.cor = case_when(num2 == 10 ~ 1, num1 != 10 ~ 0, is.na(num1) ~ NA_real_), # resp = 10 (people) is correct
  num3.cor = case_when(num3 == 0.1 ~ 1, num1 != 0.1 ~ 0, is.na(num1) ~ NA_real_), #resp = 0.1 (%) is correct
  num4.cor = case_when(num4 == 20 ~ 1, num1 != 20 ~ 0, is.na(num1) ~ NA_real_), # resp = 20 (%) is correct
  num5.cor = case_when(num5 == 100 ~ 1, num1 != 100 ~ 0, is.na(num1) ~ NA_real_), # resp = 100 (people) is correct
  num6.cor = case_when((num6_4 == 9 & num6_5 == 19 ~ 1),
                       (num6_4 != 9 | num6_5 != 19 ~ 0),
                       (is.na(num6_4) | is.na(num6_5) ~ NA_real_)),# resp = 9/19 is correct
  num7.cor = case_when(num7 == 4 ~ 1, num1 != 4 ~ 0, is.na(num1) ~ NA_real_), # resp = 4 (y.o) is correct
  num8.cor = case_when(num8 == 10 ~ 1, num1 != 10 ~ 0, is.na(num1) ~ NA_real_), # resp = 10 (sec) is correct
  num9.cor = case_when(num9 == 39 ~ 1, num1 != 39 ~ 0, is.na(num1) ~ NA_real_) # resp = 39 (days) is correct
 )

num <- c("num1.cor", "num2.cor", "num3.cor", "num4.cor", "num5.cor", 
         "num6.cor", "num7.cor", "num8.cor", "num9.cor")
psych::alpha(data[num]) #raw alpha = .74/ std alpha = .73
data$num = rowMeans(data[num], na.rm = TRUE) 
hist(data$num) # 1 - easy, rest rather diff
paste0("Numeracy scoress: M = ", round(mean(data$num, na.rm = TRUE), 2), ", SD = ", round(sd(data$num, na.rm = TRUE), 2))
#M = 0.40, SD = 0.25
#create 0 vs 1 scores
data <- data %>% mutate(num01 = case_when(num == 0 ~ 0, num > 0 ~ 1))# create 0-1 numeracy score

# Crt scores (last 3 items in numeracy test)
crt <- c("num7.cor", "num8.cor", "num9.cor")
data$crt = rowMeans(data[crt], na.rm = TRUE) 
hist(data$crt) # 1 - easy, rest rather diff
#create 0 vs 1 scores
data <- data %>% mutate(crt01 = case_when(crt == 0 ~ 0, crt > 0 ~ 1))# create 0-1 CRT scores

#NFC mean
data$NFC.mean <- rowMeans(data[c("nfc1", "nfc2", "nfc3")], na.rm = TRUE)
hist(data$NFC.mean)

#----- recode argument presentation order ----
data %>% select(starts_with("FL_")) %>% names() # show vars coding order 
# those that are important for us (coding the vars of tables with topic (clim, gmo, hom) and 
#their solution 
# "FL_7_DO_FL_73"                     "FL_7_DO_PRIOR.BELIEFS.GMO"         "FL_7_DO_PRIOR.BELIEFS.HOM"         "FL_59_DO_FL_69"                   
# "FL_59_DO_FL_70"                    "FL_60_DO_FL_67"                    "FL_60_DO_FL_68"                    "FL_61_DO_FL_65"                   
# "FL_61_DO_FL_66"                    "FL_62_DO_FL_63"                    "FL_62_DO_FL_64"                    "FL_54_DO_TABLE.HARD.CLIM"         
# "FL_54_DO_TABLE.HARD.GMO"           "FL_54_DO_TABLE.HARD.HOM"           "FL_50_DO_TABLE.EASY.CLIM"          "FL_50_DO_TABLE.EASY.GMO"          
# "FL_50_DO_TABLE.EASY.HOM"           "FL_46_DO_TABLE.HARD.CLIM.SOLUTION" "FL_46_DO_TABLE.HARD.GMO.SOLUTION"  "FL_46_DO_TABLE.HARD.HOM.SOLUTION" 
# "FL_42_DO_TABLE.EASY.CLIM.SOLUTION" "FL_42_DO_TABLE.EASY.GMO.SOLUTION"  "FL_42_DO_TABLE.EASY.HOM.SOLUTION"  "FL_25_DO_HOMEO1"                  
# "FL_25_DO_HOMEO2"                   "FL_25_DO_HOMEO3"                   "FL_25_DO_HOMEO4"                   "FL_25_DO_HOMEO5"                  
# "FL_25_DO_HOMEO6"                   "FL_25_DO_HOMEO7"                   "FL_25_DO_HOMEO8"                   "FL_25_DO_HOMEO9"                  
# "FL_25_DO_HOMEO10"                  "FL_25_DO_HOMEO11"                  "FL_25_DO_HOMEO12"                  "FL_25_DO_HOMEO13"                 

data <- data %>% 
  rename(
    t.hard.clim.order = FL_54_DO_TABLE.HARD.CLIM,
    t.hard.gmo.order = FL_54_DO_TABLE.HARD.GMO, 
    t.hard.hom.order = FL_54_DO_TABLE.HARD.HOM, 
    t.easy.clim.order = FL_50_DO_TABLE.EASY.CLIM, 
    t.easy.gmo.order = FL_50_DO_TABLE.EASY.GMO, 
    t.easy.hom.order = FL_50_DO_TABLE.EASY.HOM, 
    t.hard.clim.sol.order = FL_46_DO_TABLE.HARD.CLIM.SOLUTION,
    t.hard.gmo.sol.order = FL_46_DO_TABLE.HARD.GMO.SOLUTION,
    t.hard.hom.sol.order = FL_46_DO_TABLE.HARD.HOM.SOLUTION,
    t.easy.clim.sol.order = FL_42_DO_TABLE.EASY.CLIM.SOLUTION,
    t.easy.gmo.sol.order = FL_42_DO_TABLE.EASY.GMO.SOLUTION,
    t.easy.hom.sol.order = FL_42_DO_TABLE.EASY.HOM.SOLUTION)

names(data)

#----- remove original ids and qualtrics data ---- 
# and other not informative vars
data <- data %>% select(
  -ResponseId,
  -Progress, -Status, 
  -StartDate, -EndDate, -Finished, 
  -DistributionChannel, #-Q_RecaptchaScore,
  -UserLanguage,
  -FL_7_DO_FL_73,
  -FL_59_DO_FL_69,                   
  -FL_59_DO_FL_70,                
  -FL_60_DO_FL_67,
  -FL_60_DO_FL_68,          
  -FL_61_DO_FL_65,
  -FL_61_DO_FL_66,
  -FL_62_DO_FL_63,
  -FL_62_DO_FL_64)


#---- Save full data data ----
# all responses in the survey (incl. those who failed attention checks etc)
write.csv(data,"data contingency beh study 2 full data.csv")
write.csv2(data,"data contingency beh study 2 full data.csv")

#---------------------------------------------------------------------------------------------------------------------
#---- Filter data ----
nrow(data) # N = 1654

## remove people who failed att chceks
#att.check.birthday - correct ans = 1   
#att.memory1 - correct ans = nie, Qualtrics coding: Yes = 5, No = 4, so 4 is correct ans
#att.memory2 - correct ans - "kwiat"
data <- data %>%
  mutate(
    att.check1.cor = case_when(att.check.birthday == 1 ~ 1, TRUE ~ 0),
    att.check2.cor = case_when(att.memory1 == 4 ~ 1, TRUE ~ 0),
    att.check3.cor = case_when(tolower(att.memory2) == "kwiat" ~ 1, TRUE ~ 0)
  )

data <- data %>%
  mutate(
    total_att_check = att.check1.cor + att.check2.cor + att.check3.cor
  ) %>%
  filter(total_att_check == 3) #N = 1071


# Leave observations where subjects completed the table task
data <- data %>% filter(finished.tables == 1) #N = 714


#Remove the 'total_att_check' column if no longer needed
#data <- select(data, -total_att_check)

#remove people who admit failing instructions -- NOT EXCLUDED
#data <- data %>% filter(instruction.check == 1)
#nrow(data) #372

#RT > 300 -- NOT EXCLUDED
#data <- data %>% filter (Duration__in_seconds_ >300) #N = 369

# remove people who didn't finish the study -- TO DECIDE
#data2 <- data %>% filter(!is.na(instruction.check)) #N = 401 

#---- Rescale and grand-mean center vars ----

# rescale vars between 0-1
data <- data %>% 
  mutate(
    across(
      c(age, educ, 
        #relig,
        polit.cult3, polit.econ3, polit.lr), 
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
      polit.cult3_s, polit.econ3_s,
      num, crt), 
      .fns = ~c(scale(., center = TRUE, scale = FALSE)),
      .names = "{.col}_c"))
# check vars
plot(data$num_c, data$num)
hist(data$age_s_c)
#cor(data$guessing.check, data$num) #.39

#---- Save clean data----
data.clean <- data
write.csv(data.clean,"data contingency beh study 2 clean data.csv")
write.csv2(data.clean,"data contingency beh study 2 clean data.csv")

#---------------------------------------------------------------------------------------------------------------------
#---- Create long dataset ----
# select data for argument evaluation & restructure data
names(data.clean)

# GC: create dataset with version of tables
data.versions <- data.clean %>%
  select(subj.id, table.clim, table.gmo, table.hom) %>% 
  gather(key = "topic", "version", -subj.id) %>%
  separate(topic, c("t", "topic")) %>% select(-t) %>%
  filter(version != "") # keep only cells with data

# create long data
data.long <- data.clean %>%
  select(subj.id,
         starts_with("t.hard"), starts_with("t.easy")) %>% 
  select(-ends_with("_First_Click"), -ends_with("_Last_Click"), 
         -ends_with("_Click_Count")) %>%
  select(-contains("_DO_"), -contains("sol")) %>% 
  gather(key = "question", value = "resp", -subj.id) %>% #create long format
  mutate(question = str_replace_all(question, "rt_Page_Submit", "rt")) %>%
  separate(question, into = c("t", "condition", "topic", "question", "version"), 
           sep = "\\.") %>% 
  select(-t, -version) %>%
  filter(!is.na(resp)) %>%
  left_join(data.versions, by = c("subj.id", "topic")) %>%
  spread(question, resp)

# recode levels for condition
data.long <- data.long %>% 
  mutate(condition = factor(condition, levels = c("easy", "hard")))
head(data.long)

# select individual-level data to add to the long data
data.ind <- data.clean %>% 
  dplyr::select(subj.id, gender, age_s_c, educ_s_c, 
                ideology, 
                num_c, crt_c,
                Duration__in_seconds, 
                starts_with("prior"))
names(data.ind)

# add ind diff data 
data.long <- data.long %>% left_join(data.ind, by = "subj.id") %>%
  dplyr::select(subj.id, age_s_c, gender, educ_s_c, starts_with("prior"),
                everything())
# remove temp data 
#rm(data.pre.post.long)

#---- add info on concordance of task version with initial issue position
# (concordance: version vs. position --> -1 = discordant, 0 = neutral, 1 = concordant)
data.long <- data.long %>% 
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

summary(as.factor(data.long$prior.conc))
#disc neutr  conc --> CORRECT CODING (using 0 as middle) BUT ALSO INCL. PEOPLE WITH MISSING DATA
#1041  18   1083


#---- add info on concordance of task version with ideology
#clim ns - concordant right-wing, gmo ns - concordant right with, homeo = 0 
data.long <- data.long %>% 
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
data.long %>% group_by(topic, condition, version) %>%
  summarize(n = n()) # fairly balanced s and ns across topics
# check N per condition and priors
x <- data.long %>% group_by(topic, condition, version, prior.conc) %>%
  summarize(n = n()) 
y <- data.long %>% group_by(topic, condition, version, ideology.conc) %>%
  summarize(n = n()) 

## recode condition variable
data.long$condition.binary <- ifelse(data.long$condition == "hard", 1, 0)
data.long %>%
  group_by(condition.binary) %>%
  summarize(count = n()) %>%
  print()
#easy = 1077/3 = 359 ok, hard =1065/3 = 355
data.long$condition.binary <- factor(data.long$condition.binary)

# effort & difficulty
cor(data.long$eff, data.long$diff, use = "na.or.complete") # .74
#effort index
#eff.index <- mean(c(data.long$eff, data.long$diff))
#print(paste("eff.index:", eff.index))
data.long$eff.index <- rowMeans(data.long[, c("eff", "diff")], na.rm = TRUE)

# Print the first few rows of the dataset to verify the new variable
head(data.long)

#GC: check resp per person 
#x <- data.long %>% group_by(subj.id) %>% summarize(n = n())

#---- Save long data (clean)----
data.long.clean <- data.long
write.csv(data.long.clean,"data contingency beh study 2 clean long data.csv")
write.csv2(data.long.clean,"data contingency beh study 2 clean long data.csv")
