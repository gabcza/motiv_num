#---- Load packages ----
#library(tidyverse)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble) # for the codebook (after use, mark with a hash mark)
library(stringr)

#---- Load data ----
data <- read_sav("Contingency+table+%5Bbeh+study+1%5D_April+17%2C+2024_03.08.sav") # load data with viewing order 14.03
data <- read_sav("Contingency+table+%5Bbeh+study+1%5D_April+19%2C+2024_02.26.sav") # data with viewing order 19.04
data <- read_sav("Contingency+table+%5Bbeh+study+1%5D_April+19%2C+2024_08.12.sav") # data with viewing order 19.04 time: 16:00

#summary(data) 
nrow(data) # N = 862
glimpse(data) 

# summarize correct responses in attention checks -- we have three in this study
ncol(data %>% dplyr::select(., starts_with("att."))) #3

data <- data %>%
  mutate(
    att.check1.cor = case_when(att.check.birthday == 1 ~ 1, TRUE ~ 0),
    att.check2.cor = case_when(att.memory1 == 4 ~ 1, TRUE ~ 0),
    att.check3.cor = case_when(tolower(att.memory2) == "kot" ~ 1, TRUE ~ 0)
  )

data <- data %>%
  mutate(total_att_check = att.check1.cor + att.check2.cor + att.check3.cor) %>%
  filter(total_att_check == 3) #N = 531

summary(as.factor(data$instruction.check))

# odfitruj osoby, które nie ukończyły (nie odpowiedziały na ostatnie obowiązkowe pytanie)
data <- data %>% filter(!is.na(guessing.check)) #N = 401 

# demographics
summary(as.factor(data$gender))
summary(as.factor(data$educ))

data <- data %>% 
  mutate(age.cat = case_when(
    (age >= 18 & age <= 29) ~ "18_29",
    (age >= 30 & age <= 39) ~ "30_39",
    (age >= 40 & age <= 49) ~ "40_49",
    (age >= 50 & age <= 59) ~ "50_59",
    (age >= 60) ~ "60+"))
summary(as.factor(data$age.cat))
#

#---- find people who did not pass quotas
# people responded to demographics but do not have responses to first block of question

data <- read_sav("Contingency+table+%5Bbeh+study+1%5D_April+17%2C+2024_03.08.sav") # load data with viewing order 14.03

data <- data %>%
  filter(!is.na(gender)) %>% # no NA on demographics
  filter(!is.na(age)) %>%
  filter(!is.na(educ)) %>%
  # NA on the first block of questions
  filter(is.na(prior.climate.pos_1)) %>%
  filter(is.na(prior.gmo.pos_1)) %>%
  filter(is.na(prior.hom.pos_1))

x <- data %>% select(puser, age, gender)
write.csv2(x, "ids.csv")
