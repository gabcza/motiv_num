#---- Info -----
# Goal: analyze data from contingency beh study 1
##Q1. What is the accuracy of responding in the contingency table task?
#
# write 05-03-2024 by Iwona
#--------------------------------------------------------------------------------
#---- Load packages ----
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)


#---- Load generated data ----
#data <- read_sav("[generated]Contingency+table+%5Bbeh+study+1%5D_March+5%2C+2024_10.05.sav") #data generated 5.03
#data <- read_sav("[generated]Contingency+table+%5Bbeh+study+1%5D_March+5%2C+2024_10.05.sav") #data generated 5.03
data <- read_sav("Contingency+table+%5Bbeh+study+1+--+PILOT%5D_March+14%2C+2024_08.17.sav") #pilot 11.03

#---- Load and clean data ----
# use the script called 'contingency beh study 01 clean data.R"

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


#Check how many pp are in condtions:

data %>%
  group_by(condition) %>%
  summarize(count = n()) %>%
  print()
#easy = 34, hard = 38

#---- 1)  Accuracy in easy vs. hard condition, science (s) and non science (ns) version for all problems----
#---!!! variable t.easy.hom.s is not informative (the table for "s" was confused with the table for "ns")


problem.resp <- c(
  "t.hard.clim.resp.s", "t.hard.clim.resp.ns",
  "t.easy.clim.resp.s", "t.easy.clim.resp.ns",
  "t.hard.gmo.resp.s", "t.hard.gmo.resp.ns",
  "t.easy.gmo.resp.s", "t.easy.gmo.resp.ns",
  "t.hard.hom.resp.s", "t.hard.hom.resp.ns",
  "t.easy.hom.resp.s", "t.easy.hom.resp.ns"
)

# Calculate means for each variable
mean_values <- sapply(data[, problem.resp], function(col) mean(col, na.rm = TRUE))

# Print mean values
cat("Mean values:\n")
print(mean_values)

# Set up a layout for multiple plots
par(mfrow = c(3, 4))

# Create side-by-side bar plots for each list variable
for (col in problem.resp) {
  barplot(table(unlist(data[[col]])), main = col, col = "skyblue", border = "black", space = 0.5)
}
# Reset the layout to default
par(mfrow = c(1, 1))

#---- 2)  Effort in easy vs. hard condition, science (s) and non science (ns) version for all problems----
#effort
library(dplyr)

mean.clim.effort <- data %>%
  group_by(table.clim) %>%
  summarize(
    mean.t.hard.clim.eff = mean(t.hard.clim.eff, na.rm = TRUE),
    mean.t.easy.clim.eff = mean(t.easy.clim.eff, na.rm = TRUE)
  )

mean.gmo.effort <- data %>%
  group_by(table.gmo) %>%
  summarize(
    mean.t.hard.gmo.eff = mean(t.hard.gmo.eff, na.rm = TRUE),
    mean.t.easy.gmo.eff = mean(t.easy.gmo.eff, na.rm = TRUE)
  )


mean.hom.effort <- data %>%
  group_by(table.hom) %>%
  summarize(
          mean.t.hard.hom.eff = mean(t.hard.hom.eff, na.rm = TRUE),
          mean.t.easy.hom.eff = mean(t.easy.hom.eff, na.rm = TRUE)
        )

table.mean.effort <- bind_cols(mean.clim.effort, mean.gmo.effort, mean.hom.effort
                          )

table.mean.effort <- table.mean.effort %>%
  select(-table.gmo, -table.hom) %>%
  rename(science.conformity = table.clim)

# Print the cleaned table
print(table.mean.effort)


#histograms
#climate
clim.eff <- c("t.hard.clim.eff", "t.easy.clim.eff")
# Histograms for climate
data %>%
  gather(key = "variable", value = "value", all_of(clim.eff)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_grid(variable ~ table.clim, scales = "free") +
  labs(title = "Histograms of Climate Effort", x = "Value", y = "Frequency")


#GMO
gmo.eff <- c("t.hard.gmo.eff", "t.easy.gmo.eff")
# Histograms for GMO
data %>%
  gather(key = "variable", value = "value", all_of(gmo.eff)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_grid(variable ~ table.gmo, scales = "free") +
  labs(title = "Histograms of GMO Effort", x = "Value", y = "Frequency")

#homeopathy
hom.eff <- c("t.hard.hom.eff", "t.easy.hom.eff")
# Histograms for homeopathy
data %>%
  gather(key = "variable", value = "value", all_of(hom.eff)) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_grid(variable ~ table.hom, scales = "free") +
  labs(title = "Histograms of Homeopathy Effort", x = "Value", y = "Frequency")

#---- 3)  Difficulty in easy vs. hard condition, science (s) and non science (ns) version for all problems----

mean.clim.diff <- data %>%
  group_by(table.clim) %>%
  summarize(
    mean.t.hard.clim.diff = mean(t.hard.clim.diff, na.rm = TRUE),
    mean.t.easy.clim.diff = mean(t.easy.clim.diff, na.rm = TRUE)
  )

mean.gmo.diff <- data %>%
  group_by(table.gmo) %>%
  summarize(
    mean.t.hard.gmo.diff = mean(t.hard.gmo.diff, na.rm = TRUE),
    mean.t.easy.gmo.diff = mean(t.easy.gmo.diff, na.rm = TRUE)
  )


mean.hom.diff <- data %>%
  group_by(table.hom) %>%
  summarize(
    mean.t.hard.hom.diff = mean(t.hard.hom.diff, na.rm = TRUE),
    mean.t.easy.hom.diff = mean(t.easy.hom.diff, na.rm = TRUE)
  )

table.mean.diff <-
  bind_cols(mean.clim.diff, mean.gmo.diff, mean.hom.diff)
            
table.mean.diff <- table.mean.diff %>%
  select(
    -table.gmo,
    -table.hom) %>% 
  rename(science.conformity = table.clim)
    
# Print the cleaned table
print(table.mean.diff)

#histograms
#climate
clim.diff <- c("t.hard.clim.diff", "t.easy.clim.diff")
# Histograms for climate
data %>%
  gather(key = "variable", value = "value", all_of(clim.diff)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.clim, scales = "free") +
  labs(title = "Histograms of Climate Difficulty", x = "Value", y = "Frequency")

#GMO
gmo.diff <- c("t.hard.gmo.diff", "t.easy.gmo.diff")

# Histograms for GMO
data %>%
  gather(key = "variable", value = "value", all_of(gmo.diff)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.gmo, scales = "free") +
  labs(title = "Histograms of GMO Difficulty", x = "Value", y = "Frequency")

#homeopathy
hom.diff <- c("t.hard.hom.diff", "t.easy.hom.diff") 
# Histograms for homeopathy
data %>%
  gather(key = "variable", value = "value", all_of(hom.diff)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.hom, scales = "free") +
  labs(title = "Histograms of Homeopathy Difficulty", x = "Value", y = "Frequency")

#---- 4)  Frustration in easy vs. hard condition, science (s) and non science (ns) version for all problems----

mean.clim.frustr <- data %>%
  group_by(table.clim) %>%
  summarize(
    mean.t.hard.clim.frustr = mean(t.hard.clim.frustr, na.rm = TRUE),
    mean.t.easy.clim.frustr = mean(t.easy.clim.frustr, na.rm = TRUE)
  )

mean.gmo.frustr <- data %>%
  group_by(table.gmo) %>%
  summarize(
    mean.t.hard.gmo.frustr = mean(t.hard.gmo.frustr, na.rm = TRUE), # trzeba ujednolicić frust albo frustr
    mean.t.easy.gmo.frustr = mean(t.easy.gmo.frustr, na.rm = TRUE) # trzeba ujednolicić frust albo frustr
  )

mean.hom.frustr <- data %>%
  group_by(table.hom) %>%
  summarize(
    mean.t.hard.hom.frustr = mean(t.hard.hom.frustr, na.rm = TRUE),
    mean.t.easy.hom.frustr = mean(t.easy.hom.frustr, na.rm = TRUE)
  )

table.mean.frustr <-
  bind_cols(mean.clim.frustr, mean.gmo.frustr, mean.hom.frustr)

table.mean.frustr <- table.mean.frustr %>%
  select(
    -table.gmo,
    -table.hom) %>% 
  rename(science.conformity = table.clim)

# Print the cleaned table
print(table.mean.frustr)

#histograms
#climate
clim.frustr <- c("t.hard.clim.frustr", "t.easy.clim.frustr")
# Histograms for climate
data %>%
  gather(key = "variable", value = "value", all_of(clim.frustr)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.clim, scales = "free") +
  labs(title = "Histograms of Climate Frustration", x = "Value", y = "Frequency")


#GMO
gmo.frustr <- c("t.hard.gmo.frustr", "t.easy.gmo.frustr")
# Histograms for GMO
data %>%
  gather(key = "variable", value = "value", all_of(gmo.frustr)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.gmo, scales = "free") +
  labs(title = "Histograms of GMO Frustration", x = "Value", y = "Frequency")

#homeopathy
hom.frustr <- c("t.hard.hom.frustr", "t.easy.hom.frustr") #variable to change
# Histograms for homeopathy
data %>%
  gather(key = "variable", value = "value", all_of(hom.frustr)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.hom, scales = "free") +
  labs(title = "Histograms of Homeopathy Frustration", x = "Value", y = "Frequency")

#---- 5) Perceived correctness in easy vs. hard condition, science (s) and non science (ns) version for all problems----

mean.clim.percc <- data %>%
  group_by(table.clim) %>%
  summarize(
    mean.t.hard.clim.percc = mean(t.hard.clim.percc, na.rm = TRUE),
    mean.t.easy.clim.percc = mean(t.easy.clim.percc, na.rm = TRUE) 
  )

mean.gmo.percc <- data %>%
  group_by(table.gmo) %>%
  summarize(
    mean.t.hard.gmo.percc = mean(t.hard.gmo.percc, na.rm = TRUE), 
    mean.t.easy.gmo.percc = mean(t.easy.gmo.percc, na.rm = TRUE) 
  )

mean.hom.percc <- data %>%
  group_by(table.hom) %>%
  summarize(
    mean.t.hard.hom.percc = mean(t.hard.hom.percc, na.rm = TRUE), #to change
    mean.t.easy.hom.percc = mean(t.easy.hom.percc, na.rm = TRUE)
  )

table.mean.percc <-
  bind_cols(mean.clim.percc, mean.gmo.percc, mean.hom.percc)

table.mean.percc <- table.mean.percc %>%
  select(
    -table.gmo,
    -table.hom) %>% 
  rename(science.conformity = table.clim)

# Print the cleaned table
print(table.mean.percc)

#histograms
#climate
clim.percc <- c("t.hard.clim.percc", "t.easy.clim.percc")
# Histograms for climate
data %>%
  gather(key = "variable", value = "value", all_of(clim.percc)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.clim, scales = "free") +
  labs(title = "Histograms of Climate Perceived Correctness", x = "Value", y = "Frequency")


#GMO
gmo.percc <- c("t.hard.gmo.percc", "t.easy.gmo.percc")
# Histograms for GMO
data %>%
  gather(key = "variable", value = "value", all_of(gmo.percc)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.gmo, scales = "free") +
  labs(title = "Histograms of GMO Perceived Correctness", x = "Value", y = "Frequency")

#homeopathy
hom.percc <- c("t.hard.hom.percc", "t.easy.hom.percc") 
# Histograms for homeopathy
data %>%
  gather(key = "variable", value = "value", all_of(hom.percc)) %>%
  filter(!is.na(value) & is.finite(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  facet_grid(variable ~ table.hom, scales = "free") +
  labs(title = "Histograms of Homeopathy Perceived Correctness", x = "Value", y = "Frequency")

