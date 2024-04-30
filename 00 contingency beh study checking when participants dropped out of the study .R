#checking when participants dropped out of the study 

names(data) #N = 865 


num_prior.hom.diff<- data %>% 
  summarize(
    num_prior.hom.diff = sum(is.na(prior.hom.diff))
  )

print(num_prior.hom.diff) #260 osób ma puste 

#przed attention check z kotem jest 865 - 260 = 605 osób

data <- data %>% filter(!is.na(prior.hom.diff)) #N = 605

condition_counts <- data %>%
  group_by(condition) %>%
  summarize(num_participants = n())

print(condition_counts)

#ok, z tego 86 jeszcze odpada (pewnie na att checku)
#""                      86
# "easy"                 259
#"hard"                 260

#hard condition
hard.condition.count <- data %>%
  filter(condition == "hard") %>%
  summarize(num.pp.with.answer = sum(!is.na(t.hard.clim.percc)))

print(hard.condition.count) #248

hard.condition.count <- data %>%
  filter(condition == "hard") %>%
  summarize(num.pp.with.answer = sum(!is.na(t.hard.gmo.percc)))

print(hard.condition.count) #246

hard.condition.count <- data %>%
  filter(condition == "hard") %>%
  summarize(num.pp.with.answer = sum(!is.na(t.hard.hom.percc)))

print(hard.condition.count) #245


#after manipulation
hard.condition.count <- data %>%
  filter(condition == "hard") %>%
  summarize(num.pp.with.answer = sum(!is.na(num1)))

print(hard.condition.count) #224


hard.condition.count <- data %>%
  filter(condition == "hard") %>%
  summarize(num.pp.with.answer = sum(!is.na(num9)))

print(hard.condition.count) #214

#easy condition
easy.condition.count <- data %>%
  filter(condition == "easy") %>%
  summarize(num.pp.with.answer = sum(!is.na(t.easy.clim.percc)))

print(easy.condition.count) #245

easy.condition.count <- data %>%
  filter(condition == "easy") %>%
  summarize(num.pp.with.answer = sum(!is.na(t.easy.gmo.percc)))

print(easy.condition.count) #241

easy.condition.count <- data %>%
  filter(condition == "easy") %>%
  summarize(num.pp.with.answer = sum(!is.na(t.easy.hom.percc)))

print(easy.condition.count) #244

#after manipulation
easy.condition.count <- data %>%
  filter(condition == "easy") %>%
  summarize(num.pp.with.answer = sum(!is.na(num1)))

print(easy.condition.count) #213


easy.condition.count <- data %>%
  filter(condition == "easy") %>%
  summarize(num.pp.with.answer = sum(!is.na(num9)))

print(easy.condition.count) #204

#ostatnie pytanie
hard.condition.count <- data %>%
  filter(condition == "hard") %>%
  summarize(num.pp.with.answer = sum(!is.na(guessing.check)))

print(hard.condition.count) #204

easy.condition.count <- data %>%
  filter(condition == "easy") %>%
  summarize(num.pp.with.answer = sum(!is.na(guessing.check)))

print(easy.condition.count) #197

------

#HARD
hard.condition.data <- data %>%
  filter(condition == "hard" & !is.na(guessing.check))

#remove people who failed instruction check
hard.condition.data <- hard.condition.data %>% filter(instruction.check == 1)
nrow(hard.condition.data) #189

#RT > 300
hard.condition.data <- hard.condition.data %>% filter (Duration__in_seconds_ >300) #N = 187
nrow(hard.condition.data)

# EASY
easy.condition.data <- data %>%
  filter(condition == "easy" & !is.na(guessing.check))

#remove people who failed instruction check
easy.condition.data <- easy.condition.data %>% filter(instruction.check == 1)
nrow(easy.condition.data) #183

#RT > 300
easy.condition.data <- easy.condition.data %>% filter (Duration__in_seconds_ >300) #N = 182
nrow(easy.condition.data)







