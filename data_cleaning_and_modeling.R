library(dplyr)
library(lme4)
library(gtsummary)
library(magrittr)
library(geepack)
library(sjlabelled)
library(glmnet)
library(stringr)
library(GGally)

# Set the path to the data files as the working directory
setwd('/Users/wenjzh/githubproject/Health-Big-Data/data')

###################### Read sourse data files #################################

exercise_data <- read.csv('AppleExerciseTime_202004.csv')
energy_data <- read.csv('ActiveEnergyBurned_202004.csv')
bmi_data <- read.csv("BMI_202004.csv")
bodyfat_data <- read.csv('BodyFatPercentage_202004.csv')
demo_data <- read.csv('EHR_Demographic_202010.csv')
survey_data <- read.csv('Surveys_202004.csv')
diastolic_data <- read.csv('BloodPressureDiastolic_202004.csv')
systolic_data <- read.csv('BloodPressureSystolic_202004.csv')

############################ Data Cleaning ####################################

# exercise <- exercise %>%
#   select(ParticipantResearchID, StartDate, Value)

exercise <- exercise_data$Value
unique(exercise)

energy <- energy_data$Value
unique(energy)

# Exercise and energy only contain values equal to one, so we can not use them.

# Average BMI by month and individual
bmi <- bmi_data %>%
  select(ParticipantResearchID, StartDate, Value) %>%
  mutate(StartDate = str_sub(StartDate, 1, 7)) %>%
  group_by(ParticipantResearchID, StartDate) %>%
  summarise(bmi = mean(Value, na.rm = TRUE)) %>%
  select(id = ParticipantResearchID, bmi)

# Avarage body fat by month and individual
bodyfat <- bodyfat_data %>%
  select(ParticipantResearchID, StartDate, Value) %>%
  mutate(StartDate = str_sub(StartDate, 1, 7)) %>%
  group_by(ParticipantResearchID, StartDate) %>%
  summarise(bodyfat = mean(Value, na.rm = TRUE)) %>%
  select(id = ParticipantResearchID, bodyfat)

# Select age, gender, marital status, race
# Change age at enrollment to current age
demo <- demo_data %>%
  select(ParticipantResearchID, EnrollmentDate, AgeAtEnrollment, 
         GenderName, MaritalStatusName, RaceName) %>%
  mutate(EnrollmentDate = as.numeric(as.character(str_sub(EnrollmentDate, 1, 4)))) %>%
  mutate(age = 2020 - EnrollmentDate + AgeAtEnrollment) %>%
  rename(id = ParticipantResearchID, gender = GenderName, 
         marital = MaritalStatusName, race = RaceName) %>%
  select(id, age, gender, marital, race)

# Extract sleep quality
sleep <- survey_data %>% 
  filter(SurveyQuestion == 'In the past 7 days:My sleep quality was') %>%
  mutate(StartDate = str_sub(SurveyStartDate, 1, 7),
         SurveyAnswer = as.numeric(as.character(SurveyAnswer))) %>%
  select(id = ParticipantResearchID, sleep = SurveyAnswer)

# Average stress by month and individual
stress <- survey_data %>% 
  filter(SurveyQuestion == 'Current Stress') %>%
  mutate(StartDate = str_sub(SurveyStartDate, 1, 7),
         SurveyAnswer = as.numeric(as.character(SurveyAnswer))) %>%
  group_by(ParticipantResearchID, StartDate) %>%
  summarise(stress = mean(SurveyAnswer, na.rm = TRUE)) %>%
  select(id = ParticipantResearchID, stress)

# Average mood by month and individual
mood <- survey_data %>% 
    filter(SurveyQuestion == 'Current Mood') %>%
    mutate(StartDate = str_sub(SurveyStartDate, 1, 7),
           SurveyAnswer = as.numeric(as.character(SurveyAnswer))) %>%
    group_by(ParticipantResearchID, StartDate) %>%
    summarise(mood = mean(SurveyAnswer, na.rm = TRUE)) %>%
    select(id = ParticipantResearchID, mood) 

# Average systolic blood pressure by month and individual
systolic <- systolic_data %>%
  select(ParticipantResearchID, StartDate, Value) %>%
  mutate(StartDate = str_sub(StartDate, 1, 7)) %>%
  group_by(ParticipantResearchID, StartDate) %>%
  summarise(systolic = mean(Value, na.rm = TRUE)) %>%
  select(id = ParticipantResearchID, systolic)

# Average diastolic blood pressure by month and individual
diastolic <- diastolic_data %>%
  select(ParticipantResearchID, StartDate, Value) %>%
  mutate(StartDate = str_sub(StartDate, 1, 7)) %>%
  group_by(ParticipantResearchID, StartDate) %>%
  summarise(diastolic = mean(Value, na.rm = TRUE)) %>%
  select(id = ParticipantResearchID, diastolic)

############################ Marge Datasets ###################################

cohort = sleep %>%
  left_join(bmi, by = 'id') %>%
  left_join(bodyfat, by = 'id') %>%
  left_join(demo, by = 'id') %>%
  left_join(diastolic, by = 'id') %>%
  left_join(systolic, by = 'id') %>%
  left_join(mood, by = 'id') %>%
  left_join(stress, by = 'id')
  
# Missing proportion 
missing = colMeans(is.na(cohort)) * 100
missing

# Drop BMI and body fat
analysis = cohort %>%
  select(-bmi, -bodyfat, -id)

# Drop all rows still containing NA's
analysis = analysis[complete.cases(analysis), ]

# Examine the dataset
colMeans(is.na(analysis)) * 100
dim(analysis)

########################### Correlation Analysis ##############################

ggpairs(analysis)

# diastolic and systolic are highly correlated
# diastolic has a higher correlation with response
analysis = analysis %>% select(-systolic)

# Final dataset
dim(analysis)

############################## Model Building #################################

linear_model1 = lm(sleep ~ ., data = analysis)

linear_model2 = lm(sleep ~ age + gender + marital + race + 
                     diastolic + stress + mood + gender:diastolic + 
                     marital:age + marital:stress + marital:mood + 
                     marital:diastolic, data = analysis)

summary(linear_model1)
summary(linear_model2)

