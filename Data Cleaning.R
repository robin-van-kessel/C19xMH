#Load necessary packages
library(dplyr)
library(purrr)
library(tidyverse)
library(lubridate)
library(quantmod)

#Importing ICL-YouGov raw data files
australia <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/australia.csv", header = TRUE, na.strings = c("", " ", "NA"))
canada <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/canada.csv", header = TRUE, na.strings = c("", " ", "NA"))
denmark <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/denmark.csv", header = TRUE, na.strings = c("", " ", "NA"))
france <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/france.csv", header = TRUE, na.strings = c("", " ", "NA"))
germany <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/germany.csv", header = TRUE, na.strings = c("", " ", "NA"))
israel <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/israel.csv", header = TRUE, na.strings = c("", " ", "NA"))
italy <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/italy.csv", header = TRUE, na.strings = c("", " ", "NA"))
japan <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/japan.csv", header = TRUE, na.strings = c("", " ", "NA"))
netherlands  <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/netherlands.csv", header = TRUE, na.strings = c("", " ", "NA"))
spain <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/spain.csv", header = TRUE, na.strings = c("", " ", "NA"))
united_kingdom <-read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/united-kingdom.csv", header = TRUE, na.strings = c("", " ", "NA"))

#Cleaning ICL-YouGov data files
australia <- australia %>%
  mutate(country = "Australia",
         region = NA,
         employment_status_dummy = employment_status,
         employment_status = case_when(
           employment_status_dummy == "Working full time (35 or more hours per week)" ~ "Full time employment",
           employment_status_dummy == "Working part time (8-34 hours a week)" | 
             employment_status_dummy == "Working part time (Less than 8 hours a week)" ~ "Part time employment",
           employment_status_dummy == "Not working - looking for work" ~ "Unemployed",
           employment_status_dummy == "Not working - not looking for work" ~ "Not working",
           employment_status_dummy == "Full time employment" ~ "Full time employment",
           employment_status_dummy == "Part time employment" ~ "Part time employment",
           employment_status_dummy == "Retired" ~ "Retired",
           employment_status_dummy == "Unemployed" ~ "Unemployed",
           employment_status_dummy == "Not working" ~ "Not working",
           employment_status_dummy == "Other" ~ "Other",
           employment_status_dummy == "Full time student" ~ "Full time student"))

canada <- canada %>%
  mutate(country = "Canada",
         region_dummy = region,
         region = case_when(region_dummy == "British Columbia / Colombie Britanique" ~ "British Columbia",
                            region_dummy == "Ontario" ~ "Ontario",
                            region_dummy == "Alberta" ~ "Alberta",
                            region_dummy == "Manitoba" ~ "Manitoba",
                            region_dummy == "Quebec / Québec" ~ "Quebec",
                            region_dummy == "Nova Scotia / Nouvelle-Écosse" ~ "Nova Scotia",
                            region_dummy == "Yukon" ~ "Yukon",
                            region_dummy == "Prince Edward Island / Île-du-Prince-Édouard" ~ "Prince Edward Island",
                            region_dummy == "New Brunswick / Nouveau-Brunswick" ~ "New Brunswick",
                            region_dummy == "Saskatchewan" ~ "Saskatchewan",
                            region_dummy == "Newfoundland & Labrador / Terre-Neuve-et-Labrador" ~ "Newfoundland and Labrador",
                            region_dummy == "Northwest Territories / Territoires du Nord-Ouest" ~ "Northwest Territories",
                            region_dummy == "Nunavut" ~ "Nunavut"))

denmark <- denmark %>%
  mutate(country = "Denmark",
         region = NA,
         employment_status = case_when(employment_status_1 == "Yes" ~ "Full time employment",
                                        employment_status_2 == "Yes" ~ "Part time employment",
                                        employment_status_3 == "Yes" ~ "Full time student",
                                        employment_status_4 == "Yes" ~ "Retired",
                                        employment_status_5 == "Yes" ~ "Unemployed",
                                        employment_status_6 == "Yes" ~ "Not working",
                                        employment_status_7 == "Yes" ~ "Other"))
france <- france %>%
  mutate(country = "France",
         region = NA)
germany <- germany %>%
  mutate(country = "Germany",
         region = NA)
israel <- israel %>%
  mutate(country = "Israel",
         region = NA,
         employment_status = case_when(profile_work_stat == "Working full time (30 or more hours per week)" ~ "Full time employment",
                                       profile_work_stat == "Working part time (8-29 hours a week)" ~ "Part time employment",
                                       profile_work_stat == "Full time student" ~ "Full time student",
                                       profile_work_stat == "Retired" ~ "Retired",
                                       profile_work_stat == "Unemployed" ~ "Unemployed",
                                       profile_work_stat == "Not working" ~ "Not working",
                                       profile_work_stat == "Other" ~ "Other"))
italy <- italy %>%
  mutate(country = "Italy",
         region = NA)

japan <- japan %>%
  mutate(country = "Japan",
         region = NA)

netherlands <- netherlands %>% 
  mutate(country = "Netherlands",
         region = NA)

spain <- spain %>%
  mutate(country = "Spain",
         region = NA)

united_kingdom <- united_kingdom %>%
  mutate(country = "United Kingdom",
         region_dummy = region,
         region = case_when(region_dummy != "Wales" | 
                             region_dummy != "Scotland" | 
                             region_dummy != "Northern Ireland" ~ "England",
                           region_dummy == "Wales" ~ "Wales",
                           region_dummy == "Scotland" ~ "Scotland",
                           region_dummy == "Northern Ireland" ~ "Northern Ireland"))

australia <- australia %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
canada <- canada %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
denmark <- denmark %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
        d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
        d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
        d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
france <- france %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
germany <- germany %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
israel <- israel %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
italy <- italy %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
japan <- japan %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
netherlands <- netherlands %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
spain <- spain %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)
united_kingdom <- united_kingdom %>%
  select(RecordNo, endtime, qweek, age, gender, employment_status, country, region, weight, d1_health_1, 
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, d1_health_98, d1_health_99, PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4)

survey_data <- rbind(australia, canada, denmark, france, germany, israel, italy, japan, 
                   netherlands, spain, united_kingdom)

survey_data <- survey_data %>%
  mutate(chroncon1 = case_when(d1_health_1 == "No" ~ 0,
                               d1_health_1 == "Yes" ~ 1),
         chroncon2 = case_when(d1_health_2 == "No" ~ 0,
                               d1_health_2 == "Yes" ~ 1),
         chroncon3 = case_when(d1_health_3 == "No" ~ 0,
                               d1_health_3 == "Yes" ~ 1),
         chroncon4 = case_when(d1_health_4 == "No" ~ 0,
                               d1_health_4 == "Yes" ~ 1),
         chroncon5 = case_when(d1_health_5 == "No" ~ 0,
                               d1_health_5 == "Yes" ~ 1),
         chroncon6 = case_when(d1_health_6 == "No" ~ 0,
                               d1_health_6 == "Yes" ~ 1),
         chroncon7 = case_when(d1_health_7 == "No" ~ 0,
                               d1_health_7 == "Yes" ~ 1),
         chroncon8 = case_when(d1_health_8 == "No" ~ 0,
                               d1_health_8 == "Yes" ~ 1),
         chroncon9 = case_when(d1_health_9 == "No" ~ 0,
                               d1_health_9 == "Yes" ~ 1),
         chroncon10 = case_when(d1_health_10 == "No" ~ 0,
                               d1_health_10 == "Yes" ~ 1),
         chroncon11 = case_when(d1_health_11 == "No" ~ 0,
                               d1_health_11 == "Yes" ~ 1),
         chroncon12 = case_when(d1_health_12 == "No" ~ 0,
                               d1_health_12 == "Yes" ~ 1),
         chroncon13 = case_when(d1_health_13 == "No" ~ 0,
                               d1_health_13 == "Yes" ~ 1),
         chroncon98 = case_when(d1_health_98 == "No" ~ 0,
                               d1_health_98 == "Yes" ~ 1),
         chroncon99 = case_when(d1_health_99 == "No" ~ 0,
                               d1_health_99 == "Yes" ~ 1),
         endtime = as.POSIXct(endtime, format = "%d/%m/%Y %H:%M"))

survey_data <- survey_data %>% 
  mutate(sex = case_when(gender == "Female" ~ 0,
                         gender == "Male" ~ 1),
         age_range = case_when(age < 24 ~ "18-24",
                               age >= 25 & age < 35 ~ "25-34",
                               age >= 35 & age < 45 ~ "35-44",
                               age >= 45 & age < 55 ~ "45-54",
                               age >= 55 & age < 65 ~ "55-64",
                               age >= 65  ~ "65+"),
         date = format(endtime, format = "%Y-%m-%d"),
         date = ymd(date),
         date_range = case_when(date >= "2020-01-01" & date < "2020-04-01" ~ "Q1",
                                date >= "2020-04-01" & date < "2020-07-01" ~ "Q2",
                                date >= "2020-07-01" & date < "2020-10-01" ~ "Q3",
                                date >= "2020-10-01" & date < "2021-01-01" ~ "Q4",
                                date >= "2021-01-01" & date < "2021-04-01" ~ "Q5",
                                date >= "2021-04-01" & date < "2021-07-01" ~ "Q6",
                                date >= "2021-07-01" & date < "2021-10-01" ~ "Q7",
                                date >= "2021-10-01" ~ "Q8"),
         employment = case_when(employment_status == "Full time employment" |  
                                  employment_status == "Part time employment"~ "Employed", #Employed
                                employment_status == "Full time student" | 
                                  employment_status == "Other" ~ "Student/Other",               #Student/Other
                                employment_status == "Retired" ~ "Retired",               #Retired
                                employment_status == "Unemployed" | 
                                  employment_status == "Not working" ~ "Unemployed"),        #Unemployed
         chronic_disease = case_when(chroncon1 + chroncon2 + chroncon3 + chroncon4 + 
                                       chroncon5 + chroncon6 + chroncon7 + chroncon8 +
                                       chroncon9 + chroncon10 + chroncon11 + chroncon12 + 
                                       chroncon13 == 0 ~ "No",
                                     chroncon1 + chroncon2 + chroncon3 + chroncon4 + 
                                       chroncon5 + chroncon6 + chroncon7 + chroncon8 +
                                       chroncon9 + chroncon10 + chroncon11 + chroncon12 + 
                                       chroncon13 > 0 ~ "Yes"),
         n_chronic_disease = chroncon1 + chroncon2 + chroncon3 + chroncon4 + 
           chroncon5 + chroncon6 + chroncon7 + chroncon8 +
           chroncon9 + chroncon10 + chroncon11 + chroncon12 + 
           chroncon13,
         anxiety = case_when(PHQ4_3 == "Not at all" | 
                               PHQ4_3 == "Several days" | 
                               PHQ4_4 == "Not at all" | 
                               PHQ4_4 == "Several days" ~ "No",
                             PHQ4_3 == "More than half the days" | 
                               PHQ4_3 == "Nearly every day" | 
                               PHQ4_4 == "More than half the days" | 
                               PHQ4_4 == "Nearly every day" ~ "Yes"),
         depression = case_when(PHQ4_1 == "Not at all" | 
                                  PHQ4_1 == "Several days" | 
                                  PHQ4_2 == "Not at all" | 
                                  PHQ4_2 == "Several days" ~ "No",
                                PHQ4_1 == "More than half the days" | 
                                 PHQ4_1 == "Nearly every day" | 
                                 PHQ4_2 == "More than half the days" | 
                                 PHQ4_2 == "Nearly every day" ~ "Yes"))
survey_data$chronic_disease[is.na(survey_data$chronic_disease)] <- "No"
survey_data$n_chronic_disease[is.na(survey_data$n_chronic_disease)] <- "No"

survey_data <- survey_data[complete.cases(survey_data[, 25:28]),]

#Adding levels to data
survey_data$age <- as.numeric(survey_data$age)
survey_data$gender <- factor(survey_data$gender, levels = c("Female", "Male"))
survey_data$employment_status <- factor(survey_data$employment_status, levels = c("Full time employment", "Part time employment", "Full time student", "Retired", "Unemployed", "Not working", "Other"))
survey_data$date_range <- factor(survey_data$date_range)
survey_data$age_range <- factor(survey_data$age_range, levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
survey_data$employment <- factor(survey_data$employment, levels = c("Employed", "Student/Other", "Retired", "Unemployed"))
survey_data$chronic_disease <- factor(survey_data$chronic_disease, levels = c("No", "Yes"))
survey_data$anxiety <- factor(survey_data$anxiety, levels = c("No", "Yes"))
survey_data$depression <- factor(survey_data$depression, levels = c("No", "Yes"))
survey_data$n_chronic_disease <- as.numeric(survey_data$n_chronic_disease)

#Importing Oxford Policy Data raw file
policy_data <- read_csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/OxCGRT_latest.csv")
policy_data$Date <- as.character(policy_data$Date)

policy_data <- policy_data %>%
  mutate(country = CountryName,
         region = RegionName,
         date = as.Date(Date, format = "%Y%m%d"),
         c1_s = as.numeric(`C1_School closing`),
         f_c1 = as.numeric(C1_Flag),
         c2_w = as.numeric(`C2_Workplace closing`),
         f_c2 = as.numeric(C2_Flag),
         c3_c = as.numeric(`C3_Cancel public events`),
         f_c3 = as.numeric(C3_Flag),
         c4_r = as.numeric(`C4_Restrictions on gatherings`),
         f_c4 = as.numeric(C4_Flag),
         c5_c = as.numeric(`C5_Close public transport`),
         f_c5 = as.numeric(C5_Flag),
         c6_s = as.numeric(`C6_Stay at home requirements`),
         f_c6 = as.numeric(C6_Flag),
         c7_r = as.numeric(`C7_Restrictions on internal movement`),
         f_c7 = as.numeric(C7_Flag),
         c8_i = as.numeric(`C8_International travel controls`),
         e1_i = as.numeric(`E1_Income support`),
         f_e1 = as.numeric(E1_Flag),
         e2_r = as.numeric(`E2_Debt/contract relief`),
         e3_f = as.numeric(`E3_Fiscal measures`),
         e4_s = as.numeric(`E4_International support`),
         h1_p = as.numeric(`H1_Public information campaigns`),
         f_h1 = as.numeric(H1_Flag),
         h2_t = as.numeric(`H2_Testing policy`),
         h3_c = as.numeric(`H3_Contact tracing`),
         h4_e = as.numeric(`H4_Emergency investment in healthcare`),
         h5_v = as.numeric(`H5_Investment in vaccines`),
         h6_f = as.numeric(`H6_Facial Coverings`),
         f_h6 = as.numeric(H6_Flag),
         h7_p = as.numeric(`H7_Vaccination policy`),
         f_h7 = as.numeric(H7_Flag),
         h8_e = as.numeric(`H8_Protection of elderly people`),
         f_h8 = as.numeric(H8_Flag),
         indexc1 = 0,
         indexc2 = 0,
         indexc3 = 0,
         indexc4 = 0,
         indexc5 = 0,
         indexc6 = 0,
         indexc7 = 0,
         indexc8 = 0,
         indexh6 = 0)
policy_data$f_c1[is.na(policy_data$f_c1)] <- 0
policy_data$f_c2[is.na(policy_data$f_c2)] <- 0
policy_data$f_c3[is.na(policy_data$f_c3)] <- 0
policy_data$f_c4[is.na(policy_data$f_c4)] <- 0
policy_data$f_c5[is.na(policy_data$f_c5)] <- 0
policy_data$f_c6[is.na(policy_data$f_c6)] <- 0
policy_data$f_c7[is.na(policy_data$f_c7)] <- 0
policy_data$f_e1[is.na(policy_data$f_e1)] <- 0
policy_data$f_h1[is.na(policy_data$f_h1)] <- 0
policy_data$f_h6[is.na(policy_data$f_h6)] <- 0
policy_data$f_h7[is.na(policy_data$f_h7)] <- 0
policy_data$f_h8[is.na(policy_data$f_h8)] <- 0

#Create individual policy index
policy_data$indexc1[policy_data$c1_s > 0 & !is.na(policy_data$c1_s)] <- 100*((policy_data$c1_s-0.5*(1-policy_data$f_c1))/3)
policy_data$indexc1[policy_data$c1_s > 0 && !is.na(policy_data$c1_s)] <- 100*((policy_data$c1_s-0.5*(1-policy_data$f_c1))/3)

policy_data$indexc2[policy_data$c2_w > 0 & !is.na(policy_data$c2_w)] <- 100*((policy_data$c2_w-0.5*(1-policy_data$f_c2))/3)
policy_data$indexc2[policy_data$c2_w > 0 && !is.na(policy_data$c2_w)] <- 100*((policy_data$c2_w-0.5*(1-policy_data$f_c2))/3)

policy_data$indexc3[policy_data$c3_c > 0 & !is.na(policy_data$c3_c)] <- 100*((policy_data$c3_c-0.5*(1-policy_data$f_c3))/2)
policy_data$indexc3[policy_data$c3_c > 0 && !is.na(policy_data$c3_c)] <- 100*((policy_data$c3_c-0.5*(1-policy_data$f_c3))/2)

policy_data$indexc4[policy_data$c4_r > 0 & !is.na(policy_data$c4_r)] <- 100*((policy_data$c4_r-0.5*(1-policy_data$f_c4))/4)
policy_data$indexc4[policy_data$c4_r > 0 && !is.na(policy_data$c4_r)] <- 100*((policy_data$c4_r-0.5*(1-policy_data$f_c4))/4)

policy_data$indexc5[policy_data$c5_c > 0 & !is.na(policy_data$c5_c)] <- 100*((policy_data$c5_c-0.5*(1-policy_data$f_c5))/2)
policy_data$indexc5[policy_data$c5_c > 0 && !is.na(policy_data$c5_c)] <- 100*((policy_data$c5_c-0.5*(1-policy_data$f_c5))/2)

policy_data$indexc6[policy_data$c6_s > 0 & !is.na(policy_data$c6_s)] <- 100*((policy_data$c6_s-0.5*(1-policy_data$f_c6))/3)
policy_data$indexc6[policy_data$c6_s > 0 && !is.na(policy_data$c6_s)] <- 100*((policy_data$c6_s-0.5*(1-policy_data$f_c6))/3)

policy_data$indexc7[policy_data$c7_r > 0 & !is.na(policy_data$c7_r)] <- 100*((policy_data$c7_r-0.5*(1-policy_data$f_c7))/2)
policy_data$indexc7[policy_data$c7_r > 0 && !is.na(policy_data$c7_r)] <- 100*((policy_data$c7_r-0.5*(1-policy_data$f_c7))/2)

policy_data$indexc8[policy_data$c8_i > 0 & !is.na(policy_data$c8_i)] <- 100*((policy_data$c8_i)/4)
policy_data$indexc8[policy_data$c8_i > 0 && !is.na(policy_data$c8_i)] <- 100*((policy_data$c8_i)/4)

policy_data$indexh6[policy_data$h6_f > 0 & !is.na(policy_data$h6_f)] <- 100*((policy_data$h6_f-0.5*(1-policy_data$f_h6))/4)
policy_data$indexh6[policy_data$h6_f > 0 && !is.na(policy_data$h6_f)] <- 100*((policy_data$h6_f-0.5*(1-policy_data$f_h6))/4)

#Create required policy indicator
policy_data <- policy_data %>%
  mutate(req_c1 = case_when(`C1_School closing` > 1 & !is.na(`C1_School closing`) ~ 1),
         req_c2 = case_when(`C2_Workplace closing` > 1 & !is.na(`C2_Workplace closing`) ~ 1),
         req_c3 = case_when(`C3_Cancel public events` > 1 & !is.na(`C3_Cancel public events`) ~ 1),
         req_c4 = case_when(`C4_Restrictions on gatherings` > 1 & !is.na(`C4_Restrictions on gatherings`) ~ 1),
         req_c5 = case_when(`C5_Close public transport` > 1 & !is.na(`C5_Close public transport`) ~ 1),
         req_c6 = case_when(`C6_Stay at home requirements` > 1 & !is.na(`C6_Stay at home requirements`) ~ 1),
         req_c7 = case_when(`C7_Restrictions on internal movement` > 1 & !is.na(`C7_Restrictions on internal movement`) ~ 1),
         req_e1 = case_when(`E1_Income support` > 1 & !is.na(`E1_Income support`) ~ 1),
         req_e2 = case_when(`E2_Debt/contract relief` > 1 &is.na(`E2_Debt/contract relief`) ~ 1),
         req_e3 = case_when(`E3_Fiscal measures` > 1 &is.na(`E3_Fiscal measures`) ~ 1),
         req_e4 = case_when(`E4_International support` > 1 & is.na(`E4_International support`) ~ 1),
         req_h1 = case_when(`H1_Public information campaigns` > 1 & is.na(`H1_Public information campaigns`) ~ 1),
         req_h2 = case_when(`H2_Testing policy` > 1 & is.na(`H2_Testing policy`) ~ 1),
         req_h3 = case_when(`H3_Contact tracing` > 1 & is.na(`H3_Contact tracing`) ~ 1),
         req_h4 = case_when(`H4_Emergency investment in healthcare` > 1 & is.na(`H4_Emergency investment in healthcare`) ~ 1),
         req_h5 = case_when(`H5_Investment in vaccines` > 1 & is.na(`H5_Investment in vaccines`) ~ 1),
         req_h6 = case_when(`H6_Facial Coverings` > 1 & is.na(`H6_Facial Coverings`) ~ 1),
         req_h7 = case_when(`H7_Vaccination policy` > 1 & is.na(`H7_Vaccination policy`) ~ 1),
         req_h8 = case_when(`H8_Protection of elderly people` > 1 & is.na(`H8_Protection of elderly people`) ~ 1))
policy_data$req_c1[is.na(policy_data$req_c1)] <- 0
policy_data$req_c2[is.na(policy_data$req_c2)] <- 0
policy_data$req_c3[is.na(policy_data$req_c3)] <- 0
policy_data$req_c4[is.na(policy_data$req_c4)] <- 0
policy_data$req_c5[is.na(policy_data$req_c5)] <- 0
policy_data$req_c6[is.na(policy_data$req_c6)] <- 0
policy_data$req_c7[is.na(policy_data$req_c7)] <- 0
policy_data$req_e1[is.na(policy_data$req_e1)] <- 0
policy_data$req_e2[is.na(policy_data$req_e2)] <- 0
policy_data$req_e3[is.na(policy_data$req_e3)] <- 0
policy_data$req_e4[is.na(policy_data$req_e4)] <- 0
policy_data$req_h1[is.na(policy_data$req_h1)] <- 0
policy_data$req_h2[is.na(policy_data$req_h2)] <- 0
policy_data$req_h3[is.na(policy_data$req_h3)] <- 0
policy_data$req_h4[is.na(policy_data$req_h4)] <- 0
policy_data$req_h5[is.na(policy_data$req_h5)] <- 0
policy_data$req_h6[is.na(policy_data$req_h6)] <- 0
policy_data$req_h7[is.na(policy_data$req_h7)] <- 0
policy_data$req_h8[is.na(policy_data$req_h8)] <- 0

policy_data <- policy_data %>%
  mutate(sum_req_c1_c7 = req_c1 + req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + req_c7,
         sum_req_e1_e4 = req_e1 + req_e2 + req_e3 + req_e4,
         sum_req_h1_h8 = req_h1 + req_h2 + req_h3 + req_h4 + req_h5 + req_h6 + req_h7 + req_h8,
         date_range = case_when(date >= "2020-01-01" & date < "2020-04-01" ~ "Q1",
                                date >= "2020-04-01" & date < "2020-07-01" ~ "Q2",
                                date >= "2020-07-01" & date < "2020-10-01" ~ "Q3",
                                date >= "2020-10-01" & date < "2021-01-01" ~ "Q4",
                                date >= "2021-01-01" & date < "2021-04-01" ~ "Q5",
                                date >= "2021-04-01" & date < "2021-07-01" ~ "Q6",
                                date >= "2021-07-01" & date < "2021-10-01" ~ "Q7",
                                date >= "2021-10-01" ~ "Q8"))
policy_data <- policy_data %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(daily_cases = c(NA, diff(ConfirmedCases)),
         daily_deaths = c(NA, diff(ConfirmedDeaths)),
         daily_cases_MA = (lag(daily_cases, n = 3L) + lag(daily_cases, n = 2L) + 
           lag(daily_cases, n = 1L) + daily_cases + lead(daily_cases, n = 1L) + 
           lead(daily_cases, n = 2L) + lead(daily_cases, n = 3L)) / 7,
         daily_deaths_MA = (lag(daily_deaths, n = 3L) + lag(daily_deaths, n = 2L) + 
           lag(daily_deaths, n = 1L) + daily_deaths + lead(daily_deaths, n = 1L) + 
           lead(daily_deaths, n = 2L) + lead(daily_deaths, n = 3L)) / 7) %>%
  ungroup()

policy_data <- policy_data %>%
  select(country, region, date, date_range, c1_s, f_c1, c2_w, f_c2, c3_c, f_c3, c4_r, f_c4, c5_c, 
         f_c5, c6_s, f_c6, c7_r, f_c7, c8_i, e1_i, f_e1, e2_r, e3_f, e4_s, 
         h1_p, f_h1, h2_t, h3_c, h4_e, h5_v, h6_f, f_h6, h7_p, f_h7, h8_e, f_h8,
         req_c1, req_c2, req_c3, req_c4, req_c5, req_c6, req_c7, req_e1, req_e2,
         req_e3, req_e4, req_h1, req_h2, req_h3, req_h4, req_h5, req_h6, req_h7,
         req_h8, sum_req_c1_c7, sum_req_e1_e4, sum_req_h1_h8, daily_cases,
         daily_deaths, daily_cases_MA, daily_deaths_MA)

#Selecting countries to match survey countries
policy_data <- policy_data %>%
  filter(country %in% c("Australia", "Canada", "Denmark", "Germany", "Israel", "Italy", "Japan", "Netherlands", "Spain", "United Kingdom"))

#Saving survey and policy data to separate files
write_csv(survey_data, file = "/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/surveydata.csv")
write.csv(policy_data, file = "/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/policydata.csv")
