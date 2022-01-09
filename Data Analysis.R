#R Packages
library(plyr)
library(summarytools)
library(dplyr)
library(mice)
library(lmtest)

#Import data files
survey_data <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/surveydata.csv", header = TRUE, na.strings = c("", " ", "NA"))
policy_data <- read.csv("/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/policydata.csv", header = TRUE, na.strings = c("", " ", "NA"))
survey_data$date <- as.Date(survey_data$date, format = "%Y-%m-%d")

#Merging data files
full_data <- merge(survey_data, policy_data, by = c("country", "region", "date", "date_range"), all.x = TRUE)

full_data <- full_data[-188287, ]

#Adding levels to data files
full_data$country <- factor(full_data$country)
full_data$date_range <- factor(full_data$date_range)
full_data$gender <- factor(full_data$gender)
full_data$employment_status <- factor(full_data$employment_status)
full_data$d1_health_1 <- factor(full_data$d1_health_1)
full_data$d1_health_2 <- factor(full_data$d1_health_2)
full_data$d1_health_3 <- factor(full_data$d1_health_3)
full_data$d1_health_4 <- factor(full_data$d1_health_4)
full_data$d1_health_5 <- factor(full_data$d1_health_5)
full_data$d1_health_6 <- factor(full_data$d1_health_6)
full_data$d1_health_7 <- factor(full_data$d1_health_7)
full_data$d1_health_8 <- factor(full_data$d1_health_8)
full_data$d1_health_9 <- factor(full_data$d1_health_9)
full_data$d1_health_10 <- factor(full_data$d1_health_10)
full_data$d1_health_11 <- factor(full_data$d1_health_11)
full_data$d1_health_12 <- factor(full_data$d1_health_12)
full_data$d1_health_13 <- factor(full_data$d1_health_13)
full_data$age_range <- factor(full_data$age_range)
full_data$employment <- factor(full_data$employment)
full_data$chronic_disease <- factor(full_data$chronic_disease)
full_data$anxiety <- factor(full_data$anxiety)
full_data$depression <- factor(full_data$depression)
full_data$c1_s <- factor(as.character(full_data$c1_s))
full_data$f_c1 <- factor(as.character(full_data$f_c1))
full_data$c2_w <- factor(as.character(full_data$c2_w))
full_data$f_c2 <- factor(as.character(full_data$f_c2))
full_data$c3_c <- factor(as.character(full_data$c3_c))
full_data$f_c3 <- factor(as.character(full_data$f_c3))
full_data$c4_r <- factor(as.character(full_data$c4_r))
full_data$f_c4 <- factor(as.character(full_data$f_c4))
full_data$c5_c <- factor(as.character(full_data$c5_c))
full_data$f_c5 <- factor(as.character(full_data$f_c5))
full_data$c6_s <- factor(as.character(full_data$c6_s))
full_data$f_c6 <- factor(as.character(full_data$f_c6))
full_data$c7_r <- factor(as.character(full_data$c7_r))
full_data$f_c7 <- factor(as.character(full_data$f_c7))
full_data$c8_i <- factor(as.character(full_data$c8_i))
full_data$e1_i <- factor(as.character(full_data$e1_i))
full_data$f_e1 <- factor(as.character(full_data$f_e1))
full_data$e2_r <- factor(as.character(full_data$e2_r))
full_data$e3_f <- factor(as.character(full_data$e3_f))
full_data$e4_s <- factor(as.character(full_data$e4_s))
full_data$h1_p <- factor(as.character(full_data$h1_p))
full_data$f_h1 <- factor(as.character(full_data$f_h1))
full_data$h2_t <- factor(as.character(full_data$h2_t))
full_data$h3_c <- factor(as.character(full_data$h3_c))
full_data$h4_e <- factor(as.character(full_data$h4_e))
full_data$h5_v <- factor(as.character(full_data$h5_v))
full_data$h6_f <- factor(as.character(full_data$h6_f))
full_data$f_h6 <- factor(as.character(full_data$f_h6))
full_data$h7_p <- factor(as.character(full_data$h7_p))
full_data$f_h7 <- factor(as.character(full_data$f_h7))
full_data$h8_e <- factor(as.character(full_data$h8_e))
full_data$f_h8 <- factor(as.character(full_data$f_h8))
full_data$req_c1 <- factor(as.character(full_data$req_c1))
full_data$req_c2 <- factor(as.character(full_data$req_c2))
full_data$req_c3 <- factor(as.character(full_data$req_c3))
full_data$req_c4 <- factor(as.character(full_data$req_c4))
full_data$req_c5 <- factor(as.character(full_data$req_c5))
full_data$req_c6 <- factor(as.character(full_data$req_c6))
full_data$req_c7 <- factor(as.character(full_data$req_c7))
full_data$req_e1 <- factor(as.character(full_data$req_e1))
full_data$req_e2 <- factor(as.character(full_data$req_e2))
full_data$req_e3 <- factor(as.character(full_data$req_e3))
full_data$req_e4 <- factor(as.character(full_data$req_e4))
full_data$req_h1 <- factor(as.character(full_data$req_h1))
full_data$req_h2 <- factor(as.character(full_data$req_h2))
full_data$req_h3 <- factor(as.character(full_data$req_h3))
full_data$req_h4 <- factor(as.character(full_data$req_h4))
full_data$req_h5 <- factor(as.character(full_data$req_h5))
full_data$req_h6 <- factor(as.character(full_data$req_h6))
full_data$req_h7 <- factor(as.character(full_data$req_h7))
full_data$req_h8 <- factor(as.character(full_data$req_h8))

#Saving full data file
write.csv(full_data, file = "/Users/robinvankessel/OneDrive/Documenten/Publications/Assistant Professor UM/14 - TBD/fulldata.csv")

#Demographic overview
     #Overall
summary(full_datagender)
descr(full_data$age)
summary(full_data$employment)
summary(full_data$age_range)
summary(full_data$chronic_disease)
summary(full_data$d1_health_1)
summary(full_data$d1_health_2)
summary(full_data$d1_health_3)
summary(full_data$d1_health_4)
summary(full_data$d1_health_5)
summary(full_data$d1_health_6)
summary(full_data$d1_health_7)
summary(full_data$d1_health_8)
summary(full_data$d1_health_9)
summary(full_data$d1_health_10)
summary(full_data$d1_health_11)
summary(full_data$d1_health_12)
summary(full_data$d1_health_13)
     #By country
#Australia
aus_data <- full_data %>%
  filter(country == "Australia")
summary(aus_data$gender)
descr(aus_data$age)
summary(aus_data$age_range)
summary(aus_data$employment)
summary(aus_data$chronic_disease)
summary(aus_data$d1_health_1)
summary(aus_data$d1_health_2)
summary(aus_data$d1_health_3)
summary(aus_data$d1_health_4)
summary(aus_data$d1_health_5)
summary(aus_data$d1_health_6)
summary(aus_data$d1_health_7)
summary(aus_data$d1_health_8)
summary(aus_data$d1_health_9)
summary(aus_data$d1_health_10)
summary(aus_data$d1_health_11)
summary(aus_data$d1_health_12)
summary(aus_data$d1_health_13)

#Canada
can_data <- full_data %>%
  filter(country == "Canada")
summary(can_data$gender)
descr(can_data$age)
summary(can_data$age_range)
summary(can_data$employment)
summary(can_data$chronic_disease)
summary(can_data$d1_health_1)
summary(can_data$d1_health_2)
summary(can_data$d1_health_3)
summary(can_data$d1_health_4)
summary(can_data$d1_health_5)
summary(can_data$d1_health_6)
summary(can_data$d1_health_7)
summary(can_data$d1_health_8)
summary(can_data$d1_health_9)
summary(can_data$d1_health_10)
summary(can_data$d1_health_11)
summary(can_data$d1_health_12)
summary(can_data$d1_health_13)

#Denmark
den_data <- full_data %>%
  filter(country == "Denmark")
summary(den_data$gender)
descr(den_data$age)
summary(den_data$age_range)
summary(den_data$employment)
summary(den_data$chronic_disease)
summary(den_data$d1_health_1)
summary(den_data$d1_health_2)
summary(den_data$d1_health_3)
summary(den_data$d1_health_4)
summary(den_data$d1_health_5)
summary(den_data$d1_health_6)
summary(den_data$d1_health_7)
summary(den_data$d1_health_8)
summary(den_data$d1_health_9)
summary(den_data$d1_health_10)
summary(den_data$d1_health_11)
summary(den_data$d1_health_12)
summary(den_data$d1_health_13)

#Germany
ger_data <- full_data %>%
  filter(country == "Germany")
summary(ger_data$gender)
descr(ger_data$age)
summary(ger_data$age_range)
summary(ger_data$employment)
summary(ger_data$chronic_disease)
summary(ger_data$d1_health_1)
summary(ger_data$d1_health_2)
summary(ger_data$d1_health_3)
summary(ger_data$d1_health_4)
summary(ger_data$d1_health_5)
summary(ger_data$d1_health_6)
summary(ger_data$d1_health_7)
summary(ger_data$d1_health_8)
summary(ger_data$d1_health_9)
summary(ger_data$d1_health_10)
summary(ger_data$d1_health_11)
summary(ger_data$d1_health_12)
summary(ger_data$d1_health_13)

#Israel
isr_data <- full_data %>%
  filter(country == "Israel")
summary(isr_data$gender)
descr(isr_data$age)
summary(isr_data$age_range)
summary(isr_data$employment)
summary(isr_data$chronic_disease)
summary(isr_data$d1_health_1)
summary(isr_data$d1_health_2)
summary(isr_data$d1_health_3)
summary(isr_data$d1_health_4)
summary(isr_data$d1_health_5)
summary(isr_data$d1_health_6)
summary(isr_data$d1_health_7)
summary(isr_data$d1_health_8)
summary(isr_data$d1_health_9)
summary(isr_data$d1_health_10)
summary(isr_data$d1_health_11)
summary(isr_data$d1_health_12)
summary(isr_data$d1_health_13)

#Italy
ita_data <- full_data %>%
  filter(country == "Italy")
summary(ita_data$gender)
descr(ita_data$age)
summary(ita_data$age_range)
summary(ita_data$employment)
summary(ita_data$chronic_disease)
summary(ita_data$d1_health_1)
summary(ita_data$d1_health_2)
summary(ita_data$d1_health_3)
summary(ita_data$d1_health_4)
summary(ita_data$d1_health_5)
summary(ita_data$d1_health_6)
summary(ita_data$d1_health_7)
summary(ita_data$d1_health_8)
summary(ita_data$d1_health_9)
summary(ita_data$d1_health_10)
summary(ita_data$d1_health_11)
summary(ita_data$d1_health_12)
summary(ita_data$d1_health_13)

#Japan
jpn_data <- full_data %>%
  filter(country == "Japan")
summary(jpn_data$gender)
descr(jpn_data$age)
summary(jpn_data$age_range)
summary(jpn_data$employment)
summary(jpn_data$chronic_disease)
summary(jpn_data$d1_health_1)
summary(jpn_data$d1_health_2)
summary(jpn_data$d1_health_3)
summary(jpn_data$d1_health_4)
summary(jpn_data$d1_health_5)
summary(jpn_data$d1_health_6)
summary(jpn_data$d1_health_7)
summary(jpn_data$d1_health_8)
summary(jpn_data$d1_health_9)
summary(jpn_data$d1_health_10)
summary(jpn_data$d1_health_11)
summary(jpn_data$d1_health_12)
summary(jpn_data$d1_health_13)

#Netherlands
nld_data <- full_data %>%
  filter(country == "Netherlands")
summary(nld_data$gender)
descr(nld_data$age)
summary(nld_data$age_range)
summary(nld_data$employment)
summary(nld_data$chronic_disease)
summary(nld_data$d1_health_1)
summary(nld_data$d1_health_2)
summary(nld_data$d1_health_3)
summary(nld_data$d1_health_4)
summary(nld_data$d1_health_5)
summary(nld_data$d1_health_6)
summary(nld_data$d1_health_7)
summary(nld_data$d1_health_8)
summary(nld_data$d1_health_9)
summary(nld_data$d1_health_10)
summary(nld_data$d1_health_11)
summary(nld_data$d1_health_12)
summary(nld_data$d1_health_13)

#Spain
esp_data <- full_data %>%
  filter(country == "Spain")
summary(esp_data$gender)
descr(esp_data$age)
summary(esp_data$age_range)
summary(esp_data$employment)
summary(esp_data$chronic_disease)
summary(esp_data$d1_health_1)
summary(esp_data$d1_health_2)
summary(esp_data$d1_health_3)
summary(esp_data$d1_health_4)
summary(esp_data$d1_health_5)
summary(esp_data$d1_health_6)
summary(esp_data$d1_health_7)
summary(esp_data$d1_health_8)
summary(esp_data$d1_health_9)
summary(esp_data$d1_health_10)
summary(esp_data$d1_health_11)
summary(esp_data$d1_health_12)
summary(esp_data$d1_health_13)

#United Kingdom
ukd_data <- full_data %>%
  filter(country == "United Kingdom")
summary(ukd_data$gender)
descr(ukd_data$age)
summary(ukd_data$age_range)
summary(ukd_data$employment)
summary(ukd_data$chronic_disease)
summary(ukd_data$d1_health_1)
summary(ukd_data$d1_health_2)
summary(ukd_data$d1_health_3)
summary(ukd_data$d1_health_4)
summary(ukd_data$d1_health_5)
summary(ukd_data$d1_health_6)
summary(ukd_data$d1_health_7)
summary(ukd_data$d1_health_8)
summary(ukd_data$d1_health_9)
summary(ukd_data$d1_health_10)
summary(ukd_data$d1_health_11)
summary(ukd_data$d1_health_12)
summary(ukd_data$d1_health_13)

#Define logistic regression model for anxiety and depression
anx_model <- glm(formula = anxiety ~ gender + 
                   age_range + employment + 
                   chronic_disease + d1_health_1 + d1_health_2 + 
                   d1_health_3 + d1_health_4 + d1_health_5 +
                   d1_health_6 + d1_health_7 + d1_health_8 + 
                   d1_health_9 + d1_health_10 + d1_health_11 + 
                   d1_health_12 + d1_health_13 + req_c1 + 
                   req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                   req_c7 + req_e1,
                 family = binomial(link = "logit"), data = full_data)

dpr_model <- glm(formula = depression ~ gender + 
                   age_range + employment + 
                   chronic_disease + d1_health_1 + d1_health_2 + 
                   d1_health_3 + d1_health_4 + d1_health_5 +
                   d1_health_6 + d1_health_7 + d1_health_8 + 
                   d1_health_9 + d1_health_10 + d1_health_11 + 
                   d1_health_12 + d1_health_13 + date_range + req_c1 + 
                   req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                   req_c7 + req_e1,
                 family = binomial(link = "logit"), data = full_data)

#Test for heteroscedasticity using Breusch-Pagan test
bptest(anx_model, ~ gender + 
         age_range + employment + 
         chronic_disease + d1_health_1 + d1_health_2 + 
         d1_health_3 + d1_health_4 + d1_health_5 +
         d1_health_6 + d1_health_7 + d1_health_8 + 
         d1_health_9 + d1_health_10 + d1_health_11 + 
         d1_health_12 + d1_health_13 + date_range + req_c1 + 
         req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
         req_c7 + req_e1, data = full_data)

bptest(dpr_model, ~ gender + 
         age_range + employment + 
         chronic_disease + d1_health_1 + d1_health_2 + 
         d1_health_3 + d1_health_4 + d1_health_5 +
         d1_health_6 + d1_health_7 + d1_health_8 + 
         d1_health_9 + d1_health_10 + d1_health_11 + 
         d1_health_12 + d1_health_13 + date_range + req_c1 + 
         req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
         req_c7 + req_e1, data = full_data)

#Create weighted models for anxiety and depression (complete case)
weighted_anx_model <- glm(formula = anxiety ~ gender + 
                            age_range + employment + 
                            chronic_disease + d1_health_1 + d1_health_2 + 
                            d1_health_3 + d1_health_4 + d1_health_5 +
                            d1_health_6 + d1_health_7 + d1_health_8 + 
                            d1_health_9 + d1_health_10 + d1_health_11 + 
                            d1_health_12 + d1_health_13 + date_range + req_c1 + 
                            req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                            req_c7 + req_e1,
                          family = binomial(link = "logit"), weights = weight, data = full_data)

weighted_dpr_model <- glm(formula = depression ~ gender + 
                            age_range + employment + 
                            chronic_disease + d1_health_1 + d1_health_2 + 
                            d1_health_3 + d1_health_4 + d1_health_5 +
                            d1_health_6 + d1_health_7 + d1_health_8 + 
                            d1_health_9 + d1_health_10 + d1_health_11 + 
                            d1_health_12 + d1_health_13 + date_range + req_c1 + 
                            req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                            req_c7 + req_e1, family = binomial(link = "logit"), 
                            weights = weight, data = full_data)

#Analysis of the complete case models
summary(weighted_anx_model)
exp(coef(weighted_anx_model))
exp(confint(weighted_anx_model))

summary(weighted_dpr_model)
exp(coef(weighted_dpr_model))
exp(confint(weighted_dpr_model))

#Compute Multiple Imputation by Chained Equation Model
df2 <- full_data %>%
  select(country, gender, age_range, employment, weight, chronic_disease, d1_health_1,
         d1_health_2, d1_health_3, d1_health_4, d1_health_5, d1_health_6, 
         d1_health_7, d1_health_8, d1_health_9, d1_health_10, d1_health_11, 
         d1_health_12, d1_health_13, date_range, req_c1, req_c2, req_c3, 
         req_c4, req_c5, req_c6, req_c7, req_e1)

temp_data <- mice(data = df2, m = 18, method = "pmm", maxit = 30, seed = 500)

imp_anx_model <- with(temp_data, glm(formula = anxiety ~ gender + 
                                       age_range + employment + 
                                       chronic_disease + d1_health_1 + d1_health_2 + 
                                       d1_health_3 + d1_health_4 + d1_health_5 +
                                       d1_health_6 + d1_health_7 + d1_health_8 + 
                                       d1_health_9 + d1_health_10 + d1_health_11 + 
                                       d1_health_12 + d1_health_13 + date_range + req_c1 + 
                                       req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                                       req_c7 + req_e1,
                                     family = binomial(link = "logit"), weights = weight, data = full_data))
summary(pool(imp_anx_model), conf.int = TRUE, exponentiate = TRUE)

imp_dpr_model <- with(temp_data, glm(formula = depression ~ gender + 
                                       age_range + employment + 
                                       chronic_disease + d1_health_1 + d1_health_2 + 
                                       d1_health_3 + d1_health_4 + d1_health_5 +
                                       d1_health_6 + d1_health_7 + d1_health_8 + 
                                       d1_health_9 + d1_health_10 + d1_health_11 + 
                                       d1_health_12 + d1_health_13 + date_range + req_c1 + 
                                       req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                                       req_c7 + req_e1, family = binomial(link = "logit"), weights = weight, data = full_data))
summary(pool(imp_dpr_model), conf.int = TRUE, exponentiate = TRUE)

#Create time-specific regression models for anxiety
imp_anx_model_Q2 <- with(temp_data, glm(formula = anxiety ~ gender + 
                                       age_range + employment + 
                                       chronic_disease + d1_health_1 + d1_health_2 + 
                                       d1_health_3 + d1_health_4 + d1_health_5 +
                                       d1_health_6 + d1_health_7 + d1_health_8 + 
                                       d1_health_9 + d1_health_10 + d1_health_11 + 
                                       d1_health_12 + d1_health_13  + req_c1 + 
                                       req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                                       req_c7 + req_e1,
                                     family = binomial(link = "logit"), weights = weight, 
                                     subset = date_range == "Q2", data = full_data))
summary(pool(imp_anx_model_Q2), conf.int = TRUE, exponentiate = TRUE)

imp_anx_model_Q3 <- with(temp_data, glm(formula = anxiety ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q3", data = full_data))
summary(pool(imp_anx_model_Q3), conf.int = TRUE, exponentiate = TRUE)

imp_anx_model_Q4 <- with(temp_data, glm(formula = anxiety ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q4", data = full_data))
summary(pool(imp_anx_model_Q4), conf.int = TRUE, exponentiate = TRUE)

imp_anx_model_Q5 <- with(temp_data, glm(formula = anxiety ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q5", data = full_data))
summary(pool(imp_anx_model_Q5), conf.int = TRUE, exponentiate = TRUE)

imp_anx_model_Q6 <- with(temp_data, glm(formula = anxiety ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q6", data = full_data))
summary(pool(imp_anx_model_Q6), conf.int = TRUE, exponentiate = TRUE)

imp_anx_model_Q7 <- with(temp_data, glm(formula = anxiety ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q7", data = full_data))
summary(pool(imp_anx_model_Q7), conf.int = TRUE, exponentiate = TRUE)

imp_anx_model_Q8 <- with(temp_data, glm(formula = anxiety ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q8", data = full_data))
summary(pool(imp_anx_model_Q8), conf.int = TRUE, exponentiate = TRUE)


#Create time-specific regression models for depression
imp_dpr_model_Q2 <- with(temp_data, glm(formula = depression ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q2", data = full_data))
summary(pool(imp_dpr_model_Q2), conf.int = TRUE, exponentiate = TRUE)

imp_dpr_model_Q3 <- with(temp_data, glm(formula = depression ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q3", data = full_data))
summary(pool(imp_dpr_model_Q3), conf.int = TRUE, exponentiate = TRUE)

imp_dpr_model_Q4 <- with(temp_data, glm(formula = depression ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q4", data = full_data))
summary(pool(imp_dpr_model_Q4), conf.int = TRUE, exponentiate = TRUE)

imp_dpr_model_Q5 <- with(temp_data, glm(formula = depression ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q5", data = full_data))
summary(pool(imp_dpr_model_Q5), conf.int = TRUE, exponentiate = TRUE)

imp_dpr_model_Q6 <- with(temp_data, glm(formula = depression ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q6", data = full_data))
summary(pool(imp_dpr_model_Q6), conf.int = TRUE, exponentiate = TRUE)

imp_dpr_model_Q7 <- with(temp_data, glm(formula = depression ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q7", data = full_data))
summary(pool(imp_dpr_model_Q7), conf.int = TRUE, exponentiate = TRUE)

imp_dpr_model_Q8 <- with(temp_data, glm(formula = depression ~ gender + 
                                          age_range + employment + 
                                          chronic_disease + d1_health_1 + d1_health_2 + 
                                          d1_health_3 + d1_health_4 + d1_health_5 +
                                          d1_health_6 + d1_health_7 + d1_health_8 + 
                                          d1_health_9 + d1_health_10 + d1_health_11 + 
                                          d1_health_12 + d1_health_13  + req_c1 + 
                                          req_c2 + req_c3 + req_c4 + req_c5 + req_c6 + 
                                          req_c7 + req_e1,
                                        family = binomial(link = "logit"), weights = weight, 
                                        subset = date_range == "Q8", data = full_data))
summary(pool(imp_dpr_model_Q8), conf.int = TRUE, exponentiate = TRUE)



