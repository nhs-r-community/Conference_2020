##################################################################
### The purpose of this script is to describe the process of using
### R to analyse the data of data of Children presenting to Great
### Ormond Street Hospital with COVID-19 and a new inflammatory
### inflammatory syndrome known as PIMS-TS. Also included are a 
### group of COVID-19 patients as a control sample for ITU.
##################################################################

## Load libraries
library(rstatix)
library(tidyverse)
library(lubridate)
library(magrittr)
library(lme4)
library(cowplot)
library(PupillometryR)
library(plotrix)
library(ggsignif)
library(ggpubr)
library(lm.beta)
library(mediation)

## Define datasets
# N.B. because of data protection, no data can be uploaded so the
# below are descriptions of the data sets used

demographics # hospital number, dates of birth and death, age, gender and ethnicity.
# 57 patients in total - 32 COVID-19 positive, 25 COVID-19 negative

labs # laboratory data of all patients. Dim = 101,582 x 29

admissions # admissions recorded for the 57 patients
# data includes start and end date

ward_stays # length of stays for both ward and intensive care unit (ITU)
# data includes binary variable is_itu to indicate type of ward as well
# as start and end datetimes for all ward stays

severity # clinical data on the admission risk of death score (pims_3),
# as well as maximal rates of vasoactive drugs

############

# Question 1. Do we see the same over-representation of BAME as in adults?

demographics %>%
  # group patients into "white" or "BAME" category
  mutate(ethnic = ifelse(str_detect(ethnicity, "White"), "White", "BAME")) %>%
  group_by(covid_19_status, ethnic) %>%
  count() %>%
  # create a contingency table
  xtabs(n ~ covid_19_status, ethnic, data.) %>%
  prop_test(detailed = TRUE)  

# Answer 1. Although the majority of patients seen in all groups were
# BAME, there was no significant difference in the proportions in 
# each group. p = 0.64

############

# Question 2. Are there differences in inflammatory markers and 
# other parameters in patients with COVID-19?
# Lab results will be largely patient dependent to use a mixed
# model type approach

# split the lab results by the different elements
by_component <- labs %>% split(.$test_name)

# define models to determine the difference between patient level and
# diagnosis level effects
full_model <- by_component %>% map(~lmer(lab_value ~ factor(covid_19_status) + (1|hospital_no), REML=FALSE, data = .x, na.rm = T))
null_model <- by_component %>% map(~lmer(lab_value ~ (1|hospital_no), REML=FALSE, data = .x, na.rm = T))

# perform anova on the models across lists
anova_list <- mapply(anova, null_mods, full_mods, SIMPLIFY = FALSE)

# Answer 2. Hospitalised COVID-19 positive patients are significantly
# lower than patients who are COVID-19 negative as well as signicantly
# increased inflammatory markers including C-reactive protein, D Dimers
# and fibrinogen 

### Plot 1 Variation in vitamin D levels by COVID-19 status

labs %>%
  filter(test_name == "Vitamin D (Total)") %>%
  # make x axis numeric to allow offsetting of data
  mutate(covid_19_status = factor(covid_19_status, levels = c("non_covid", "covid"), labels = c(1,2))) %>%
  ggplot(aes(covid_19_status, lab_value, fill = covid_19_status)) +
  # create the density plot positively offset from the timepoint
  geom_flat_violin(aes(fill = covid_19_status), 
                   position = position_nudge(x = .1, y = 0), 
                   adjust = 1.0, trim = T, alpha = 0.5, colour = NA) +
  # place the individual patient points negatively offset from the timepoint
  geom_point(aes(as.numeric(covid_19_status), y = lab_value), colour = "black", shape = 20) +
  geom_boxplot(aes(fill = covid_19_status), outlier.shape = NA, alpha = 0.5, width = 0.1, colour = "black") +
  # annotate with the appropriate significance levels
  geom_signif(comparisons = list(c(1, 2)), step_increase = 0.1, annotations = c("p <0.001")) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "",
       y = "Total Vitamin D (nmol/L)") +
  # convert numeric x axis labels back to COVID-19 status
  scale_x_discrete(labels = c("1" = "Covid-19 Negative", "2" = "Covid-19 Positive")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############

# Question 3. Is vitamin D level determined by disease severity in COVID-19 patients
# when adjusted by patients age, gender and ethnicity? 
# Use multivarible linear regression

# Step one, isolate the maximal value of C-reactive protein by patient

max_crp <- labs %>%
  filter(test_name == "c-reactive_protein") %>%
  group_by(hospital_no) %>%
  slice(which.max(lab_value)) %>%
  mutate(crp_numeric = lab_value) %>%
  select(hospital_no, test_name, crp_numeric)

# Integrate the datasets into a single dataset

severe_query <- labs %>%
  # vitamin D measured on arrival to hospital
  filter(test_name == "Vitamin D (Total)") %>%
  left_join(demographics, by = "hospital_no") %>%
  # looking at only COVID-19 positive patients
  filter(covid_19_status == "positive") %>%
  left_join(severity, by = "hospital_no") %>%
  left_join(max_crp, by = "hospital_no") %>%
  mutate(ethnic = ifelse(str_detect(ethnicity, "White"), "White", "BAME"))
  
vit_d_model<-  lm(lab_value ~ age + sex + pims_3 + crp_numeric + ethnic, data = vit_d) 

# Answer 3. Vitamin D level does not appear to be significantly linked to
# disease severity with only age as a significant predictor of vitamin D level
# Model explained 65% of the variance.

# Question 3.b If vitamin D is associated with inflammation and COVID-19 is
# also associated with inflammation what is the direct and indirect effect
# of vitamin D on inflammation

vit_d<- labs %>%
  filter(test_name == "Vitamin D (Total)") %>%
  left_join(demographics, by = "hospital_no") %>%
  left_join(severity, by = "hospital_no") %>%
  left_join(max_crp, by = "hospital_no") %>%
  mutate(ethnic = factor(ethnic))

fit.totaleffect <- lm(lab_value ~ covid_19_status, data = vit_d)
fit.mediator <- lm(covid_19_status ~ lab_value, data = vit_d)
fit.dv <- lm(crp_numeric ~ lab_value + covid_19_status, data = vit_d)
results = mediate(fit.mediator, fit.dv, treat="covid_19_status", mediator="lab_value", boot = T)

# Answer 3.b. The effects vitamin D on inflammation was not mediated by
# COVID-19 status
############
