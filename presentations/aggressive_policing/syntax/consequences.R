library(tidyverse)
library(haven)
library(rbw)
library(texreg)
library(survey)
library(brglm2)

load("data/export/cc_tract_2000.RData")
load("data/import/phdcn_neighborhoods/policing/data/derived/trr_tract_2000.RData")
demographics <- read_csv("data/import/phdcn_neighborhoods/demographics/data/export/ltdb_factors_2000.csv")
crimes <- read_csv("data/import/phdcn_neighborhoods/crime/data/export/tract/cpd_crimes_tract_2000_year.csv")
load("data/import/phdcn_neighborhoods/boundaries/data/derived/chicago_tract_2000.RData")



load("data/import/PHDCN/age_tract_index.RData")
phdcn_wgt <- read_stata("data/import/PHDCN/PHDCN_final_wgt_121521.dta")
phdcn <- read_stata("data/import/PHDCN/PHDCN5+.dta")
phdcn_demo <- read_stata("data/import/PHDCN/masterfile_allvariables_noimputations_final_sp15.dta")

c0 <-
  phdcn %>%
  filter(!is.na(co_date)) %>%
  filter(cohorts == 0)

age_tract_c0 <-
  age_tract_index %>%
  filter(year >= 2012 & year <= 2015) %>%
  filter(subid %in% c0$subid) %>%
  rename(tract_2000 = tract) %>%
  left_join(cc_tract_2000) %>%
  left_join(trr_tract_2000) %>%
  left_join(crimes %>%
              mutate(tract_2000 = as_factor(tract_2000),
                     violent = agg_assault + homicide + robbery) %>%
              dplyr::select(tract_2000, year, violent)
  ) %>%
  pivot_wider(id_cols = c(subid),
              names_from = year,
              values_from = c(cc, trrs, violent)) %>%
  left_join(phdcn_demo %>%
              mutate(subid = factor(subid),
                     black = sp_ethn == 3,
                     white = sp_ethn == 4,
                     latino = sp_ethn == 0,
                     hs_higher = case_when(
                       pc_educ1 >= 3 ~ T,
                       TRUE ~ FALSE),
                     immigrant = case_when(
                       spimgen == 1 ~ 1,
                       spimgen == 2 ~ 1,
                       TRUE ~ 0
                     )) %>%
              dplyr::select(subid, COHORT = cohort, Sp_Male_Corrected, white, black, latino, hs_higher, immigrant))

### only 68 out 217 respondents were in Chicago between 2012 and 2015

library(mice)
age_tract_c0_with_missing <- age_tract_c0
age_tract_c0_imputed <- mice(age_tract_c0, method = "rf", m = 5)

completed_datasets <- complete(age_tract_c0_imputed, action = "long", include = TRUE)

# Replace missing values in original dataset with imputed values
for (i in unique(completed_datasets$.imp)) {
  imputed_values <- completed_datasets[completed_datasets$.imp == i, ]
  
  # Loop through each variable (excluding .imp and .id columns)
  for (var in names(imputed_values)) {
    if (!grepl("^.imp|^\\.id$", var)) {  # Exclude .imp and .id columns
      # Replace missing values in the original dataset with imputed values
      age_tract_c0[is.na(age_tract_c0[, var]), var] <- imputed_values[is.na(age_tract_c0[, var]), var]
    }
  }
}

age_tract_c0_long <- age_tract_c0 %>%
  pivot_longer(cols = cc_2012:violent_2015,
               names_to = c("type", "year"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(id_cols = c(subid:immigrant, year),
              values_from = value,
              names_from = c("type")) %>%
  mutate(year = as.numeric(year),
         subid = as_factor(subid))
  
data_rbw <-
  age_tract_c0_long %>%
  dplyr::select(subid, year, cc, trrs, violent) %>%
  arrange(subid, year) %>%   # make sure data is sorted correctly
  group_by(subid) %>%
  mutate(
    sqf.lag1 = replace_na(lag(cc, n = 1), 1),
    trr.lag1 = replace_na(lag(trrs, n = 1), 1),
    violent.lag1 = replace_na(lag(violent, n = 1), 1)
  ) %>%
  ungroup()

m.wgt_trr <- MASS::glm.nb(violent ~ log(trr.lag1 + 1) * factor(year) + log(violent.lag1 + 1) * factor(year) + log(sqf.lag1) * factor(year), data = data_rbw)

rbw_trr <- rbwPanel(
  treatment = trrs,
  xmodels = list(m.wgt_trr),
  id = subid,
  time = year,
  data = data_rbw,
  max_iter = 10000
)

age_tract_c0_wide <-
  age_tract_c0_long %>%
  group_by(subid) %>%
  summarise(sqf = sum(cc),
            trr = sum(trrs)) %>%
  left_join(age_tract_c0 %>% dplyr::select(subid, Sp_Male_Corrected:immigrant)) %>%
  left_join(phdcn %>% mutate(subid = as_factor(subid))) %>%
  filter(Q57 != 98) %>%
  mutate(arrest = Q41 == 1,
         jail = Q54 == 1,
         jail.family = Q55 == 1,
         attended.college = case_when(
    Q57 >= 4 ~ T, T ~ F),
    graduated.college = case_when(
      Q57 >= 8 ~ T, T ~ F),
    ) %>%
  mutate(across(c(Q35A:Q36E), ~case_when(. >5 ~ NA_real_, TRUE ~ .))) %>%
  # treating survey indicators as continuous
  mutate(across(c(Q35A:Q36E), as.numeric)) %>%
  mutate(across(c(Q36A:Q36E), ~case_when(
    . == 1 ~ 5,
    . == 2 ~ 4,
    . == 3 ~ 3,
    . == 4 ~ 2,
    . == 5 ~ 1,
    TRUE ~ NA_real_
  ))) %>%
  left_join(rbw_trr$weights %>% rename(subid = id))

library(lavaan)
cfa_2factor<-
  cfa(
    "police.cyn21_2f =~ Q36A + Q36B + Q36C
     moral.cyn21_2f =~ Q35A + Q35B + Q35C + Q35D
     
     police.cyn21_2f ~~ moral.cyn21_2f
    "
    , std.lv = T, 
    data = age_tract_c0_wide,  ordered = c('Q35A', 'Q35B', 'Q35C', 'Q35D', 'Q36A', 'Q36B', 'Q36C'), estimator = "PML", missing = "available.cases"
    #estimator = "MLR", missing = "ML"
  )

age_tract_c0_wide <-
  age_tract_c0_wide %>%
  mutate(police.cyn = lavPredict(cfa_2factor)[, "police.cyn21_2f"],
         moral.cyn = lavPredict(cfa_2factor)[, "moral.cyn21_2f"])

rbw_trr_design <- svydesign(ids = ~ 1, weights = ~ rbw, data = age_tract_c0_wide)

m.arrest <- svyglm(police.cyn ~ log(sqf)
                   #, family = "poisson"
                   #, family = binomial(link = 'logit')
                   , method = "brglmFit", 
                   , design = rbw_trr_design)
screenreg(m.arrest)

m.attended_sqf <- svyglm(attended.college ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + white + latino
                         , family = "poisson"
                         #, family = binomial(link = 'logit')
                         , method = "brglmFit", 
                         , design = rbw_trr_design)

m.attended_trr <- svyglm(attended.college ~ log(trr + 1) + Sp_Male_Corrected + hs_higher + immigrant + white + latino
                         , family = "poisson"
                         #, family = binomial(link = 'logit')
                         , method = "brglmFit", 
                         , design = rbw_trr_design)

m.attended_trr <- svyolr(factor(Q57) ~ log(trr + 1) + Sp_Male_Corrected + hs_higher + immigrant + white + latino
                         #, family = "poisson"
                         #, family = binomial(link = 'logit')
                         #, method = "brglmFit", 
                         , design = rbw_trr_design)

m.attended_sqf <- svyolr(factor(Q57) ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + white + latino
                         #, family = "poisson"
                         #, family = binomial(link = 'logit')
                         #, method = "brglmFit", 
                         , design = rbw_trr_design)

list(m.attended_sqf, m.attended_trr) %>% screenreg()

m.graduated_trr <- svyglm(graduated.college ~ log(trr + 1) + Sp_Male_Corrected + hs_higher + immigrant + police_abuse_ever + white #+ latino
                          , family = "poisson"
                          #, family = binomial(link = 'logit')
                          , method = "brglmFit", 
                          , design = rbw_trr_design)



age_tract_c0_new <-
  age_tract_index %>%
  filter(year >= 2012 & year <= 2015) %>%
  filter(subid %in% c0$subid) %>%
  rename(tract_2000 = tract) %>%
  left_join(cc_tract_2000) %>%
  left_join(trr_tract_2000) %>%
  mutate(chicago = case_when(
                    is.na(cc) ~ F,
                    TRUE ~ TRUE),
        cc = case_when(
                is.na(cc) ~ 0,
                TRUE ~ cc),
          trrs = case_when(
                  is.na(trrs) ~ 0,
                  TRUE ~ trrs)
    ) %>%
  group_by(subid) %>%
  summarise(sqf = sum(cc),
            trr = sum(trrs),
            chicago = mean(chicago))

age_sqf_trr <-
  age_tract_index %>%
  rename(tract_2000 = tract) %>%
  filter(year >= 2012 & year <= 2015) %>%
  left_join(cc_tract_2000) %>%
  left_join(trr_tract_2000)

age_sqf_trr_grouped <-
  age_sqf_trr %>%
  mutate(subid = as_factor(subid)) %>%
  left_join(phdcn %>% dplyr::select(subid, cohorts, co_date) %>% mutate(subid = as_factor(subid))) %>%
  filter(cohorts == 0) %>%
  filter(!is.na(co_date)) %>%
  #filter(age < 25) %>%
  filter(tract_2000 %in% chicago_tract_2000$tract) %>%
  group_by(subid) %>%
  summarise(sqf = sum(cc, na.rm = T),
            trr = sum(trrs, na.rm = T))

data_rbw <-
  age_tract_index %>%
  filter(year >= 2012 & year <= 2015) %>%
  filter(subid %in% c0$subid) %>%
  rename(tract_2000 = tract) %>%
  left_join(cc_tract_2000) %>%
  left_join(trr_tract_2000) %>%
  left_join(crimes %>%
              mutate(tract_2000 = as_factor(tract_2000),
                     violent = agg_assault + homicide + robbery) %>%
              dplyr::select(tract_2000, year, violent)
  ) %>%
  dplyr::select(subid, year, cc, trrs, violent) %>%
  mutate(chicago = case_when(
    is.na(cc) ~ F,
    TRUE ~ TRUE),,
    cc = case_when(
    is.na(cc) ~ 0,
    TRUE ~ cc),
    trrs = case_when(
      is.na(trrs) ~ 0,
      TRUE ~ trrs),
    violent = case_when(
      is.na(violent) ~ 0,
      TRUE ~ violent)
  ) %>%
  arrange(subid, year) %>%   # make sure data is sorted correctly
  group_by(subid) %>%
  mutate(
    sqf.lag1 = replace_na(lag(cc, n = 1), 1),
    trr.lag1 = replace_na(lag(trrs, n = 1), 1),
    violent.lag1 = replace_na(lag(violent, n = 1), 1)
  ) %>%
  ungroup()


data_rbw <-
  age_tract_c0_new %>%
  mutate(subid = as_factor(subid)) %>%
  left_join(phdcn %>% dplyr::select(subid, cohorts, co_date) %>% mutate(subid = as_factor(subid))) %>%
  filter(cohorts == 0) %>%
  filter(tract_2000 %in% chicago_tract_2000$tract) %>%
  left_join(crimes %>%
              mutate(tract_2000 = as_factor(tract_2000),
                     violent = agg_assault + homicide + robbery) %>%
              dplyr::select(tract_2000, year, violent)
            ) %>%
  dplyr::select(subid, year, cc, trrs, violent) %>%
  arrange(subid, year) %>%   # make sure data is sorted correctly
  group_by(subid) %>%
  mutate(
    sqf.lag1 = replace_na(lag(cc, n = 1), 1),
    trr.lag1 = replace_na(lag(trrs, n = 1), 1),
    violent.lag1 = replace_na(lag(violent, n = 1), 1)
  ) %>%
  ungroup() %>%
  mutate(log.sqf = log(cc),
         log.trr = log(trrs + 1))

m.wgt.sqf <- MASS::glm.nb(violent ~ log(sqf.lag1 + 1) * factor(year) + log(violent.lag1 + 1) * factor(year), data = data_rbw)
m.wgt.trr_violent <- MASS::glm.nb(violent ~ log(trr.lag1 + 1) * factor(year) + log(violent.lag1 + 1) * factor(year), data = data_rbw)
m.wgt.trr_sqf <- MASS::glm.nb(cc ~ log(trr.lag1 + 1) * factor(year) + log(sqf.lag1 + 1) * factor(year), data = data_rbw)

rbw_sqf <- rbwPanel(
  treatment = cc,
  xmodels = list(m.wgt.sqf),
  id = subid,
  time = year,
  data = data_rbw,
  max_iter = 10000
)

rbw_trr <- rbwPanel(
  treatment = trrs,
  xmodels = list(m.wgt.trr_violent),
  id = subid,
  time = year,
  data = data_rbw,
  max_iter = 10000
)

analytic_sample <-
  age_tract_c0_new %>%
  mutate(subid = as_factor(subid)) %>%
  left_join(phdcn %>% mutate(subid = as_factor(subid))) %>%
  mutate(education = case_when(
    Q57 == 98 ~ NA_real_,
    TRUE ~ Q57
  )) %>%
  mutate(attended.college = case_when(
    Q57 >= 6 ~ T, T ~ F),
    graduated.college = case_when(
      Q57 >= 8 ~ T, T ~ F)) %>%
  left_join(phdcn_demo %>%
              mutate(subid = factor(subid),
                     black = sp_ethn == 3,
                     white = sp_ethn == 4,
                     latino = sp_ethn == 0,
                     hs_higher = case_when(
                       pc_educ1 >= 3 ~ T,
                       TRUE ~ FALSE),
                     immigrant = case_when(
                       spimgen == 1 ~ 1,
                       spimgen == 2 ~ 1,
                       TRUE ~ 0
                     )) %>%
              dplyr::select(subid, COHORT = cohort, tractw1, Sp_Male_Corrected, white, black, latino, sp_fborn, hs_higher, immigrant)) %>%
  left_join(rbw_trr$weights %>% rename(subid = id))

rbw_trr_design <- svydesign(ids = ~ 1, weights = ~ rbw, data = analytic_sample)


m.attended_trr <- glm(attended.college ~ log(trr + 1) + Sp_Male_Corrected + hs_higher + immigrant + white #+ latino
                         , family = "poisson"
                         #, family = binomial(link = 'logit')
                         , method = "brglmFit", 
                         #, design = rbw_trr_design
                         , data = analytic_sample
                      )

m.graduated_trr <- svyglm(graduated.college ~ log(trr + 1) + Sp_Male_Corrected + hs_higher + immigrant + police_abuse_ever + white #+ latino
                          , family = "poisson"
                          #, family = binomial(link = 'logit')
                          , method = "brglmFit", 
                          , design = rbw_trr_design)

### non-integer #successes in a binomial glm!

m.attended_sqf <- svyglm(attended.college ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + white #+ latino
                         , family = "poisson"
                         #, family = binomial(link = 'logit')
                         , method = "brglmFit", 
                         , design = rbw_trr_design)

m.graduated_sqf <- svyglm(graduated.college ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + white #+ latino
                          , family = "poisson"
                          #, family = binomial(link = 'logit')
                          , method = "brglmFit", 
                          , design = rbw_trr_design)

list(
  'attended college' = m.attended_trr,
  'attended college' = m.attended_sqf,
  'graduated college' = m.graduated_trr,
  'graduated college' = m.graduated_sqf
) %>% screenreg



analytic_sample <-
  age_sqf_trr_grouped %>% 
  mutate(subid = as_factor(subid)) %>%
  left_join(phdcn %>% mutate(subid = as_factor(subid))) %>%
  mutate(education = case_when(
    Q57 == 98 ~ NA_real_,
    TRUE ~ Q57
  )) %>%
  mutate(attended.college = case_when(
           Q57 >= 6 ~ T, T ~ F),
         graduated.college = case_when(
           Q57 >= 8 ~ T, T ~ F)) %>%
  left_join(phdcn_demo %>%
              mutate(subid = factor(subid),
                     black = sp_ethn == 3,
                     white = sp_ethn == 4,
                     latino = sp_ethn == 0,
                     hs_higher = case_when(
                       pc_educ1 >= 3 ~ T,
                       TRUE ~ FALSE),
                     immigrant = case_when(
                       spimgen == 1 ~ 1,
                       spimgen == 2 ~ 1,
                       TRUE ~ 0
                     )) %>%
              dplyr::select(subid, COHORT = cohort, tractw1, Sp_Male_Corrected, white, black, latino, sp_fborn, hs_higher, immigrant)) %>%
  mutate(ethnicity = case_when(
    QD2_C1 == 1 ~ "White",
    QD2_C2 == 1 ~ "Black",
    QD1_C1 == 0 ~ "Hispanic and others",
    TRUE ~ "Hispanic and others"
  )) %>%
  mutate(ethnicity = fct_relevel(ethnicity, "White", "Black", "Hispanic and others")) %>%
  #filter(ethnicity != "Other") %>%
  mutate(white = ethnicity == "White", 
         black = ethnicity == "Black",
         latino = ethnicity == "Hispanic and others") %>%
  mutate(across(c(Q28, Q29, Q30, Q31, Q32, Q33, Q34), na_if, 98)) %>%
  mutate(across(c(Q28, Q29, Q30, Q31, Q32, Q33, Q34), na_if, 99)) %>%
  mutate(gun.use.25 = case_when(Q33A >= 25 ~ 1, TRUE ~ 0),
         gun.shot.25 = case_when(Q34A >= 25 ~ 1, TRUE ~ 0),
         gun.use_lastfive = case_when(
           cohorts == 0 & Q33A >= 21 ~ 1,
           cohorts == 9 & Q33A >= 30 ~ 1,
           cohorts == 12 & Q33A >= 33 ~ 1,
           cohorts == 15 & Q33A >= 36 ~ 1,
           TRUE ~ 0
         ),
         gun.shot_lastfive = case_when(
           cohorts == 0 & Q34A >= 21 ~ 1,
           cohorts == 9 & Q34A >= 30 ~ 1,
           cohorts == 12 & Q34A >= 33 ~ 1,
           cohorts == 15 & Q34A >= 36 ~ 1,
           TRUE ~ 0
         )) %>%
  mutate(
    chicago = case_when(
      p_city == "CHICAGO" ~ 1,
      p_city == "Chicago" ~ 1,
      p_city == "CHICAGO RIDGE" ~ 1,
      p_city == "EAST CHICAGO" ~ 1,
      TRUE ~ 0
    ),
    arrest_ever = case_when(
      Q41 == 1 ~ 1,
      TRUE ~ 0
    ),
    arrest_adult = case_when(
      Q41B >= 18 ~ 1,
      TRUE ~ 0
    ),
    arrest_older20 = case_when(
      Q41B >= 20 ~ 1,
      TRUE ~ 0
    ),
    crime_sum = Q28 + Q29 + Q30 + gun.use.25 + gun.shot.25,
    arrest = Q54 == 1,
    violent_sum = Q30 + Q31 + Q32 + Q33 + Q34,
    crime_recent_sum = Q28 + Q29 + Q30 + gun.use_lastfive + gun.shot_lastfive,
    crime_recent_either = case_when(
      Q28 == 1 ~ T, Q29 == 1 ~ T, Q30 == 1 ~ T, gun.use_lastfive == 1 ~ T, gun.shot_lastfive == 1 ~ T, T ~ F
    ),
    Q33A = na_if(Q33A, 998)) %>%
  mutate(gun_carry.recent = case_when(Q31A < 4 ~ TRUE, TRUE ~ FALSE),
         gun_use.recent = case_when(Q33A > 19 ~ TRUE, TRUE ~ FALSE),
         gun_shot.recent = case_when(Q34A > 19 ~ TRUE, TRUE ~ FALSE)) %>%
  #mutate(across(c(gun_carry.recent, gun_shot.recent, gun_use.recent), ~as.numeric)) %>%
  mutate(gun_recent = gun_carry.recent + gun_use.recent + gun_shot.recent) %>%
  mutate(across(c(Q40, Q42, Q43, Q44, Q45), ~case_when(
    . > 1 ~ as.numeric(NA),
    TRUE ~ .
  ))) %>%
  mutate(police_abuse = Q40 + Q42 + Q43 + Q44 + Q45,
         police_abuse_recent_1 = case_when(Q40A < 5 ~ TRUE, TRUE ~ FALSE),
         police_abuse_recent_2 = case_when(Q42A < 3 ~ TRUE, TRUE ~ FALSE),
         police_abuse_recent_3 = case_when(Q43A < 3 ~ TRUE, TRUE ~ FALSE),
         police_abuse_recent_4 = case_when(Q44A < 3 ~ TRUE, TRUE ~ FALSE),
         police_abuse_ever_1 = case_when(Q40A == 5 ~ TRUE, TRUE ~ FALSE),
         police_abuse_ever_2 = case_when(Q42A == 3 ~ TRUE, TRUE ~ FALSE),
         police_abuse_ever_3 = case_when(Q43A == 3 ~ TRUE, TRUE ~ FALSE),
         police_abuse_ever_4 = case_when(Q44A == 3 ~ TRUE, TRUE ~ FALSE)
  ) %>%
  mutate(police_abuse_recent = police_abuse_recent_1 + police_abuse_recent_2 + police_abuse_recent_3 + police_abuse_recent_4,
         police_abuse_ever = police_abuse_ever_1 + police_abuse_ever_2 + police_abuse_ever_3 + police_abuse_ever_4) %>%
  left_join(rbw_trr$weights %>% rename(subid = id))

library(brglm2)
library(survey)
rbw_trr_design <- svydesign(ids = ~ 1, weights = ~ rbw, data = analytic_sample)


m.attended_trr <- svyglm(attended.college ~ log(trr + 1) + Sp_Male_Corrected + hs_higher + immigrant + police_abuse_ever + white #+ latino
                    , family = "poisson"
                    #, family = binomial(link = 'logit')
                    , method = "brglmFit", 
                    , design = rbw_trr_design)

m.graduated_trr <- svyglm(graduated.college ~ log(trr + 1) + Sp_Male_Corrected + hs_higher + immigrant + police_abuse_ever + white #+ latino
                     , family = "poisson"
                     #, family = binomial(link = 'logit')
                     , method = "brglmFit", 
                     , design = rbw_trr_design)

### non-integer #successes in a binomial glm!

m.attended_sqf <- svyglm(attended.college ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + white #+ latino
                     , family = "poisson"
                     #, family = binomial(link = 'logit')
                     , method = "brglmFit", 
                     , design = rbw_trr_design)

m.graduated_sqf <- svyglm(graduated.college ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + white #+ latino
                      , family = "poisson"
                      #, family = binomial(link = 'logit')
                      , method = "brglmFit", 
                      , design = rbw_trr_design)

library(texreg)
list(
  'attended college' = m.attended_trr,
  'attended college' = m.attended_sqf,
  'graduated college' = m.graduated_trr,
  'graduated college' = m.graduated_sqf
) %>% screenreg

m.attended_sqf <- svyglm(attended.college ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + white #+ latino
                         , family = "poisson"
                         #, family = binomial(link = 'logit')
                         , method = "brglmFit", 
                         , design = rbw_trr_design)



m.att.sqf <- glm(attended.college ~ log(sqf) + Sp_Male_Corrected + hs_higher + sp_fborn + immigrant + police_abuse_ever
                 , analytic_sample, family = binomial(link = "logit"))
m.att.trr <- svyglm(attended.college ~ log(trr+1) + Sp_Male_Corrected + hs_higher + immigrant + black + latino#+ sp_fborn #+ immigrant
                    , family = "poisson"
                    , design = rbw_trr_design)
m.att.trr <- svyolr(factor(education) ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + black + latino#+ sp_fborn #+ immigrant
                    #, family = "poisson"
                    , design = rbw_trr_design)
m.att.trr <- svyglm(attended.college ~ log(trr + 1) + Sp_Male_Corrected + hs_higher + immigrant + white #+ latino#+ sp_fborn #+ immigrant
                    , family = "poisson"
                    , design = rbw_trr_design)
m.att.trr <- svyglm(Q57 ~ log(sqf) + Sp_Male_Corrected + hs_higher + immigrant + black + latino#+ sp_fborn #+ immigrant
                    , family = "poisson"
                    , design = rbw_trr_design)
m.grad.sqf <- glm(graduated.college ~ log(sqf) + Sp_Male_Corrected + hs_higher + sp_fborn + immigrant + police_abuse_ever
                 , analytic_sample, family = binomial(link = "logit"))
m.grad.trr <- glm(graduated.college ~ log(trr + 1) + Sp_Male_Corrected  + hs_higher + sp_fborn + immigrant + police_abuse_ever
                 , analytic_sample, family = binomial(link = "logit"))

list(m.att.sqf, m.att.trr) %>% screenreg
list(m.grad.sqf, m.grad.trr) %>% screenreg

m.arrest.sqf <- glm(arrest_ever ~ log(sqf) + Sp_Male_Corrected + hs_higher + sp_fborn + immigrant
                  , analytic_sample, family = "poisson")
m.arrest.trr <- glm(arrest_ever ~ log(trr + 1) + Sp_Male_Corrected  + hs_higher + sp_fborn + immigrant
                  , analytic_sample, family = "poisson")
list(m.arrest.sqf, m.arrest.trr) %>% screenreg

m.abuse.sqf <- MASS::glm.nb(police_abuse_ever ~ log(sqf) #+ Sp_Male_Corrected + hs_higher + sp_fborn + immigrant
                    , analytic_sample)
m.abuse.trr <- MASS::glm.nb(police_abuse_ever ~ log(trr + 1) #+ Sp_Male_Corrected  + hs_higher + sp_fborn + immigrant
                    , analytic_sample)
list(m.abuse.sqf, m.abuse.trr) %>% screenreg


glm(attended.college ~ log(sqf), analytic_sample, family = binomial(link = "logit")) %>% summary
glm(attended.college ~ log(trr + 1), analytic_sample, family = binomial(link = "logit")) %>% summary
glm(graduated.college ~ log(sqf), analytic_sample, family = binomial(link = "logit")) %>% summary
glm(graduated.college ~ log(trr + 1), analytic_sample, family = binomial(link = "logit")) %>% summary

analytic_sample <-
  phdcn %>%
  filter(!is.na(co_date)) %>%
  filter(cohorts == 0) %>%
  mutate(subid = as_factor(subid),
         more.than.high.school = case_when(
           Q57 >= 4 ~ T, T ~ F),
         graduated.college = case_when(
           Q57 >= 8 ~ T, T ~ F)
         ) %>%
  left_join(age_sqf_trr_grouped %>% mutate(subid = as_factor(subid))) %>%
  filter(!is.na(sqf))
  