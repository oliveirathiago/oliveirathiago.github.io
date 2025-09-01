library(tidyverse)
library(haven)
library(rbw)
library(texreg)
library(survey)
library(brglm2)
library(sandwich)
library(lmtest)
library(showtext)
font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

load("data/export/cc_tract_2000.RData")
load("data/import/phdcn_neighborhoods/policing/data/derived/trr_tract_2000.RData")
demographics <- read_csv("data/import/phdcn_neighborhoods/demographics/data/export/ltdb_factors_2000.csv")
crimes <- read_csv("data/import/phdcn_neighborhoods/crime/data/export/tract/cpd_crimes_tract_2000_year.csv")
load("data/import/phdcn_neighborhoods/boundaries/data/derived/chicago_tract_2000.RData")



load("data/import/PHDCN/age_tract_index.RData")
phdcn_wgt <- read_stata("data/import/PHDCN/PHDCN_final_wgt_121521.dta")
phdcn <- read_stata("data/import/PHDCN/PHDCN5+.dta")
phdcn_demo <- read_stata("data/import/PHDCN/masterfile_allvariables_noimputations_final_sp15.dta")
phdcn_cbcl_neighb <- readRDS("../Enduring legacy/manuscript_legal_cynicism/manuscript/files/data/chicago_studyw5.RDS") %>%
  dplyr::select(subid, cbcl_internalizing, cbcl_externalizing, starts_with("average_"))

phdcn <- 
  phdcn %>%
  filter(!is.na(co_date))

c0 <-
  phdcn %>%
  filter(cohorts == 0)

age_tract <-
  age_tract_index %>%
  filter(year >= 2012 & year <= 2015) %>%
  filter(subid %in% phdcn$subid) %>%
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
                     #black = sp_ethn == 3,
                     #white = sp_ethn == 4,
                     #latino = sp_ethn == 0,
                     hs_higher = case_when(
                       pc_educ1 >= 3 ~ T,
                       TRUE ~ FALSE),
                     immigrant = case_when(
                       spimgen == 1 ~ 1,
                       spimgen == 2 ~ 1,
                       TRUE ~ 0
                     )) %>%
              dplyr::select(subid, COHORT = cohort, Sp_Male_Corrected, hs_higher, immigrant)) %>%
  left_join(phdcn_cbcl_neighb)

age_tract_nomissin <-
  age_tract %>%
  drop_na()

age_tract_long <- age_tract_nomissin %>%
  pivot_longer(cols = cc_2012:violent_2015,
               names_to = c("type", "year"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(id_cols = c(subid:average_stability, year),
              values_from = value,
              names_from = c("type")) %>%
  mutate(year = as.numeric(year),
         subid = as_factor(subid))

data_rbw <-
  age_tract_long %>%
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

age_tract_wide <-
  age_tract_long %>%
  group_by(subid) %>%
  summarise(sqf = sum(cc),
            trr = sum(trrs)) %>%
  left_join(age_tract_nomissin %>% dplyr::select(subid, Sp_Male_Corrected:average_stability)) %>%
  left_join(phdcn %>% mutate(subid = as_factor(subid))) %>%
  mutate(ethnicity = case_when(
    QD2_C1 == 1 ~ "White",
    QD2_C2 == 1 ~ "Black",
    QD1_C1 == 0 ~ "Hispanic and others",
    TRUE ~ "Hispanic and others"
  )) %>%
  mutate(ethnicity = fct_relevel(ethnicity, "White", "Black", "Hispanic and others")) %>%
  mutate(white = ethnicity == "White", 
         black = ethnicity == "Black",
         latino = ethnicity == "Hispanic and others") %>%
  filter(Q57 != 98) %>%
  mutate(arrest = Q41 == 1,
         jail = Q54 == 1,
         jail.family = Q55 == 1,
         political = Q8 == 1,
         gun.carry = case_when(
           Q31 == 0 ~ F,
           Q31 == 0 & Q31A == 4 ~ F,
           Q31 == 1 & Q31A != 4 ~ T
         ),
         gun.home = Q32 == 1,
         police.violence.family = Q44 == 1,
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

rbw_trr_design <- svydesign(ids = ~ 1, weights = ~ rbw, data = age_tract_wide)


m.policekilling <- svyglm(Q45 ~ (black + latino) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                   , method = "brglmFit"
                   , design = rbw_trr_design)
coeftest(m.policekilling, vcov = sandwich)

m.police.force.family <- svyglm(police.violence.family ~ (black + latino) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                                   , method = "brglmFit"
                                   , design = rbw_trr_design)
coeftest(m.police.force.family, vcov = sandwich)

m.police.violence <- svyglm(Q42 ~ (black + latino) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                                   , method = "brglmFit"
                                   , design = rbw_trr_design)
coeftest(m.police.violence, vcov = sandwich)

m.police.force <- svyglm(Q43 ~ (black + latino) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                            , method = "brglmFit"
                            #, family = "poisson"
                            , design = rbw_trr_design)
coeftest(m.police.force, vcov = sandwich)

m.police.abuse <- svyglm(Q40 ~ (black + latino) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                         , method = "brglmFit"
                         , design = rbw_trr_design)
coeftest(m.police.abuse, vcov = sandwich)


##########
## effects among blacks
m.police.abuse_black <- svyglm(Q40 ~ (white + latino) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                         , method = "brglmFit"
                         , design = rbw_trr_design)
m.police.force_black <- svyglm(Q43 ~ (white + latino) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                         , method = "brglmFit"
                         , design = rbw_trr_design)
m.policekilling_black <- svyglm(Q45 ~ (white + latino) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                          , method = "brglmFit"
                          , design = rbw_trr_design)

## effects among latinos
m.police.abuse_latino <- svyglm(Q40 ~ (white + black) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                               , method = "brglmFit"
                               , design = rbw_trr_design)
m.police.force_latino <- svyglm(Q43 ~ (white + black) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                               , method = "brglmFit"
                               , design = rbw_trr_design)
m.policekilling_latino <- svyglm(Q45 ~ (white + black) * log(trr+1) + factor(cohorts) + Sp_Male_Corrected + hs_higher + immigrant + cbcl_internalizing + cbcl_externalizing
                                , method = "brglmFit"
                                , design = rbw_trr_design)

dataplot <-
  broom::tidy(coeftest(m.police.abuse, vcov = sandwich)) %>%
  filter(term == "log(trr + 1)") %>%
  mutate(outcome = "Police abuse",
         effect = "white") %>%
  bind_rows(
    broom::tidy(coeftest(m.police.abuse_black, vcov = sandwich)) %>%
      filter(term == "log(trr + 1)") %>%
      mutate(outcome = "Police abuse",
             effect = "black")
  ) %>%
  bind_rows(
    broom::tidy(coeftest(m.police.abuse_latino, vcov = sandwich)) %>%
      filter(term == "log(trr + 1)") %>%
      mutate(outcome = "Police abuse",
             effect = "latino")
  ) %>%
  bind_rows(
    broom::tidy(coeftest(m.police.force, vcov = sandwich)) %>%
      filter(term == "log(trr + 1)") %>%
      mutate(outcome = "Police use-of-force",
             effect = "white")
  ) %>%
  bind_rows(
    broom::tidy(coeftest(m.police.force_black, vcov = sandwich)) %>%
      filter(term == "log(trr + 1)") %>%
      mutate(outcome = "Police use-of-force",
             effect = "black")
  ) %>%
  bind_rows(
    broom::tidy(coeftest(m.police.force_latino, vcov = sandwich)) %>%
      filter(term == "log(trr + 1)") %>%
      mutate(outcome = "Police use-of-force",
             effect = "latino")
  ) %>%
  bind_rows(
    broom::tidy(coeftest(m.policekilling, vcov = sandwich)) %>%
      filter(term == "log(trr + 1)") %>%
      mutate(outcome = "Police killing (vicarious)",
             effect = "white")
  ) %>%
  bind_rows(
    broom::tidy(coeftest(m.policekilling_black, vcov = sandwich)) %>%
      filter(term == "log(trr + 1)") %>%
      mutate(outcome = "Police killing (vicarious)",
             effect = "black")
  ) %>%
  bind_rows(
    broom::tidy(coeftest(m.policekilling_latino, vcov = sandwich)) %>%
      filter(term == "log(trr + 1)") %>%
      mutate(outcome = "Police killing (vicarious)",
             effect = "latino")
  ) %>%
  mutate(significance = case_when(p.value < 0.05 ~ "significant", TRUE ~ "non significant"),
         ci.low = estimate - 1.96 * std.error,
         ci.upp = estimate + 1.96 * std.error,
         effect = factor(effect, levels = c("latino", "black", "white")),
         outcome = factor(outcome, levels = c("Police use-of-force", "Police abuse", "Police killing (vicarious)")))

plot.consequences <- 
  ggplot(dataplot, aes(y = estimate, x = term, group = effect, colour = effect)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp, alpha = significance), width = .25, position = position_dodge(width = .4), lwd = .5, show.legend = T) + 
  geom_point(aes(shape = significance, group = effect, alpha = significance), 
             size = 3, 
             position = position_dodge(width = .4)) +
  scale_alpha_manual(values = c(.5, 1)) +
  scale_shape_manual(values = c(NA, 8), labels = c("", "95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  facet_grid(~ outcome) +
  coord_flip() + 
  #ggtitle("Effects of neighborhood exposure to police use-of-force\n(2012--2015) on police victimization (2021)") + 
  ylab("") + xlab("") +
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         shape = guide_legend(title = "",
                              override.aes = list(linetype = c(0,1),
                                                  shape = c(NA,8),
                                                  size = c(0,3)))) +
  theme(text = element_text(family = "ebgaramond"),
        plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12),
        axis.text.y = element_text(colour = "#3C3C3C", size = 12),
        axis.text.x = element_text(colour = "#3C3C3C", size = 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        strip.text.x = element_text(size = 12),
        #panel.spacing.x=unit(1, "lines"),
        #panel.spacing.y=unit(1, "lines"),
        #plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        #plot.margin = margin(.5, 0, .5, 0, "cm"),
        plot.background  = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = "#3C3C3C", linewidth = 0.3),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key        = element_rect(fill = NA, colour = NA)) + 
  scale_x_discrete(labels = c('Neighborhood\nexposure to\npolice use of force')) + 
  scale_colour_brewer(palette = "Set1", 
                      breaks = c('white', 'black', 'latino'),
                      labels = c('White', 'Black', 'Latino')) + 
  theme(aspect.ratio = 1)
  
png("plots/consequences.png", width = 8, height = 6, units = "in", res = 300)
plot.consequences
dev.off()

save(dataplot, file = "data/export/dataplot_consequences.RData")
ggsave("esc/presentation_files/plot_vict.svg", plot = plot.consequences, bg = "transparent", width = 10, height = 6, units = "in")


