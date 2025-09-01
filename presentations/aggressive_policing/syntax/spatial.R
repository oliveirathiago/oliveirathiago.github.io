library(haven)
library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(MASS)
library(texreg)
options(scipen = 999)
library(showtext)
library(systemfonts)
font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

load("data/export/cc_tract_2000.RData")
load("data/import/phdcn_neighborhoods/policing/data/derived/trr_tract_2000.RData")
demographics <- read_csv("data/import/phdcn_neighborhoods/demographics/data/export/ltdb_factors_2000.csv")
crimes <- read_csv("data/import/phdcn_neighborhoods/crime/data/export/tract/cpd_crimes_tract_2000_year.csv")
load("data/import/phdcn_neighborhoods/boundaries/data/derived/chicago_tract_2000.RData")

chicago <- 
  cc_tract_2000 %>%
  left_join(trr_tract_2000) %>%
  group_by(tract_2000) %>%
  summarise(sqf = sum(cc),
            trr = sum(trrs)) %>%
  mutate(prop_trr = trr / sqf) %>%
  left_join(crimes %>%
              mutate(tract_2000 = as_factor(tract_2000),
                     violent = agg_assault + homicide + robbery) %>%
              dplyr::select(tract_2000, year, violent) %>%
              filter(year >= 2007 & year <= 2010) %>%
              group_by(tract_2000) %>%
              summarise(crime = sum(violent))
  ) %>%
  left_join(demographics %>% filter(year == 2010)) %>% dplyr::select(-year) %>%
  mutate(pop_density = population / area)

# spatial data set (by NCs)
chicago.geo <-
  chicago_tract_2000 %>%
  dplyr::select(tract_2000 = tract) %>%
  left_join(chicago)

listw <- chicago.geo %>% st_make_valid() %>% poly2nb() %>% nb2listw(style = "W", zero.policy = T)

chicago.geo <- chicago.geo %>%
  mutate(across(c(sqf, crime), 
                   ~ lag.listw(listw, .), .names = "splag_{.col}"))

save(chicago.geo, file = "data/export/chicago.geo.RData")

chicago_sqf <-
  ggplot(chicago.geo) +
  geom_sf(aes(fill = sqf), show.legend = F) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "sqrt",
    direction = -1,
    breaks = pretty(chicago.geo$sqf),
    labels = scales::comma
  ) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = NA, colour = NA),
    panel.background = element_rect(fill = NA, colour = NA)
  )

ggsave("esc/presentation_files/sqf_map.svg", plot = chicago_sqf, bg = "transparent")

# SQF models
sqf.spatial <- lagsarlm(log(sqf) ~ FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density), data = chicago.geo, type = "SLX", listw = listw)
sqf.poisson <- glm(sqf ~ FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density) + splag_crime, data = chicago.geo, family = "poisson")
sqf.nb <- glm.nb(sqf ~ FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density) + splag_crime, data = chicago.geo)

# SQF crime models
sqf.spatial_crime <- lagsarlm(log(sqf) ~ log(crime) + FAC_disadv + FAC_hispimm + FAC_stability, data = chicago.geo, type = "SLX", listw = listw)
sqf.poisson_crime <- glm(sqf ~ log(crime) + FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density) + splag_crime, data = chicago.geo, family = "poisson")
sqf.nb_crime <- glm.nb(sqf ~ log(crime) + FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density) + splag_crime, data = chicago.geo)

# TRR models
trr.spatial <- lagsarlm(sqrt(trr) ~ log(sqf) + log(crime) + FAC_disadv + FAC_hispimm + FAC_stability, data = chicago.geo, type = "SLX", listw = listw)
trr.poisson <- glm(trr ~ log(sqf) + log(crime) + FAC_disadv + FAC_hispimm + FAC_stability + splag_sqf + splag_crime, data = chicago.geo, family = "poisson")
trr.nb <- glm.nb(trr ~ log(sqf) + log(crime) + FAC_disadv + FAC_hispimm + splag_sqf + splag_crime, data = chicago.geo)

list(sqf.spatial, sqf.nb) %>% screenreg()
list(sqf.spatial_crime, sqf.nb_crime) %>% screenreg()
list(trr.spatial, trr.nb) %>% screenreg()

### use spatial models as they have better model fit!
list(sqf.spatial, sqf.spatial_crime, trr.spatial) %>% screenreg()

dataplot_spatial <-
  broom::tidy(sqf.spatial) %>%
  mutate(model = "sqf 1") %>%
  bind_rows(
    broom::tidy(sqf.spatial_crime) %>%
      mutate(model = "sqf 2")
  ) %>%
  bind_rows(
    broom::tidy(trr.spatial) %>%
      mutate(model = "trr")
  ) %>%
  mutate(significance = case_when(p.value < 0.05 ~ "significant", TRUE ~ "non significant"),
         ci.low = estimate - 1.96 * std.error,
         ci.upp = estimate + 1.96 * std.error)

save(dataplot_spatial, file = "data/export/dataplot_spatial.RData")

plot_sqf <- 
  ggplot(dataplot_spatial %>% 
           filter(model != "trr") %>% 
           filter(term %in% c("FAC_disadv", "log(crime)")) %>%
           mutate(model_gr = case_when(model == "sqf 1" ~ "Model 1", model == "sqf 2" ~ "Model 2")) %>%
           mutate(model_gr = factor(model_gr, levels = c("Model 2", "Model 1")))
       , aes(y = estimate, x = term, group = model_gr, colour = model_gr)) + 
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp, group = model_gr, alpha = significance), width = .15, position = position_dodge(width = .4), lwd = .5) + 
  geom_point(aes(shape = significance, alpha = significance, group = model_gr), 
             position = position_dodge(width = .4)) +
  scale_alpha_manual(values = c(.5, 1)) +
  scale_shape_manual(values = c(NA,8), labels = c("", "95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  ylim(-.5, 1.2) +
  coord_flip() + 
  ylab("") + xlab("") + 
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         shape = guide_legend(title = "", 
                              override.aes = list(linetype = c(0,1),
                                                  shape = c(NA,8),
                                                  size = c(0,1)))) +
  theme(text = element_text(family = "ebgaramond"),
        plot.title = element_text(hjust = .5, vjust = 1, colour = "#3C3C3C", size = 10, face = "italic"),
        axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 6),
        legend.position="right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 8, colour = "#3C3C3C"),
        strip.text.y = element_text(size = 8, colour = "#3C3C3C"),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.background  = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = "#3C3C3C", linewidth = 0.3),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key        = element_rect(fill = NA, colour = NA)) + 
  theme(aspect.ratio = 1) +
  scale_x_discrete(limits = c('log(crime)', 'FAC_disadv'),
                   breaks = c('log(crime)', 'FAC_disadv'),
                   labels = c('Previous violent\ncrime (logged)', 'Concentrated\ndisadvantage')) + 
  scale_colour_brewer(palette = "Dark2", 
                      breaks = c('Model 1', 'Model 2'))

ggsave("esc/presentation_files/plot_sqf.svg", plot = plot_sqf, bg = "transparent")

plot_trr <- 
  ggplot(dataplot_spatial %>% 
           filter(model == "trr") %>% 
           filter(term %in% c("log(sqf)", "log(crime)"))
         , aes(y = estimate, x = term)) + 
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp, alpha = significance), width = .15, position = position_dodge(width = .4), lwd = .5) + 
  geom_point(aes(shape = significance, alpha = significance), 
             position = position_dodge(width = .4)) +
  scale_alpha_manual(values = c(1)) +
  scale_shape_manual(values = c(8), labels = c("95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  ylim(-.5, 2) +
  coord_flip() + 
  ylab("") + xlab("") + 
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         shape = guide_legend(title = "", 
                              override.aes = list(linetype = c(1),
                                                  shape = c(8),
                                                  size = c(2)))) +
  theme(text = element_text(family = "ebgaramond"),
        plot.title = element_text(hjust = .5, vjust = 1, colour = "#3C3C3C", size = 10, face = "italic"),
        axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 6),
        legend.position="right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 8, colour = "#3C3C3C"),
        strip.text.y = element_text(size = 8, colour = "#3C3C3C"),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.background  = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = "#3C3C3C", linewidth = 0.3),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key        = element_rect(fill = NA, colour = NA)) + 
  theme(aspect.ratio = 1) +
  scale_x_discrete(limits = c('log(crime)', 'log(sqf)'),
                   breaks = c('log(crime)', 'log(sqf)'),
                   labels = c('Previous violent\ncrime (logged)', 'Stop & Frisk\n(logged)'))


ggsave("esc/presentation_files/plot_trr.svg", plot = plot_trr, bg = "transparent")


#######################################
lagsarlm(log(sqf) ~ FAC_disadv + FAC_hispimm, data = chicago.geo, type = "SLX", listw = listw) %>% summary
  

sqf_poi <- glm(sqf ~ FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density) + log(crime), chicago, family = "poisson")
sqf_poi <- glm(trr ~ FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density) + log(crime), chicago, family = "poisson")
sqf_poi2 <- glm(trr ~ sqf + FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density) + log(crime), chicago, family = "poisson")

sqf_nb <- glm.nb(sqf ~ FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density), chicago)
trr_nb <- glm.nb(trr ~ FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density), chicago)
trr_nb2 <- glm.nb(trr ~ log(sqf) + log(crime) + FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density), chicago)

list(sqf_poi, sqf_nb) %>% screenreg()
car::vif(sqf_nb)

list(sqf_nb, trr_nb, trr_nb2) %>% screenreg()


sqf_poi <- glm(sqf ~ perc_black + perc_hisp + perc_under18 + perc_owned + perc_fhh + perc_foreign + perc_edhighschool
                      + perc_edcollege + perc_unemployed + perc_professional + perc_poverty + perc_moved + log(pop_density), chicago, family = poisson)
sqf_poi <- glm(trr ~ perc_black + perc_hisp + perc_under18 + perc_owned + perc_fhh + perc_foreign + perc_edhighschool
               + perc_edcollege + perc_unemployed + perc_professional + perc_poverty + perc_moved + log(pop_density), chicago, family = "poisson")
sqf_poi2 <- glm(trr ~ sqf + perc_black + perc_hisp + perc_under18 + perc_owned + perc_fhh + perc_foreign + perc_edhighschool
                + perc_edcollege + perc_unemployed + perc_professional + perc_poverty + perc_moved + log(pop_density), chicago, family = "poisson")

sqf_nb <- glm.nb(sqf ~ perc_black + FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density), chicago)
trr_nb <- glm.nb(trr ~ perc_black + FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density), chicago)
trr_nb2 <- glm.nb(trr ~ log(sqf) + perc_black + FAC_disadv + FAC_hispimm + FAC_stability + log(pop_density), chicago)