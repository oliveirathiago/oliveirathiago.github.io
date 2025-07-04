library(tidyverse)
library(haven)
library(lme4)

reading <- read_stata("data/read_long.dta") %>%
  dplyr::select(childid:read, year)

write_dta(reading, "data/read_long.dta")

reading <- read_stata("data/read_long.dta")

random_intercepts <- lmer(read ~ t + (1 | childid), data = reading)

summary(random_intercepts)

# Step 1: Get the fixed effect estimates
fixed_effects <- fixef(random_intercepts)

# Step 2: Get random intercepts
ranef_df <- ranef(random_intercepts)$childid %>%
  tibble::rownames_to_column("childid")

# Step 3: Pick the first 10 child IDs in the dataset
first10_ids <- unique(reading$childid)[1:10]

# Step 4: Create a new dataframe to hold predicted trajectories
# You’ll want a reasonable range of time points to plot
time_range <- seq(min(reading$t), max(reading$t), length.out = 10)

# Build prediction grid
plot_data <- expand.grid(childid = first10_ids, t = time_range)

# Merge with random effects
plot_data <- left_join(plot_data %>% mutate(childid = factor(childid)), ranef_df %>% mutate(childid = factor(childid)), by = "childid")

# Add fixed effects and compute predicted read scores
plot_data <- plot_data %>%
  mutate(pred_read = fixed_effects["(Intercept)"] + `(Intercept)` + 
           fixed_effects["t"] * t)

# Step 5: Plot
pdf("plots/random_intercepts.pdf")
ggplot(plot_data, aes(x = t, y = pred_read, group = childid, color = childid)) +
  geom_line() + ylim(0,10) +
  theme_minimal()
dev.off()




random_slope <- lmer(read ~ t + (t | childid), data = reading)

summary(random_slope)

fixed_effects <- fixef(random_slope)

# 1. Fixed effects
fixed <- fixef(random_slope)

# 2. Random effects (intercepts and slopes)
ranefs <- ranef(random_slope)$childid %>%
  rownames_to_column("childid") %>%
  rename(ran_intercept = `(Intercept)`, ran_slope = t)

# 3. First 10 unique child IDs
first10_ids <- unique(reading$childid)[1:10]

# 4. Time range to use in plotting
time_range <- seq(min(reading$t), max(reading$t), length.out = 10)

# 5. Build prediction grid
plot_data <- expand.grid(childid = first10_ids, t = time_range)

# 6. Merge random effects
plot_data <- left_join(plot_data %>% mutate(childid = factor(childid)), ranefs %>% mutate(childid = factor(childid)), by = "childid")

# 7. Compute predicted read scores
plot_data <- plot_data %>%
  mutate(
    pred_read = (fixed["(Intercept)"] + ran_intercept) + 
      (fixed["t"] + ran_slope) * t
  )

# 8. Plot
pdf("plots/random_slope.pdf")
ggplot(plot_data, aes(x = t, y = pred_read, group = childid, color = childid)) +
  geom_line() + ylim(0,10) +
  theme_minimal()
dev.off()


random_quadratic <- lmer(read ~ t + t_squared + (t | childid), reading %>% mutate(t_squared = t^2))
summary(random_quadratic)

random_quadratic_2 <- lmer(read ~ t + t_squared + (t + t_squared | childid), reading %>% mutate(t_squared = t^2))
summary(random_quadratic_2)




physfunc <- read_stata("data/physfunc.dta")