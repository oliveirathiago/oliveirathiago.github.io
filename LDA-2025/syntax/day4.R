#########################

# load the dataset using the load() function. Make sure the data is located in the appropriate working directory.
load("data/dinas_golden_dawn.Rdata")

### QUESTION 1. Using only the observations from the post-treatment period (i.e. 2016), implement a regression 
#     which compares the Golden Dawn share of the vote for the treated and untreated municipalities. Does the 
#     coefficient on this regression represent the average treatment effect on the treated? If so, why? If not, 
#     why not?

# load the dplyr package
library(dplyr)

# using only observations from the post-treatment period:
post_treatment_data <- filter(muni, year == 2016)

## comparing GD votes for treated and non-treated units

# fit a cross-sectional regression model
post_treatment_period_regression <- lm(gdvote ~ treatment, data = post_treatment_data)

# print results
summary(post_treatment_period_regression)


### QUESTION 2. Calculate the sample difference-in-differences between 2015 and 2016. For this question, you should
#     calculate the relevant differences “manually”, in that you should use the mean function to construct the appropriate 
#     comparisons. Calculate both the 'difference in changes' and the 'difference in differences'. What does this 
#     calculation imply about the average treatment effect on the treated?

# GD share of votes in non-treated municipalities before the treatment:
mean_control_before <- mean(muni$gdvote[muni$ever_treated == 0 & muni$year == 2015])
mean_control_before

# GD share of votes in non-treated municipalities after the treatment:
mean_control_after <- mean(muni$gdvote[muni$ever_treated == 0 & muni$year == 2016])
mean_control_after

# GD share of votes in treated municipalities before the treatment:
mean_treat_before <- mean(muni$gdvote[muni$ever_treated == 1 & muni$year == 2015])
mean_treat_before

# GD share of votes in treated municipalities after the treatment:
mean_treat_after <- mean(muni$gdvote[muni$ever_treated == 1 & muni$year == 2016])
mean_treat_after

# Changes in treatment group:
changes_treatment <- mean_treat_after - mean_treat_before
changes_treatment

# Changes in control group:
changes_control <- mean_control_after - mean_control_before
changes_control

# Difference between units pre-treatment:
diff_before <- mean_treat_before - mean_control_before
diff_before

# Difference between units post-treatment:
diff_after <- mean_treat_after - mean_control_after
diff_after

# Difference in changes
changes_treatment - changes_control

# Difference in differences
diff_after - diff_before

### QUESTION 3. Use a linear regression with an appropriate interaction term to estimate the difference-in-differences. 
#     For this question, you should again focus only on the years 2015 and 2016.

## Subset the data to observations in either 2015 or 2016
muni_simple <- muni %>% filter(year >= 2015)

## Construct a dummy variable for the post-treatment period.
muni_simple <- mutate(muni_simple, period = case_when(year == 2016 ~ 1,
                                                      TRUE ~ 0))

# Calculate the difference-in-differences
interaction_model <- lm(gdvote ~ ever_treated * period, data = muni_simple)
summary(interaction_model)


### QUESTION 4. Let's convert the dataset from a "long" format to a "wide" format. That is, let each row represent one 
#     unit (i.e., a municipality), and variables measured at different time period appear as new columns. We can use the 
#     `reshape()` function and specify the data that we want to reshape, the variable that represents the time period, the 
#     variable that represents the units, the direction (i.e., from long to wide or from wide to long), and a character 
#     distinguishing variables at different periods:

# load the tidyr package
library(tidyr)

# use the pivot_wider function to reshape the dataset
muni_simple_wide <- muni_simple %>%
  pivot_wider(
    names_from = year,
    values_from = -c(municipality, year)
  )

# check columns of the wide dataset
names(muni_simple_wide)


### QUESTION 6. Let's assess the parallel trends assumption by plotting the evolution of Golden Dawn’s vote shares from 2012 
#     to 2016 for both the treated and non-treated municipalities. We can start using the `aggregate()` function to group 
#     average GD vote share for each treatment group and each period.

# calculate grouped averages
group_period_averages <- muni %>%
  group_by(year, ever_treated) %>%
  summarise(gdvote = mean(gdvote, na.rm = TRUE), .groups = "drop")

group_period_averages

# load the ggplot2 package
library(ggplot2)

trends <- ggplot(group_period_averages, aes(x = year, y = gdvote, color = ever_treated)) + 
  geom_point() + geom_line(aes(group = ever_treated))
trends


### QUESTION 7. Use a fixed-effects regression to estimate the difference-in-differences. Remember that the fixed-effect 
#     estimator for the diff-in-diff model requires “two-way” fixed-effects, i.e. sets of dummy variables for a) units and 
#     b) time periods. In R, you do not need to construct such dummy variables manually. It is sufficient to use the `as.factor()` 
#     function within the `lm()` function to tell R to treat a certain variable as a set of dummies.

twfe_model <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + treatment,
                 data  = muni)

# If you don't want a regression output with coefficients for 191 dummies and three years,
# you can use the screenreg() function from the texreg package, with the omit.coef option
# to omit coefficients
library(texreg)
screenreg(twfe_model, omit.coef = 'municipality|year')


### QUESTION 8. Using the same model that you implemented in question 6, swap the `treatment` variable for the `trarrprop` 
#     variable, which is a continuous treatment variable measuring the number of refugee arrivals per capita. What is the 
#     estimated average treatment effect on the treated using this variable?

twfe_model_2 <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + trarrprop,
                   data  = muni)

screenreg(list(twfe_model, twfe_model_2), omit.coef = "municipality|year")