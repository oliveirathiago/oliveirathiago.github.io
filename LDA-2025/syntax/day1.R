##################################
### LONGITUDINAL DATA ANALYSIS ###
##################################

## Thiago R. Oliveira

## Day 1

# Install the lme4 package; you only need to do that once
## install.packages("lme4") # (uncomment if you need to install)

# Load the lme4 package
library(lme4)

## We are also going to use the tidyverse package, so let's load it now

# Install the tidyverse package; you only need to do that once
## install.packages("tidyverse") # (uncomment if you need to install)

# Load the tidyverse package
library(tidyverse)

# load the 'sleepstudy' dataset
data("sleepstudy", package = "lme4")

# If you want to read more about the dataset:
help("sleepstudy")

# we can use the nrow() function to check the number of observations
nrow(sleepstudy)

# checking how many individuals were part of the study
length(unique(sleepstudy$Subject))

# we can use the count() function
count(sleepstudy, Subject)

# pivot: from long to wide format
sleep_wide <- 
  sleepstudy %>% 
  pivot_wider(names_from = Days, 
              values_from = Reaction)

# checking that pivoting wider worked
head(sleep_wide)

# pivot: from wide to long
sleep_long <- 
  sleep_wide %>%
  pivot_longer(
    cols = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'),
    names_to = "Days",
    values_to = "Reaction"
  )

# check that pivoting longer worked
head(sleep_long)

# plotting trajectories
ggplot(sleepstudy, aes(x = Days, y = Reaction, group = Subject, color = Subject)) +
  geom_line(show.legend = FALSE) +
  labs(title = "Reaction Time by Day of Sleep Deprivation",
       x = "Days",
       y = "Reaction Time (ms)") +
  theme_minimal()