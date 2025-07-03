## We are also going to use the tidyverse package, so let's load it now

# Install the tidyverse package; you only need to do that once
## install.packages("tidyverse") # (uncomment if you need to install)

# Load the tidyverse package
library(tidyverse)

## We also need the haven package to import datasets into R
## install.packages("haven") # (uncomment if you need to install)
library(haven)

# Finally, we also need the lavaan package---the SEM package for R
## install.packages("lavaan") # (uncomment if you need to install)

# Load the lavaan package
library(lavaan)

# import data into R
reading <- read_stata("https://thiagoroliveira.com/LDA-2025/data/read_long.dta")

# reshaping the dataset into a wider format
reading_wide <- 
  reading %>%
  pivot_wider(
    id_cols = c(childid, male, homecog),             # variables that do NOT vary with time
    names_from = t,                         # time variable
    values_from = c(read, year),   # time-varying variables
    names_sep = "_t"                        # e.g., read_t0, read_t1, etc.
  )

# double check it worked
head(reading_wide)

# specify the LGCM
lgcm_ri <- '
  # Latent intercept factor
  intercept =~ 1*read_t0 + 1*read_t1 + 1*read_t2 + 1*read_t3
'

# estimate the LGCM
fit_lgcm_ri <- sem(lgcm_ri, data = reading_wide, missing = "fiml")

# check results
summary(fit_lgcm_ri, fit.measures = TRUE, standardized = TRUE)

# specify the LGCM
lgcm_slope <- '
  # Latent intercept factor
  intercept =~ 1*read_t0 + 1*read_t1 + 1*read_t2 + 1*read_t3
  
  # Latent slope factor
  slope =~ 0*read_t0 + 1*read_t1 + 2*read_t2 + 3*read_t3
  
  # Latent variables allowed to covary
  intercept ~~ slope
'

# estimate the LGCM
fit_lgcm_slope <- sem(lgcm_slope, data = reading_wide, missing = "fiml")

# check results
summary(fit_lgcm_slope, fit.measures = TRUE, standardized = TRUE)

# specify the LGCM
lgcm_quadratic <- '
  # Latent intercept factor
  intercept =~ 1*read_t0 + 1*read_t1 + 1*read_t2 + 1*read_t3
  
  # Latent slope factor
  slope =~ 0*read_t0 + 1*read_t1 + 2*read_t2 + 3*read_t3
  
  # Latent quadratic factor
  quadratic =~ 0*read_t0 + 1*read_t1 + 4*read_t2 + 9*read_t3
  
  # Fix variance of quad to zero (fixed effect only)
  quadratic ~~ 0*quadratic
  
  # Latent variables allowed to covary
  intercept ~~ slope
'

# estimate the LGCM
fit_lgcm_quadratic <- sem(lgcm_quadratic, data = reading_wide, missing = "fiml")

# check results
summary(fit_lgcm_quadratic, fit.measures = TRUE, standardized = TRUE)

# specify the LGCM
lgcm_quadratic_latent <- '
  # Latent intercept factor
  intercept =~ 1*read_t0 + 1*read_t1 + 1*read_t2 + 1*read_t3
  
  # Latent slope factor
  slope =~ 0*read_t0 + 1*read_t1 + 2*read_t2 + 3*read_t3
  
  # Latent quadratic factor
  quadratic =~ 0*read_t0 + 1*read_t1 + 4*read_t2 + 9*read_t3
  
  # Latent variables allowed to covary
  intercept ~~ slope
  intercept ~~ quadratic
  slope ~~ quadratic
'

# estimate the LGCM
fit_lgcm_quadratic_latent <- sem(lgcm_quadratic_latent, data = reading_wide, missing = "fiml")

# check results
summary(fit_lgcm_quadratic_latent, fit.measures = TRUE, standardized = TRUE)

# check fit measures of each of the four models
fitMeasures(fit_lgcm_ri, c("aic", "bic", "rmsea", "cfi", "tli"))
fitMeasures(fit_lgcm_slope, c("aic", "bic", "rmsea", "cfi", "tli"))
fitMeasures(fit_lgcm_quadratic, c("aic", "bic", "rmsea", "cfi", "tli"))
fitMeasures(fit_lgcm_quadratic_latent, c("aic", "bic", "rmsea", "cfi", "tli"))

# specify the LGCM
lgcm_quadratic_covariates <- '
  # Latent intercept factor
  intercept =~ 1*read_t0 + 1*read_t1 + 1*read_t2 + 1*read_t3
  
  # Latent slope factor
  slope =~ 0*read_t0 + 1*read_t1 + 2*read_t2 + 3*read_t3
  
  # Latent quadratic factor
  quadratic =~ 0*read_t0 + 1*read_t1 + 4*read_t2 + 9*read_t3
  
  # Fix variance of quad to zero (fixed effect only)
  quadratic ~~ 0*quadratic
  
  # Latent variables allowed to covary
  intercept ~~ slope
  
  # Regress growth factors on time-invariant covariates
  intercept ~ male + homecog
  slope     ~ male + homecog
  
  # If we had, this is how we would regress observed outcomes on time-varying covariates
  # read_t0 ~ x_t0
  # read_t1 ~ x_t1
  # read_t2 ~ x_t2
  # read_t3 ~ x_t3
'

# estimate the LGCM
fit_lgcm_quadratic_covariates <- sem(lgcm_quadratic_covariates, data = reading_wide, missing = "fiml")

# check results
summary(fit_lgcm_quadratic_covariates, fit.measures = TRUE, standardized = TRUE)

