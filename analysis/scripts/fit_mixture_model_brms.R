## Fit mixture model to participant data
# 4/3/20

#### Load needed libraries ####
library("brms")
library("tidyverse")

options(mc.cores = parallel::detectCores())

#### Load data ####
load("chip_task_data_processed.Rda")

# Recode NAs as 0
task_data <- task_data %>%
  mutate(EIG4 = case_when(is.na(EIG4) == TRUE ~ 0, 
                          is.na(EIG4) == FALSE ~ EIG4), 
         PTS4 = case_when(is.na(PTS4) == TRUE ~ 0, 
                          is.na(PTS4) == FALSE ~ PTS4)) 

#### Fit model ####
  bform <- bf(
    NodeClick ~ 1,
    nlf(mu1 ~ (theta * EIG1 + (1 - theta) * PTS1) / tau),
    nlf(mu2 ~ (theta * EIG2 + (1 - theta) * PTS2) / tau),
    nlf(mu3 ~ (theta * EIG3 + (1 - theta) * PTS3) / tau),
    nlf(mu4 ~ (theta * EIG4 + (1 - theta) * PTS4) / tau),
    lf(theta ~ 0 + as.factor(SubID), 
       tau ~ 0 + as.factor(SubID)),
    family = categorical(refcat = NA),
    nl = TRUE
  )

  prior <- prior(beta(1, 1), class = "b", lb = 0, ub = 1, nlpar = "theta") +
    prior(gamma(1, .1), class = "b", lb = 0, nlpar = "tau")
  
  fit <- brm(bform,  
             task_data,         
             iter = 2000,
             save_all_pars = TRUE,
             prior = prior)
  

#extract coefficients
estimates <- as.data.frame(fixef(fit))
  
#combine the theta and tau estimates into a data frame
estimates <- estimates %>% 
  rownames_to_column(var = "rowname") %>%
  separate(rowname, into = c("parameter", "SubID"), sep = ("_as.factorSubID")) %>%
  select(parameter, SubID, Estimate) %>%
  pivot_wider(names_from = parameter, values_from = Estimate) 

save(estimates, file = "brms_mixtureModel_estimates.Rda")
save(fit, file = "bms_mixtureModel_fullModel.Rda")


