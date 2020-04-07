## Fit simulated data to mixture model
# 3/1/20

#### Load needed libraries ####
library("brms")
library("tidyverse")

options(mc.cores = parallel::detectCores())

#### Load simulated data ####
load("parameter_recoverability/simulated_choices_march24.Rda")
load("problem_list.Rda")

# recode na trials as 0
choice_df <- choice_df %>%
  arrange(sim_num, problem_num) %>%
  mutate(EIG4 = case_when(is.na(EIG4)==TRUE ~ 0, 
                   is.na(EIG4) == FALSE ~ EIG4), 
         PTS4 = case_when(is.na(PTS4 )== TRUE ~ 0, 
                   is.na(PTS4) == FALSE ~ PTS4)) 

# randomly select half the trials for each subject by generating a random vector of 40 1s and 0s 1500x
trial_filter <- vector()
for (sub in c(1:1500)){
  vec <- rep(0:1, each = 20)
  rand_vec <- sample(vec)
  trial_filter <- c(trial_filter, rand_vec)
}

#make trial_filter a column in choice_df
trial_filter <- as.data.frame(trial_filter)
choice_df <- bind_cols(choice_df, trial_filter) %>%
  filter(trial_filter == 1) %>%
  filter(sim_num > 1000)


#### Fit model ####
  bform <- bf(
    choice ~ 1,
    nlf(mu1 ~ (theta * EIG1 + (1 - theta) * PTS1) / tau),
    nlf(mu2 ~ (theta * EIG2 + (1 - theta) * PTS2) / tau),
    nlf(mu3 ~ (theta * EIG3 + (1 - theta) * PTS3) / tau),
    nlf(mu4 ~ (theta * EIG4 + (1 - theta) * PTS4) / tau),
    lf(theta ~ 0 + as.factor(sim_num), 
       tau ~ 0 + as.factor(sim_num)),
    family = categorical(refcat = NA),
    nl = TRUE
  )

  prior <- prior(beta(1, 1), class = "b", lb = 0, ub = 1, nlpar = "theta") +
    prior(gamma(1, .1), class = "b", lb = 0, nlpar = "tau")
  
  fit <- brm(bform,  
             choice_df,         
             iter = 5000,
             save_all_pars = TRUE,
             prior = prior)
  

#extract coefficients
estimates <- as.data.frame(fixef(fit))
  
#combine the theta and tau estimates into a data frame
temp <- estimates %>% 
  rownames_to_column(var = "rowname") %>%
  separate(rowname, into = c("parameter", "sim_num"), sep = ("_as.factorsim_num")) %>%
  select(parameter, sim_num, Estimate) %>%
  pivot_wider(names_from = parameter, values_from = Estimate) %>%
  rename(fitted_theta = theta, fitted_tau = tau)

save(temp, file = "simulation_params_brms_half_1001_1500.Rda")
save(fit, file = "simulation_fit_brms_half_1001_1500.Rda")

#load unfiltered choice_df
load("parameter_recoverability/simulated_choices_march24.Rda")

#Load all parameter estimates and combine
load("simulation_params_brms_half_1_500.Rda")
temp1 <- temp

load("simulation_params_brms_half_501_1000.Rda")
temp2 <- temp

load("simulation_params_brms_half_1001_1500.Rda")
temp3 <- temp

#combine
param_estimates <- bind_rows(temp1, temp2, temp3)
param_estimates$sim_num <- as.numeric(param_estimates$sim_num)



#get subject numbers and "true" parameters
sub_params <- choice_df %>%
  select(sim_num, theta, tau) %>%
  unique()

#combine with recovered params
params <- full_join(param_estimates, sub_params, by = "sim_num")

#plot parameter vs. recovered value
theta_recoverability_plot <- ggplot(params, aes(x = theta, y = fitted_theta)) +
  geom_boxplot(alpha = .1, aes(group = theta)) +
  facet_wrap(~ tau, labeller = label_both) +
  ylab("Theta Estimate") +
  xlab("True Theta") + 
  theme_bw()
theta_recoverability_plot 
ggsave('theta_recoverability_exphalf.png', dpi = 600)

tau_recoverability_plot <- ggplot(params, aes(x = tau, y = fitted_tau)) +
  geom_boxplot(alpha = .2, aes(group = tau)) +
  ylab("Tau Estimate") +
  xlab("True Tau") + 
  theme_bw() 
tau_recoverability_plot
ggsave('tau_recoverability_exphalf.png', dpi = 600)


theta_correlations <- lapply(split(params, params$tau), function(x){Hmisc::rcorr(x$theta, x$fitted_theta)})
tau_correlations <- lapply(split(params, params$theta), function(x){Hmisc::rcorr(x$tau, x$fitted_tau)})



