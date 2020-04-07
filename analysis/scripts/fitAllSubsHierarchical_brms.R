# load brms
library(brms)

options(mc.cores = parallel::detectCores())

#load data

#make stan data a data frame
task_data_processed <- as.data.frame(task_data) 

#replace NAs in EIG and PTS columns with 0s
task_data_processed %<>% mutate(EIG4 = case_when(is.na(EIG4)==TRUE ~ 0, is.na(EIG4) == FALSE ~ EIG4), 
                              PTS4 = case_when(is.na(PTS4 )== TRUE ~ 0, is.na(PTS4) == FALSE ~ PTS4))


#set up model
bform <- bf(
  NodeClick ~ 1,
  nlf(mu1 ~ (inv_logit(theta) * EIG1 + (1 - inv_logit(theta)) * PTS1) / exp(tau)),
  nlf(mu2 ~ (inv_logit(theta) * EIG2 + (1 - inv_logit(theta)) * PTS2) / exp(tau)),
  nlf(mu3 ~ (inv_logit(theta) * EIG3 + (1 - inv_logit(theta)) * PTS3) / exp(tau)),
  nlf(mu4 ~ (inv_logit(theta) * EIG4 + (1 - inv_logit(theta)) * PTS4) / exp(tau)),
  family = categorical(refcat = NA),
  nl = TRUE,
  lf(theta ~ (age + I(age^2)) + (1 | SubID), 
     tau ~ (age + I(age^2)) + (1 | SubID), 
     decomp = "QR")
)


prior <- prior(student_t(3, 0, 1), class = "b", nlpar = "theta") + 
  prior(student_t(3, 0, 5), class = "b", coef = "Intercept", nlpar = "theta") +
  prior(student_t(3, 0, 1), class = "b", nlpar = "tau") + 
  prior(student_t(3, 0, 5), class = "b", coef = "Intercept", nlpar = "tau") +
  prior(student_t(3, 0, 1), class = "sd", nlpar = "theta") + 
  prior(student_t(3, 0, 1), class = "sd", nlpar = "tau")

#run model
fit <- brm(bform,  
           task_data_processed,         
           iter = 2000,
           save_all_pars = TRUE,
           prior = prior)

save(fit, file = "model_fits_brms_hierarchical.Rda")

#display output
fit

#plot
jpeg("hierarchical_model_output.jpg", width = 11, height = 6, unit = "in", res = 600)
plot(fit, 
     pars = c("theta_age", "theta_IageE2", "tau_age", "tau_IageE2"),
     combo = c("dens","intervals"),
     fixed = T,
     theme = theme_bw())

dev.off()
