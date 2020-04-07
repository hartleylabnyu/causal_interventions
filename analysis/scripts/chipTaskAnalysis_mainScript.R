## Chip Task Analysis — Main Script ##
# Last updated: 4/7/20
# katenuss@nyu.edu

#### ANALYSES IN MAIN MANUSCRIPT ####

### SET UP EVERYTHING ### ------------------------------------------
## Load needed libraries ##
library(tidyverse)
library(magrittr)
library(pander)
library(glue)
library(afex)
library(lmSupport)
library(sjPlot)

## Load functions, themes, etc. ##
#define new function so that scale returns a vector, not a matrix
scale_this <- function(x) as.vector(scale(x))

#create standard theme to use for ggplot
chip.theme <- theme(panel.background = element_rect(fill='transparent'),
                    axis.line = element_line(color='black'),
                    panel.grid.minor = element_line(color='transparent'),
                    axis.title.x = element_text(size=20, vjust=-.25),
                    axis.title.y = element_text(size=20, vjust=1),
                    axis.text.x = element_text(size=18, colour="black"),
                    axis.text.y = element_text(size=18, colour="black"),
                    panel.spacing.x = unit(2, "lines"),
                    legend.text=element_text(size=18),
                    legend.title = element_text(size = 20),
                    plot.title = element_text(size=20, face = "bold", hjust = .5), strip.text.x = element_text(size=18), strip.text.y = element_text(size=18, face="bold"), strip.background = element_rect(colour= "black", fill = "transparent"))

#Define 3 theme colors to use
color1 = "#6B7A8F"
color2 = "#F7882F"
color3 = "#F7C331"

##### PROCESS DATA ------------------------------------------
#Either run the process data script, or load processed data by changing which line is commented 

# source("scripts/chipTask_processData.R"))

load('processed_data/chip_task_data_processed.Rda')
load('processed_data/subInfo.Rda')

##### ANALYSIS ------------------------------------------
#### Participant statistics ####

#round ages
subInfo %<>% 
  mutate(roundedAge = trunc(age), gender = as.factor(gender)) %>% 
  filter(roundedAge > 1)

#create histogram of subject genders and ages
subHist <- ggplot(subInfo, aes(x = roundedAge, fill = gender), color = "white") +
  geom_histogram(bins = 19, aes(fill = gender), color = "white") +
  scale_fill_manual(values = c(color1, color2), labels = c("Male", "Female"), name = "Sex") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  scale_x_continuous(breaks = c(8, 12, 16, 20, 24)) +
  ylab("n") +
  xlab("Age") + 
  chip.theme
subHist

# Run linear regression testing the effects of age on IQ
ageIQ.lm <- lm(IQ ~ scale_this(age), data = subInfo)
ageIQeffectSize <- modelEffectSizes(ageIQ.lm, Print = F, Digits = 3)
ageIQsummary <-(anova(ageIQ.lm))
ageIQsummary$pEta[1] <-ageIQeffectSize$Effects[2,3]
ageIQsummary$pEta[2] <-NA
pander(ageIQsummary)

# Plot correlation between age and IQ
ageIQplot <- ggplot(subInfo, aes(x = age, y = IQ)) +
  geom_point(color = color2) +
  geom_smooth(method = "lm", color = color3, fill = color3) +
  xlab("Age") + 
  ylab("IQ") +
  chip.theme 
ageIQplot

#### Fit mixture model  ------------------------------------------
# Run or load model 
#source('scripts/fit_mixture_model_brms.R')

#Load model parameter estimates
load("processed_data/brms_mixtureModel_estimates.Rda")

#Merge parameter estimates with other data
temp <- full_join(task_data, estimates, by = c("SubID"))

#make data frame with subject, age, age group, and theta
subParams <- temp %>% 
  dplyr::group_by(SubID, age, ageGroup, IQ) %>% 
  dplyr::summarize(meanTheta = mean(theta), meanTau = mean(tau))

#### Model diagnostics  ------------------------------------------
load("processed_data/brms_mixtureModel_fullModel.Rda")
model_data <- as.data.frame(summary(fit)$fixed) %>%
  rownames_to_column(var = "rowname") %>%
  separate(rowname, into = c("parameter", "SubID"), sep = ("_as.factorSubID"))

tau_data <- model_data %>%
  filter(parameter == "tau")

tau_diagnostics <- tau_data %>%
  dplyr::summarize(maxRhat = max(Rhat),
                   minBulk = min(Bulk_ESS),
                   meanBulk = mean(Bulk_ESS),
                   minTail = min(Tail_ESS),
                   meanTail = mean(Tail_ESS))

theta_data <- model_data %>%
  filter(parameter == "theta")

theta_diagnostics <- theta_data %>%
  dplyr::summarize(maxRhat = max(Rhat),
                   minBulk = min(Bulk_ESS),
                   meanBulk = mean(Bulk_ESS),
                   minTail = min(Tail_ESS),
                   meanTail = mean(Tail_ESS))

#### Theta by age  ------------------------------------------------

#scale continuous variables
subParams$ageScaled <- scale_this(subParams$age)
subParams$ageSquaredScaled <- (subParams$ageScaled)^2

#run model with linear age
linearAgeThetaModel <- lm(meanTheta ~ ageScaled, data = subParams)

#run model with quadratic age
quadraticAgeThetaModel <- lm(meanTheta ~ ageScaled + ageSquaredScaled, data = subParams)

# compare them
thetaAgeVsAgeSquared <- anova(linearAgeThetaModel, quadraticAgeThetaModel)
pander(thetaAgeVsAgeSquared)
# model with age squared fits better

# display model output - F values and beta weights
ageThetaSummary <-(anova(quadraticAgeThetaModel))
pander(ageThetaSummary)
pander(summary(quadraticAgeThetaModel))

# display effect size
thetaAgeEffectSize <- modelEffectSizes(quadraticAgeThetaModel, Print = F, Digits = 3)
pander(thetaAgeEffectSize)

# plot age vs. theta
thetaAgePlot <- ggplot(subParams, aes(x = age, y = meanTheta)) +
  geom_point(stat = "identity", color = color1) +
  geom_smooth(formula = (y~ poly(x,2)), method = glm, color = color1, fill = color1) +
  ylab("Theta Estimate") +
  xlab("Age") +
  chip.theme
thetaAgePlot
#ggsave('thetaAgePlot.png', thetaAgePlot, dpi = 1000)


#### Theta by age and IQ -------------------------------------------------
#scale IQ
subParams$IQScaled <- scale_this(subParams$IQ)

#run model with quadratic age and IQ
quadraticAgeIQThetaModel <- lm(meanTheta ~ (ageScaled + ageSquaredScaled) * IQScaled, data = subParams)
pander(summary(quadraticAgeIQThetaModel))

#no main effect of IQ or interactions


#### Tau by age -------------------------------------------------
#compare age-tau models
linearTauAgeModel <- lm(meanTau ~ ageScaled, data = subParams)
quadraticTauAgeModel <- lm(meanTau ~ ageScaled + ageSquaredScaled, data = subParams)
tauAgeVsAgeSquared <- anova(linearTauAgeModel, quadraticTauAgeModel)
pander(tauAgeVsAgeSquared)
# linear age model fits better

#linear tau model
pander(summary(linearTauAgeModel))
pander(linearTauAgeModel)

tauEffectSizes <- modelEffectSizes(linearTauAgeModel, Print = F, Digits = 3)
tauEffectSizes

#plot age vs. tau
tauAgePlot <- ggplot(subParams, aes(x = age, y = meanTau)) +
  geom_point(stat = "identity", color = color2) +
  geom_smooth(method = lm, color = color2, fill = color2) +
  ylab("Tau Estimate") +
  xlab("Age") +
  chip.theme
tauAgePlot
#ggsave('tauAgePlot.png', tauAgePlot, dpi = 1000)


#### Tau by age and IQ -------------------------------------------------
ageIQTauModel <- lm(meanTau ~ ageScaled * IQScaled, data = subParams)
pander(summary(ageIQTauModel))

#no effect of IQ or interactions

#### Theta by tau -------------------------------------------------
thetaTauModel <- lm(meanTheta ~ meanTau, data = subParams)
summary(thetaTauModel)
thetaTauEffectSizes <- modelEffectSizes(thetaTauModel, Digits = 3)


# plot theta vs. tau
thetaTauPlot <- ggplot(subParams, aes(x = meanTheta, y = meanTau)) +
  geom_point(stat = "identity", color = color2) +
  geom_smooth(method = lm, color = color3, fill = color3) +
  ylab("Tau Estimate") +
  xlab("Theta Estimate") +
  chip.theme
thetaTauPlot


#### Causal inference -----------------------------------------------------------
# Compute posterior probability of each structure given observed evidence
source("scripts/chipTask_inference.R")

# Rename variables to make them easier to keep track of and compute posterior of structure selected.
# Note: These variable names are confusing. struc1Number, struc2Number, struc1, and struc2 are variables originally imported from the "strucFiles.csv"
# They map the image numbers to the matrices. In this file, struc1 and struc2 are arbitrary. Thus they do not
# contain information about the side of the screen that participants saw them on, or which one was correct.
# That information is in SideTrueStruc and StrucSelected (which refers to the side participants selected).
# We will rename the variables here to make things clearer.

task_data %<>%
  dplyr::rename(sideSelected = StrucSelected,
                struc1_ID = struc1Number, #arbitrary side
                struc2_ID = struc2Number, #arbitrary side
                trueStrucID = TrueStrucNum, 
                foilStrucID = FoilStrucNum) 

task_data %<>%
  dplyr::mutate(trueStrucSelected = case_when(sideSelected == SideTrueStruc ~ 1, #did they select the correct side?
                                              sideSelected != SideTrueStruc ~ 0), 
                strucID_selected = case_when(trueStrucSelected == 1 ~ trueStrucID, #which structure did they select?
                                             trueStrucSelected == 0 ~ foilStrucID),
                strucNumSelected = case_when(strucID_selected == struc1_ID ~ 1, #was this labeled as 1 or 2 in the other file?
                                             strucID_selected == struc2_ID ~ 2),
                postSelected = case_when(strucNumSelected == 1 ~ struc1p, # what is the posterior probability of the structure they selected?
                                         strucNumSelected == 2 ~ struc2p))
                

# average posterior probabliity of struc selected by age group plot
posteriorProbStrucSub <- task_data %>% 
  dplyr::group_by(ageGroup, SubID, age) %>% 
  dplyr::summarize(meanPostSub = mean(postSelected, na.rm = TRUE), N = n(), 
            seProp = sd(postSelected, na.rm = TRUE)/(sqrt(N)))

posteriorProbStruc <- posteriorProbStrucSub %>% 
  dplyr::group_by(ageGroup) %>% 
  dplyr::summarize(meanPost = mean(meanPostSub, na.rm = TRUE), N = n(), sePost = sd(meanPostSub, na.rm = TRUE)/(sqrt(N)))


#define error bars
accLims <- aes(ymax = meanPost + sePost, ymin = meanPost - sePost)

#mean posterior plot
meanPostPlot <- ggplot(posteriorProbStruc, aes(x = ageGroup, y = meanPost, fill = ageGroup)) + 
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_errorbar(accLims, stat = "identity", position = position_dodge(width = .9), width = .1) +
  #facet_wrap(~ageGroup, nrow = 2) +
  ggtitle("Mean Posterior Probability of Selected Structure") + 
  xlab("Age Group") + ylab("Proportion of Trials") +
  scale_fill_manual(values = c(color1, color2, color3), name = "Age Group") +
  chip.theme +
  coord_cartesian(ylim = c(0,1)) + 
  NULL
meanPostPlot

#### Posterior probability of structure selected by age -------------------------------------
# scale variables
task_data$ageScaled <- scale_this(task_data$age)
task_data$ageSquaredScaled <- (task_data$ageScaled)^2

# Run model w/ age and squared age
posteriorProbModel.lmer.1 <- mixed(postSelected ~ ageScaled + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")
posteriorProbModel.lmer.2 <- mixed(postSelected ~ (ageScaled + ageSquaredScaled) + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")

#compare them
anova(posteriorProbModel.lmer.1, posteriorProbModel.lmer.2)
#quadratic model fits better

#run best-fitting model
posteriorProbModel.2 <- mixed(postSelected ~ (ageScaled + ageSquaredScaled) + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)))
posteriorProbModel.2

#### Posterior probability of structure selected by age and IQ -------------------------------------
# scale variables
task_data$IQScaled <- scale_this(task_data$IQ)

#Add IQ into best-fitting model
posteriorProbModel.2.IQ <- mixed(postSelected ~ (ageScaled + ageSquaredScaled) * IQScaled + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)))
posteriorProbModel.2.IQ

posteriorProbModel.2.IQ.lmer <- mixed(postSelected ~ ageScaled * IQScaled + ageSquaredScaled * IQScaled + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")

# Main effect of IQ, age * IQ interaction
age.IQ.postProb.plot <- plot_model(posteriorProbModel.2.IQ.lmer, type = "pred", 
                                   terms = c("ageScaled", "IQScaled"),
                                   axis.title = "Posterior probability of selected structure",
                                   colors = c(color1, color2, color3)) +
  chip.theme +
  xlab("Age") +
  ggtitle("")
age.IQ.postProb.plot


#### Confidence by posterior probabilities ---------------------------------------------------------
#scale variables
task_data$confScaled <- scale_this(task_data$ConfRating)
task_data$postSelectedScaled <- scale_this(task_data$postSelected)

# Run model w/ age and squared age
posteriorProbModel.conf.lmer.1 <- mixed(confScaled ~ ageScaled * postSelectedScaled + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")
posteriorProbModel.conf.lmer.2 <- mixed(confScaled ~ (ageScaled + ageSquaredScaled) * postSelectedScaled + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")

#compare them
anova(posteriorProbModel.conf.lmer.1, posteriorProbModel.conf.lmer.2)
#model with age and age squared fits best

#run best-fitting model
posteriorProbModel.conf.2 <- mixed(confScaled ~ (ageScaled + ageSquaredScaled) * postSelectedScaled + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)))
posteriorProbModel.conf.2

#### Confidence by posterior probabilities and IQ  ---------------------------------------------------------
#add IQ into best-fitting model
posteriorProbModel.conf.2.IQ <- mixed(confScaled ~ (ageScaled + ageSquaredScaled) * IQScaled * postSelectedScaled + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)))
posteriorProbModel.conf.2.IQ

posteriorProbModel.conf.2.IQ.lmer <- mixed(confScaled ~ ageScaled * IQScaled * postSelectedScaled + ageSquaredScaled * IQScaled * postSelectedScaled + (1|SubID), data = task_data, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")


# IQ x posterior interaction effect
# age x IQ x posterior interaction effect 

age.IQ.postProb.conf.plot <- plot_model(posteriorProbModel.conf.2.IQ.lmer, type = "pred", 
                                   terms = c("postSelectedScaled", "ageScaled", "IQScaled"),
                                   axis.title = "Posterior probability of selected structure",
                                   colors = c(color1, color2, color3)) +
  chip.theme +
  ggtitle("")
age.IQ.postProb.conf.plot 


#### Compute evidence sensitivity  -------------------------------------
# Evidence sensitivity = correlation between posterior probability of structure selected and confidence ratings
posteriorConfidenceCor <- data.frame("SubID" = 1:90, "postConfCorr" = 1:90) #initialize data frame
for (sub in c(1:length(unique(task_data$SubID)))){ #loop through subjects
  subjectList <- unique(task_data$SubID)
  subject <- subjectList[sub] #get subject ID
  subData <- task_data[which(task_data$SubID == subject),] #get subject data
  posteriorConfidenceCor$postConfCorr[sub] = cor(subData$postSelected, subData$ConfRating) #compute correlation between confidence and posterior of the structure they selected 
  posteriorConfidenceCor$SubID[sub] = as.character(subject)
}

#merge with data
tempData <- merge(task_data, posteriorConfidenceCor, by = c("SubID"), all = T)

#plot
posteriorCorAgePlot <- ggplot(tempData, aes(x = age, y = postConfCorr)) +
  geom_point(color = color1) +
  stat_smooth(color = color1, fill = color1, formula = y ~ poly(x,2), method = "glm")  +
  ylab("Evidence Sensitivity") +
  xlab("Age") +
  chip.theme
posteriorCorAgePlot


#### Evidence sensitivity by age  -------------------------------------
#scale and square age
tempData$ageScaledSquared <- (tempData$ageScaled)^2

#examine model with linear and quadratic age
confCor.age.lm <- lm(postConfCorr ~ ageScaled , data = tempData)
confCor.age.2.lm <- lm(postConfCorr ~ ageScaled + ageScaledSquared , data = tempData)

#compare them
anova(confCor.age.lm, confCor.age.2.lm)

#model wtih age squared fits better
summary(confCor.age.2.lm)
modelEffectSizes(confCor.age.2.lm)


#### Evidence sensitivity by age and IQ  -------------------------------------
# scaled IQ
tempData$IQScaled <- scale_this(tempData$IQ)

# add IQ into best-fitting model
confCor.age.IQ.lm <- lm(postConfCorr ~ ageScaledSquared *IQScaled + ageScaled * IQScaled, data = tempData)
summary(confCor.age.IQ.lm)
modelEffectSizes(confCor.age.IQ.lm)

#Everything is significant
confCor.age.IQ.plot <- sjPlot::plot_model(confCor.age.IQ.lm, type = "int", 
                                  colors = c(color1, color2))
confCor.age.IQ.plot 


#### Theta by evidence sensitivity ---------------------------------------------
#temp2 <- merge(task_data, posteriorConfidenceCor, by = c("SubID"), all = T)
tempData2 <- merge(subParams, posteriorConfidenceCor, by = c("SubID"), all = T)
tempData2$postConfCorrScaled <- scale_this(tempData2$postConfCorr)

#test age vs. age^2
theta.postConfCorr.lm <- lm(meanTheta ~ postConfCorrScaled *ageScaled, data = tempData2)
theta.postConfCorr.2.lm <- lm(meanTheta ~ (ageScaled + ageSquaredScaled) * postConfCorrScaled, data = tempData2)

#compare them
anova(theta.postConfCorr.lm, theta.postConfCorr.2.lm)
#model with just age fits bettter

#print output
summary(theta.postConfCorr.lm)
modelEffectSizes(theta.postConfCorr.lm)

#plot model to interpret interaction
plot_model(theta.postConfCorr.lm, type = "int", colors = c(color1, color2)) +
  chip.theme

#### Theta by evidence sensitivity and IQ ---------------------------------------------
tempData2$IQScaled <- scale_this(tempData2$IQ)

theta.postConfCorr.IQ.lm <- lm(meanTheta ~ ageScaled  * postConfCorrScaled *IQScaled, data = tempData2)
summary(theta.postConfCorr.IQ.lm)

#no effects of IQ or interactions


#### Within-task learning effects ####
#run model or load output
# source('scripts/fit_mixture_model_brms_expHalf.R')
load("processed_data/brms_mixtureModel_estimates_by_half.Rda")


#### Model diagnostics (by half)  ------------------------------------------
load("processed_data/brms_mixtureModel_fullModel_by_half_1.Rda")
model_data1 <- as.data.frame(summary(fit1)$fixed) %>%
  rownames_to_column(var = "rowname") %>%
  separate(rowname, into = c("parameter", "SubID"), sep = ("_as.factorSubID"))

tau_data1 <- model_data1 %>%
  filter(parameter == "tau")

theta_data1 <- model_data1 %>%
  filter(parameter == "theta")


load("processed_data/brms_mixtureModel_fullModel_by_half_2.Rda")
model_data2 <- as.data.frame(summary(fit2)$fixed) %>%
  rownames_to_column(var = "rowname") %>%
  separate(rowname, into = c("parameter", "SubID"), sep = ("_as.factorSubID"))

tau_data2 <- model_data2 %>%
  filter(parameter == "tau")

theta_data2 <- model_data2 %>%
  filter(parameter == "theta")

#merge and compute diagnoistics
tau_data_half <- rbind(tau_data1, tau_data2)
theta_data_half <- rbind(theta_data1, theta_data2)

tau_diagnostics_half <- tau_data_half %>%
  dplyr::summarize(maxRhat = max(Rhat),
                   minBulk = min(Bulk_ESS),
                   meanBulk = mean(Bulk_ESS),
                   minTail = min(Tail_ESS),
                   meanTail = mean(Tail_ESS))

theta_diagnostics_half <- theta_data_half %>%
  dplyr::summarize(maxRhat = max(Rhat),
                   minBulk = min(Bulk_ESS),
                   meanBulk = mean(Bulk_ESS),
                   minTail = min(Tail_ESS),
                   meanTail = mean(Tail_ESS))


# merge output with subInfo ------------------------------------
estimates %<>%
  dplyr::rename(sub = "SubID")

parsByHalf <- merge(estimates, subInfo, by = "sub") %>% 
  mutate(ageGroup = case_when(age < 13 ~ "Children", age < 18 & age > 12 ~ "Adolescents", age > 18 ~ "Adults")) 

parsByHalf$ageGroup <- factor(parsByHalf$ageGroup , levels = c("Children", "Adolescents", "Adults"))
parsByHalf$ageScaled <- scale_this(parsByHalf$age)
parsByHalf$ageSquared <- (parsByHalf$age)^2
parsByHalf$ageSquaredScaled <- (parsByHalf$ageScaled)^2
parsByHalf$expHalf <- as.factor(parsByHalf$expHalf)

#### Theta by experiment half ------------------------------------------------
expHalfAgeTheta.lmer.1 <- mixed(theta ~ ageScaled * expHalf + (1|sub), data = parsByHalf, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")
expHalfAgeTheta.lmer.2 <- mixed(theta ~ (ageScaled + ageSquaredScaled) * expHalf + (1|sub), data = parsByHalf, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")

anova(expHalfAgeTheta.lmer.1, expHalfAgeTheta.lmer.2)
#model with age^2 fits better

expHalfAgeTheta.2 <- mixed(theta ~ (ageScaled + ageSquaredScaled) * expHalf + (1|sub), data = parsByHalf, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)))
expHalfAgeTheta.2

#plot
thetaByHalfByAgePlot <- ggplot(parsByHalf, aes(x = age, y = theta, color = as.factor(expHalf), fill = as.factor(expHalf))) +
  geom_point() +
  stat_smooth(formula = y ~ poly(x,2), method = "glm") +
  scale_color_manual(values = c(color1, color2), name = "Experiment Half") + 
  scale_fill_manual(values = c(color1, color2), name = "Experiment Half") +
  ylab("Theta Estimate") + xlab("Age") + 
  chip.theme +
  theme(legend.position = "top")
thetaByHalfByAgePlot
#ggsave("thetaByHalfByAgePlot.png", thetaByHalfByAgePlot, dpi = 1000)


#### Theta by experiment half and IQ ------------------------------------------------
parsByHalf$IQScaled <- scale_this(parsByHalf$IQ)

expHalfAgeTheta.IQ <- mixed(theta ~ (ageScaled + ageSquaredScaled) * expHalf *IQScaled + (1|sub), data = parsByHalf, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)))
expHalfAgeTheta.IQ 

#No effect of IQ or interactions



#### Tau by experiment half ----------------------------------------------------------------
expHalfAgeTau.lmer.1 <- mixed(tau ~ ageScaled * expHalf + (1|sub), data = parsByHalf, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")
expHalfAgeTau.lmer.2 <- mixed(tau ~ (ageScaled + ageSquaredScaled) * expHalf + (1|sub), data = parsByHalf, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)), return = "merMod")

anova(expHalfAgeTau.lmer.1, expHalfAgeTau.lmer.2)
#linear model fits best

expHalfAgeTau.1 <- mixed(tau ~ ageScaled * expHalf + (1|sub), data = parsByHalf, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)))
expHalfAgeTau.1


tauByHalfByAgePlot <- ggplot(parsByHalf, aes(x = age, y = tau, color = as.factor(expHalf), fill = as.factor(expHalf))) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "glm") +
  scale_color_manual(values = c(color1, color2), name = "Experiment Half") + 
  scale_fill_manual(values = c(color1, color2), name = "Experiment Half") +
  ylab("Tau Estimate") + xlab("Age") + 
  chip.theme +
  theme(legend.position = "top")
tauByHalfByAgePlot
#ggsave("tauByHalfByAgePlot.png", tauByHalfByAgePlot, dpi = 1000)



#### Tau by experiment half and IQ  ----------------------------------------------------------------
expHalfAgeTau.IQ <- mixed(tau ~ ageScaled * expHalf *IQScaled + (1|sub), data = parsByHalf, method = "KR", control = lmerControl(optCtrl=list(maxfun=1e6)))
expHalfAgeTau.IQ

#main effect of IQ, no interactions


#### Theta change by age  ----------------------------------------------------------------
temp <- parsByHalf %>% 
  select(-tau) %>%
  spread(expHalf, theta) %>% 
  mutate(thetaChange = `2` - `1`) %>%
  dplyr::rename(SubID = sub) %>%
  select(thetaChange, SubID)

tempData3 <- full_join(temp, tempData2, by = c("SubID"))

tempData3$ageScaled <- scale_this(tempData3$age)
tempData3$ageSquaredScaled <- tempData3$ageScaled^2

thetaChangeByAge.lm.1 <- lm(thetaChange ~ ageScaled, data = tempData3)
thetaChangeByAge.lm.2 <- lm(thetaChange ~ (ageScaled + ageSquaredScaled), data = tempData3)

anova(thetaChangeByAge.lm.1, thetaChangeByAge.lm.2)
#model with just age fits better

summary(thetaChangeByAge.lm.1)
# no effect of age on theta change

#### Theta change by age and IQ  ----------------------------------------------------------------
tempData3$IQScaled <- scale_this(tempData3$IQ)
thetaChangeByAge.lm.IQ <- lm(thetaChange ~ ageScaled *IQScaled, data = tempData3)
summary(thetaChangeByAge.lm.IQ) 

#no effects of IQ or interactions


#### Theta change by evidence sensitivity  ----------------------------------------------------------------
tempData3$postConfCorrScaled <- scale_this(tempData3$postConfCorr)

postConfCorrThetaChange.lm.1 <- lm(thetaChange ~ ageScaled * postConfCorrScaled, data = tempData3)
postConfCorrThetaChange.lm.2 <- lm(thetaChange ~ (ageScaled + ageSquaredScaled) * postConfCorrScaled, data = tempData3)
anova(postConfCorrThetaChange.lmer.1, postConfCorrThetaChange.lmer.2)
#model with just age fits better

postConfCorrThetaChange.lm.1 <- lm(thetaChange ~ ageScaled * postConfCorrScaled, data = tempData3)
summary(postConfCorrThetaChange.lm.1)
modelEffectSizes(postConfCorrThetaChange.lmer.1)
#main effect of evidence sensitivity

thetaChangeByEvidenceSensitivity <- ggplot(tempData3, aes(x = postConfCorr, y = thetaChange)) +
  geom_point(color = color1) +
  stat_smooth(formula = y ~ x, method = "lm", fill = color1, color = color1) +
  ylab("Theta Change") + xlab("Evidence Sensitivity") + 
  chip.theme
thetaChangeByEvidenceSensitivity

#### Theta change by evidence sensitivity and IQ  ----------------------------------------------------------------
postConfCorrThetaChange.IQ <- lm(thetaChange ~ ageScaled * postConfCorrScaled *IQScaled, data = tempData3)
summary(postConfCorrThetaChange.IQ)
modelEffectSizes(postConfCorrThetaChange.IQ)

#still main effect of evidence sensitivity, no effects of IQ or interactions
