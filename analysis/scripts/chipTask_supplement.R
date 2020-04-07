## Chip Task - Supplemental Analyses  ##
# Last updated: 4/7/20
# katenuss@nyu.edu

#### CONFIDENCE ANALYSES IN SUPPLEMENT ####
# Supplemental IQ analyses are in main analysis file

## LOAD LIBRARIES ##
library(tidyverse)
library(magrittr)
library(glue)
library(pander)
library(afex)
library(lmSupport)
library(sjPlot)

# Load functions to convert matlab matrices into R format
source("scripts/convertMatlabMatrices.R")

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



#### Confidence ratings by age group
conf_sub_means <- task_data %>% 
  dplyr::group_by(SubID, ageGroup, age) %>%
  dplyr::summarize(min = min(ConfRating), 
                   max = max(ConfRating), 
                   mean = mean(ConfRating),
                   median = median(ConfRating),
                   sd = sd(ConfRating, na.rm=T),
                   range = (max - min))

#how does age predict minimum, maximum, mean, median, sd, range
conf_sub_means$ageScaled <- scale_this(conf_sub_means$age)

# confidence regressions
#min
minConf.age.model <- lm(min ~ ageScaled, data = conf_sub_means)
summary(minConf.age.model)

#max
maxConf.age.model <- lm(max ~ ageScaled, data = conf_sub_means)
summary(maxConf.age.model)

#mean
meanConf.age.model <- lm(mean ~ ageScaled, data = conf_sub_means)
summary(meanConf.age.model)

#median
medianConf.age.model <- lm(median ~ ageScaled, data = conf_sub_means)
summary(medianConf.age.model)

#sd
sdConf.age.model <- lm(sd ~ ageScaled, data = conf_sub_means)
summary(sdConf.age.model)

#range
rangeConf.age.model <- lm(range ~ ageScaled, data = conf_sub_means)
summary(rangeConf.age.model)


#box plots
conf_sub_means2 <- pivot_longer(conf_sub_means, 
                                cols = c("min", 
                                         "max",
                                         "mean", 
                                         "median",
                                         "sd",
                                         "range"))



confBoxPlots <- ggplot(conf_sub_means2, aes(x = ageGroup, y = value, fill = ageGroup)) +
  geom_violin(draw_quantiles = T) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, color = "white") +
  scale_fill_manual(values = c(color1, color2, color3)) +
  facet_wrap(~ name) +
  ylab(" ") +
  xlab("Age Group") +
  chip.theme +
  theme(legend.position = "none")
confBoxPlots 
ggsave("confidence_stats.png", confBoxPlots, width = 14, height = 8, units = "in", dpi = 600)


conf_age_means <- conf_sub_means %>%
  dplyr::group_by(ageGroup) %>%
  dplyr::summarize(minConf = mean(minConf), 
                   maxConf = mean(maxConf), 
                   meanConf = mean(meanConf), 
                   medianConf = mean(medianConf),
                   sdConf = mean(sdConf),
                   rangeConf = mean(rangeConf))
