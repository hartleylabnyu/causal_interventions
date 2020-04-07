## Chip Task Analysis - 1 - Process Data ##
# KN - 4/3/20

# This script reads in the raw data files, 
# a file with participant info (ages, IQ, etc), and 
# information about the causal structures presented. It merges the information
# and saves it as a dataframe (task_data) in "chip_task_processed_data.Rda".
# It also saves subInfo in "subInfo.Rda".

# Load libraries
library(tidyverse)
library(magrittr)
library(glue)
library(pander)
library(afex)
library(lmSupport)
library(sjPlot)

## Load functions, themes, etc. ##
#define new function so that scale returns a vector, not a matrix
scale_this <- function(x) as.vector(scale(x))


# Load functions to convert matlab matrices into R format
source("scripts/convertMatlabMatrices.R")

#### IMPORT INFORMATION ABOUT CAUSAL STRUCTURES PRESENTED ####
# structures
structures <- read_csv("stimFiles/strucPairs.csv")
structures %<>% filter(pairNumber > 0) #eliminate empty rows
threeNodeStrucs <- structures %>% 
  filter(nodeCondition == 3)
fourNodeStrucs <- structures %>% 
  filter(nodeCondition == 4)


# convert structures 1 and 2 to R format (3-node structures)
for (i in 1:nrow(threeNodeStrucs)){
  assign(glue("network{threeNodeStrucs$struc1Number[i]}"), mat2r_matrices3(threeNodeStrucs$struc1[i]))
}

for (i in 1:nrow(threeNodeStrucs)){
  assign(glue("network{threeNodeStrucs$struc2Number[i]}"), mat2r_matrices3(threeNodeStrucs$struc2[i]))
}

# convert structures 1 and 2 to R format (4-node structures)
for (i in 1:nrow(fourNodeStrucs)){
  assign(glue("network{fourNodeStrucs$struc1Number[i]}"), mat2r_matrices4(fourNodeStrucs$struc1[i]))
}

for (i in 1:nrow(fourNodeStrucs)){
  assign(glue("network{fourNodeStrucs$struc2Number[i]}"), mat2r_matrices4(fourNodeStrucs$struc2[i]))
}

#create new columns with EIG and PTS predictions
structures %<>% 
  mutate(PTS1 = NA, 
         PTS2 = NA, 
         PTS3 = NA, 
         PTS4 = NA, 
         EIG1 = NA, 
         EIG2 = NA, 
         EIG3 = NA,
         EIG4 = NA)

#split into two data frames - one for each node condition
threeNodeStrucPairs <- structures %>% 
  filter(nodeCondition == 3)
fourNodeStrucPairs <- structures %>% 
  filter(nodeCondition == 4)

# load models
source("scripts/models.R")

#create PTS and EIG predictions
for (i in 1:nrow(threeNodeStrucPairs)){
  strucNumber1 <- threeNodeStrucPairs$struc1Number[i]
  strucNumber2 <- threeNodeStrucPairs$struc2Number[i]
  structure1 <- eval(glue("network{strucNumber1}"))
  structure2 <- eval(glue("network{strucNumber2}"))
  PTS_pred <- PTS_CD(get(structure1), get(structure2))
  threeNodeStrucPairs$PTS1[i] <- PTS_pred[1]
  threeNodeStrucPairs$PTS2[i] <- PTS_pred[2]
  threeNodeStrucPairs$PTS3[i] <- PTS_pred[3]
  EIG_pred <- EIG_CD(get(structure1), get(structure2))
  threeNodeStrucPairs$EIG1[i] <- EIG_pred[1]
  threeNodeStrucPairs$EIG2[i] <- EIG_pred[2]
  threeNodeStrucPairs$EIG3[i] <- EIG_pred[3]
}

for (i in 1:nrow(fourNodeStrucPairs)){
  strucNumber1 <- fourNodeStrucPairs$struc1Number[i]
  strucNumber2 <- fourNodeStrucPairs$struc2Number[i]
  structure1 <- eval(glue("network{strucNumber1}"))
  structure2 <- eval(glue("network{strucNumber2}"))
  PTS_pred <- PTS_CD(get(structure1), get(structure2))
  fourNodeStrucPairs$PTS1[i] <- PTS_pred[1]
  fourNodeStrucPairs$PTS2[i] <- PTS_pred[2]
  fourNodeStrucPairs$PTS3[i] <- PTS_pred[3]
  fourNodeStrucPairs$PTS4[i] <- PTS_pred[4]
  EIG_pred <- EIG_CD(get(structure1), get(structure2))
  fourNodeStrucPairs$EIG1[i] <- EIG_pred[1]
  fourNodeStrucPairs$EIG2[i] <- EIG_pred[2]
  fourNodeStrucPairs$EIG3[i] <- EIG_pred[3]
  fourNodeStrucPairs$EIG4[i] <- EIG_pred[4]
}

#bind data frames
strucPairs <- rbind(threeNodeStrucPairs, fourNodeStrucPairs)

#### IMPORT SUBJECT INFORMATION ####
subInfo <- read.delim("anonymized_data/causes_subInfo.txt", header = TRUE, sep = "\t") %>%
  separate(sub, c("study", "sub"), "causes") %>% 
  select(-study)

#### IMPORT TASK DATA ####
data <- data.frame()

for (s in 1:nrow(subInfo)){
  sub <- subInfo$sub[s]
  filename <- glue("anonymized_data/{sub}_chipTask.txt")
  if (file.exists(filename)) {
    sub_data <- read.delim(filename, sep = "\t" , header = TRUE)
    sub_data$SubID <- as.factor(sub_data$SubID)
    sub_data$age <- subInfo$age[s]
    sub_data$IQ <- subInfo$IQ[s]
    try(sub_data %<>% select(-X), silent = T) #get rid of random column
    data <- rbind(data, sub_data)
  }}

#### PROCESS TASK DATA ####
#eliminate practice trial
data %<>% 
  filter(TrialNum < 100)

#add age group
data %<>% 
  mutate(ageGroup = case_when(age >= 18 ~ "Adults", 
                              age < 18 & age >= 13 ~ "Adolescents", 
                              age < 13 ~ "Children"))

#reorder age group factor levels
data$ageGroup <- factor(data$ageGroup, levels = c("Children", "Adolescents", "Adults"))

#assign pair numbers to images presented 
data$pairNumber <- NA
for (i in 1:nrow(data)){
  for (k in 1:nrow(strucPairs)){
    if (data$TrueStrucNum[i] == strucPairs$struc1Number[k] & data$FoilStrucNum[i] == strucPairs$struc2Number[k]){
      data$pairNumber[i] = strucPairs$pairNumber[k]
    }
    if (data$TrueStrucNum[i] == strucPairs$struc2Number[k] & data$FoilStrucNum[i] == strucPairs$struc1Number[k]){
      data$pairNumber[i] = strucPairs$pairNumber[k]
    }
  }
}

#merge data with strucPairs data frame
temp <- merge(data, strucPairs, by = c("pairNumber"), all = TRUE )
data <- temp

# Add info that indicates nodes that were turned on after the intervention
configCodes3 <- read_csv("stimFiles/configCodes.csv") %>% 
  mutate(node4 = as.integer(NA))
configCodes4 <- read_csv("stimFiles/configCodes4.csv")
configCodes <- rbind(configCodes3, configCodes4) 
configCodes %<>% select(-nodeClicked) %>% 
  dplyr::rename(node1outcome = node1,
                node2outcome = node2, 
                node3outcome = node3,
                node4outcome = node4, 
                OutcomeImg = filename)

#convert outcome image to character
data$OutcomeImg <- as.character(data$OutcomeImg)

#remove stupid space in front of name
data$OutcomeImg <- str_replace_all(data$OutcomeImg, fixed(" "), "")

#merge with task data
temp <- merge(data, configCodes, by = c("OutcomeImg"), all.x = TRUE)

#arrange data for further processing 
task_data <- temp
task_data %<>% arrange(SubID, pairNumber)
numSubs <- length(unique(task_data$SubID))

#save task data
save(task_data, file = "chip_task_data_processed.Rda")

#save subInfo
save(subInfo, file = "subInfo.Rda")



