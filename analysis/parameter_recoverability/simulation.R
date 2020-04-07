## Simulate fake participants
# 2/29/20

#libraries
library(tidyverse)

#load problem list
load("problem_list.Rda")


# Get list of problems
problems <- stanDataFirst40 %>%
  select(pairNumber, 
         struc1Number, 
         struc2Number, 
         PTS1, 
         PTS2,
         PTS3,
         PTS4,
         EIG1, 
         EIG2, 
         EIG3,
         EIG4,
         NodeCondition)

#initialize choice matrix
col_names <- c("theta", "tau", "sub_num", "problem_num", "choice", 
               "EIG1", "EIG2", "EIG3", "EIG4",
               "PTS1", "PTS2", "PTS3", "PTS4", "node_condition")
choice_df = data.frame(matrix(ncol = 14, nrow = 60000))
colnames(choice_df) <- col_names

# Simulate choice on every problem for parameter combination
sim_number = 0
row_number = 0
for (theta in c(.1, .3, .5, .7, .9)){
  for (tau in c(.1, .2, .3, .5, 1, 3)){
      for (sub in c(1:50)){
        sim_number = sim_number + 1
        for (problem_number in c(1:40)){
        row_number = row_number + 1
        node_condition = problems$NodeCondition[problem_number]
        if (node_condition == 3){
          val_1 = theta*problems$EIG1[problem_number] + (1-theta)*problems$PTS1[problem_number]
          val_2 = theta*problems$EIG2[problem_number] + (1-theta)*problems$PTS2[problem_number]
          val_3 = theta*problems$EIG3[problem_number] + (1-theta)*problems$PTS3[problem_number]
          choose1_prob = exp(val_1/tau)
          choose2_prob = exp(val_2/tau)
          choose3_prob = exp(val_3/tau)
          sum_probs = choose1_prob + choose2_prob + choose3_prob
          choose1 = choose1_prob/sum_probs
          choose2 = choose2_prob/sum_probs
          choose3 = choose3_prob/sum_probs
          choose4 = 0
        } else if (node_condition == 4){
          val_1 = theta*problems$EIG1[problem_number] + (1-theta)*problems$PTS1[problem_number]
          val_2 = theta*problems$EIG2[problem_number] + (1-theta)*problems$PTS2[problem_number]
          val_3 = theta*problems$EIG3[problem_number] + (1-theta)*problems$PTS3[problem_number]
          val_4 = theta*problems$EIG4[problem_number] + (1-theta)*problems$PTS4[problem_number]
          choose1_prob = exp(val_1/tau)
          choose2_prob = exp(val_2/tau)
          choose3_prob = exp(val_3/tau)
          choose4_prob = exp(val_4/tau)
          sum_probs = choose1_prob + choose2_prob + choose3_prob + choose4_prob
          choose1 = choose1_prob/sum_probs
          choose2 = choose2_prob/sum_probs
          choose3 = choose3_prob/sum_probs
          choose4 = choose4_prob/sum_probs}
        
        #generate random number between 0 and 1 to determine choice
        rand_num = runif(1, 0, 1) 
        if (rand_num > choose1 + choose2 + choose3){
          choice = 4
        } else if (rand_num > choose1 + choose2){
          choice = 3
        } else if (rand_num > choose1){
          choice = 2
        } else{
          choice = 1
        }
            
        #save choice
        choice_df$theta[row_number] = theta
        choice_df$tau[row_number] = tau
        choice_df$sub_num[row_number] = sub
        choice_df$sim_num[row_number] = sim_number
        choice_df$problem_num[row_number] = problem_number
        choice_df$choice[row_number] = choice
        choice_df$EIG1[row_number] = problems$EIG1[problem_number]
        choice_df$EIG2[row_number] = problems$EIG2[problem_number]
        choice_df$EIG3[row_number] = problems$EIG3[problem_number]
        choice_df$EIG4[row_number] = problems$EIG4[problem_number]
        choice_df$PTS1[row_number] = problems$PTS1[problem_number]
        choice_df$PTS2[row_number] = problems$PTS2[problem_number]
        choice_df$PTS3[row_number] = problems$PTS3[problem_number]
        choice_df$PTS4[row_number] = problems$PTS4[problem_number] 
        choice_df$node_condition[row_number] = node_condition
    
      }
    }
  }
}




#save simulated data
save(choice_df, file = "simulated_choices_march24.Rda")
#save(stanDataFirst40, file = "problem_list.Rda")
