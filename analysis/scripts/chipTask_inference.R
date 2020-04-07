#### Chip Task Inference
# This script computes the posterior probability of each structure given the intervention
# and observed evidence.

# load helper functions
source("scripts/convertMatlabMatrices.R")
source("scripts/models.R")

# Function to compute the posterior probability of structure based on intervention and outcome
inference_CD <- function(network1, network2, observation, nodeClick, nodeCondition, m=.8) {
    prior <- c(0.5, 0.5) # for now equal priors
    
    IG_vec <- rep(0,nrow(network1))
    
    # getting P(o|g,a)
    tmp1 <- IG_CD(network1, nodeClick, m)
    tmp2 <- IG_CD(network2, nodeClick, m)
    
    # P(o|a) is marginalizing over graphs
    o_a <- (tmp1$p + tmp2$p) / 2
    
    # multiplying by prior and normalizing to get posterior
    g1_ao <- (tmp1$p * prior[1]) / o_a
    g2_ao <- (tmp2$p * prior[2]) / o_a
    
    # posterior probability of the observation
    if (nodeCondition==3) {
        idx <- which(tmp2$x1==observation[1] & 
                         tmp2$x2==observation[2] & 
                         tmp2$x3==observation[3])
    } else if (nodeCondition==4) {
        idx <- which(tmp2$x1==observation[1] & 
                         tmp2$x2==observation[2] & 
                         tmp2$x3==observation[3] & 
                         tmp2$x4==observation[4])
    }
    obs_post1 <- g1_ao[idx]
    obs_post2 <- g2_ao[idx]
    
    post <- c(struc1_post=obs_post1,
              struc2_post=obs_post2)
    return(post/sum(post))
}

# Run function on data

task_data$struc1p <- rep(NaN, nrow(task_data))
task_data$struc2p <- rep(NaN, nrow(task_data))
for (e in 1:nrow(task_data)) {
    # whether 3 or 4 nodes
    nodeCondition <- task_data[e,'nodeCondition']
    
    # the two networks they got
    if (nodeCondition == 4) {
        struc1 <- mat2r_matrices4(task_data[e,'struc1'])
        struc2 <- mat2r_matrices4(task_data[e,'struc2'])
    } else if (nodeCondition == 3) {
        struc1 <- mat2r_matrices3(task_data[e,'struc1'])
        struc2 <- mat2r_matrices3(task_data[e,'struc2'])
    }
    
    # which node they clicked
    nodeClick <- task_data[e,'NodeClick']
    # the outcome they saw
    observation <- c(task_data[e,'node1outcome'],task_data[e,'node2outcome'],task_data[e,'node3outcome'],task_data[e,'node4outcome'])
    
    # the posterior probability of each of the two structures
    post <- inference_CD(network1=struc1, 
                         network2=struc2, 
                         observation=observation, 
                         nodeClick=nodeClick, 
                         nodeCondition=nodeCondition)
    task_data[e,'struc1p'] <- post[1]
    task_data[e,'struc2p'] <- post[2]
}

