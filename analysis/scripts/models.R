source("helper_files/print.lib.R")
source("helper_files/lib.R")
source("helper_files/plib.R")
source("helper_files/cgm.R")


#------------------------------------
# EIG and PTS models
#------------------------------------

# general parameters
m <- .8  # causal strength
c <- 0   # root node strength
b <- 0   # background causes



# NOTE: arrays have weird conventions so taking the transpose is clearest
# rows parent, columns child
#  xx  xy  xz
#  yx  yy  yz
#  zx  zy  zz

# example 1 from Coenen et al
if (0) {
    sample_network1 <- t(array(c(0, 0, 0,
                                 1, 0, 1,
                                 0, 0, 0),
                               dim=c(3,3)))
    sample_network2 <- t(array(c(0, 1, 1,
                                 0, 0, 0,
                                 0, 0, 0),
                               dim=c(3,3)))
}
# example 2 from Coenen et al
if (0) {
    sample_network1 <- t(array(c(0, 1, 0,
                                 0, 0, 1,
                                 0, 0, 0),
                               dim=c(3,3)))
    sample_network2 <- t(array(c(0, 0, 0,
                                 0, 0, 0,
                                 1, 0, 0),
                               dim=c(3,3)))
}
# kate's example that distinguishes between mean and max of PTS
if (1) {
  sample_network1 <- t(array(c(0, 0, 0,
                               1, 0, 0,
                               1, 0, 0),
                             dim=c(3,3)))
  sample_network2 <- t(array(c(0, 0, 0,
                               1, 0, 1,
                               0, 0, 0),
                             dim=c(3,3)))
}
if (1) {
  sample_network1 <- t(array(c(0, 1, 1,
                               0, 0, 0,
                               0, 0, 0),
                             dim=c(3,3)))
  sample_network2 <- t(array(c(0, 1, 1,
                               0, 0, 0,
                               0, 1, 0),
                             dim=c(3,3)))
}
vars <- c('x','y','z')
rownames(sample_network1) <- vars
colnames(sample_network1) <- vars
rownames(sample_network2) <- vars
colnames(sample_network2) <- vars

#---------------------
# PTS
#---------------------
PTS_CD <- function(network1, network2) {
  n <- nrow(network1)
  # storing the number of downstream nodes for each variable, for each network
  store <- array(rep(0, 2*n), dim=c(2,n))
  
  networks <- list(network1=network1,
                   network2=network2)
  
  # horrific code, should be done recursively. But basically what it does is finds the children
  # of one variable, say x1, and sums up all of the direct connections. Then, for its children,
  # it counts their children's children, etc.
  for (net_id in 1:length(networks)) {
    for (e in 1:n) {
      # doing tmp so that we don't double count some variables (set to 0 after we count them)
      tmp <- networks[[net_id]]
      store[net_id,e] <- store[net_id,e] + sum(tmp[e,] != 0)
      
      for (e2 in which(networks[[net_id]][e,] != 0)) {
        store[net_id,e] <- store[net_id,e] + sum(tmp[e2,] != 0)
        
        for (e3 in which(networks[[net_id]][e2,] != 0)) {
          store[net_id,e] <- store[net_id,e] + sum(tmp[e3,] != 0)
          
          for (e4 in which(networks[[net_id]][e3,] != 0)) {
            store[net_id,e] <- store[net_id,e] + sum(tmp[e4,] != 0)
            
          }
          tmp[e3,e4] <- 0
        }
        tmp[e2,e3] <- 0
      }
      tmp[e,e2] <- 0
    }
  }
  
  for (e in 1:length(networks)) {
    store[e,] <- store[e,] / sum(networks[[e]] != 0)
  }
  store2 <- apply(store,2,max)
  return(store2/sum(store2))
}
#print(PTS_CD(network1=DAGs_4.a[,,37],network2=DAGs_4.a[,,38]))
#print(PTS_CD(network1=sample_network1,network2=sample_network2))


#------------------------------------------------------------------------------
#### EIG
#------------------------------------------------------------------------------
# For a given network and intervention, computes joint distribution
# Compares distance between the two 

# an intervention sets the value of one of the nodes and cancels out all incoming links

# when intervened on, first set the value of b to 1 in that case (and 0 elsewhere)
# then, fuck up the graph by removing incoming links to the intervened on variables
# now recompute with the fucked up graph and new bs

# network is an array; intervention is either 'x','y','z'; m is causal strength
IG_CD <- function(network, intervention, m=.8) {
    # number of variables
    n <- nrow(network)
    # generates the exogenous probabilities of each node (0 unless intervened on)
    bs <- rep(0,n)
    bs[intervention] <- 1
    
    # this removes all links into the intervened on network
    network[,intervention] <- rep(0,n)
    rownames (network) <- sapply (1:n, function (i) paste ('x', i, sep=''))
    
    # return the joint distribution
    return(joint.cgm.generic(ms=network*m, bs=bs))
}
#IG_CD(network=DAGs_3[,,6], intervention=1)



# EIG(a) = H(G) - sum(P(o|a) H(G|a,o))

# P(o|a) is the probability of an outcome given the action. I suppose this means marginalized over graphs (e.g. just take the mean at each point for the 2 graphs). Yep, see below
# H(G|a,o) is the uncertainty over graphs, given action and outcome. It can be decomposed to:
    # H(G|a,o) = sum(P(g|a,o) log_2(1/p(g|a,o)))
    # where
        # p(g|a,o) = P(o|g,a)*P(g)/P(o|a)
        # where
            # P(o|a) can be computed by marginalizing over all possible graphs
EIG_CD <- function(network1, network2, m=.8) {
    prior <- c(0.5, 0.5) # for now equal priors
    
    IG_vec <- rep(0,nrow(network1))
    for (int in 1:nrow(network1)) {
        # getting P(o|g,a)
        tmp1 <- IG_CD(network1, int, m)
        tmp2 <- IG_CD(network2, int, m)
        
        # P(o|a) is marginalizing over graphs
        o_a <- (tmp1$p + tmp2$p) / 2
        
        g1_ao <- (tmp1$p * prior[1]) / o_a
        g2_ao <- (tmp2$p * prior[2]) / o_a
        
        # H(G|a,o) is the sum over graphs
        H_G_ao <- g1_ao*(log(g1_ao, base=2)) +
                  g2_ao*(log(g2_ao, base=2))
        #print(cbind(tmp1,tmp2$p,g1_ao,g2_ao,H_G_ao))
        # EIG(a) is the sum over outcomes
        expected_info <- sum(o_a * H_G_ao, na.rm=TRUE)
        
        IG_vec[int] <- expected_info
        
        # need to define H_G
        # well actually H_G (initial uncertainty) is gonna be the same no matter what intervention
        # we make so actually don't need to compute it
    }
    # prior uncertainty
    H_G <- sum(prior * log(prior,base=2))
    
    # EIG = H_G - expected_info
    EIG <- H_G - IG_vec
    
    EIG <- EIG/sum(EIG)
    return(EIG)
}
#EIG_CD(DAGs_4[,,6],DAGs_4[,,10])














#---------------------
# PTS mean (wrong model)
#---------------------
PTS_CD_mean <- function(network1, network2) {
  n <- nrow(network1)
  # storing the number of downstream nodes for each variable, for each network
  store <- array(rep(0, 2*n), dim=c(2,n))
  
  networks <- list(network1=network1,
                   network2=network2)
  
  # horrific code, should be done recursively. But basically what it does is finds the children
  # of one variable, say x1, and sums up all of the direct connections. Then, for its children,
  # it counts their children's children, etc.
  for (net_id in 1:length(networks)) {
    for (e in 1:n) {
      # doing tmp so that we don't double count some variables (set to 0 after we count them)
      tmp <- networks[[net_id]]
      store[net_id,e] <- store[net_id,e] + sum(tmp[e,] != 0)
      
      for (e2 in which(networks[[net_id]][e,] != 0)) {
        store[net_id,e] <- store[net_id,e] + sum(tmp[e2,] != 0)
        
        for (e3 in which(networks[[net_id]][e2,] != 0)) {
          store[net_id,e] <- store[net_id,e] + sum(tmp[e3,] != 0)
          
          for (e4 in which(networks[[net_id]][e3,] != 0)) {
            store[net_id,e] <- store[net_id,e] + sum(tmp[e4,] != 0)
            
          }
          tmp[e3,e4] <- 0
        }
        tmp[e2,e3] <- 0
      }
      tmp[e,e2] <- 0
    }
  }
  for (e in 1:length(networks)) {
    store[e,] <- store[e,] / sum(networks[[e]] != 0)
  }
  store2 <- apply(store,2,mean)
  return(store2/sum(store2))
}






