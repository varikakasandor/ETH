# Network Modeling - HS 2022
# C. Stadtfeld, A. Espinosa-Rada, A. Uzaheta
# Based on previous version from: K. Mepham, V. Amati
# Social Networks Lab
# Department of Humanities, Social and Political Sciences
# ETH Zurich
# 24 October 2022
#
# Assignment 2 - Task 2

# install.packages(c("sna","network","igraph", "ggraph"))

library(sna)
library(network)
library(ergm) # take a wild guess...
library(igraph) # for data objects and plotting
library(ggraph) # for plotting


# MHstep ------------------------------------------------------------------
#' Simulate the next step of a network in Markov chain using Metropolis-Hasting
#' 
#' The function `MHstep` simulates the the Metropolis-Hastings step that defines
#' the Markov chain whose stationary distribution is the ERGM with 
#' edge, mutual and nodematch statistics
#'
#' @param net an object of class `matrix`. Adjacency matrix of the network.
#' @param nodeAttr a character or numeric vector. The node attribute. 
#' @param theta1 a numeric value. The value of the edge parameter of the ERGM.
#' @param theta2 a numeric value. The value of the mutual parameter of the ERGM.
#' @param theta3 a numeric value. The value of the nodematch parameter of the ERGM.
#'
#' @return next state of the Markov Chain
#'
#' @examples
#' MHstep(
#'   matrix(c(0, 1, 0, 0, 0, 0, 1, 1, 0), nrow = 3, ncol = 3),
#'   c("v", "g", "g"),
#'   -log(0.5), log(0.4), log(.8)
#' )
MHstep <- function(net, nodeAttr, theta1, theta2, theta3){
  
  # Number of vertices in the network
  nvertices <- nrow(net) 
  
  # Choose randomly two vertices, prevent loops {i,i} with replace = FALSE
  tie <- sample(1:nvertices, 2, replace = FALSE) 
  i <- tie[1]
  j <- tie[2]
  
  # Compute the change statistics
  
  change_statistics_base <- 0
  if(net[i, j] == 0){
    change_statistics_base <- change_statistics_base + theta1 * 1
    if(net[j, i] == 1){
      change_statistics_base <- change_statistics_base + theta2 * 1
    }
    if(nodeAttr[i] == nodeAttr[j]){
      change_statistics_base <- change_statistics_base + theta3 * 1
    }
  }
  else{
    change_statistics_base <- change_statistics_base + theta1 * (-1)
    if(net[j, i] == 0){
      change_statistics_base <- change_statistics_base + theta2 * (-1)
    }
    if(nodeAttr[i] == nodeAttr[j]){
      change_statistics_base <- change_statistics_base + theta3 * (-1)
    }
  }

  change_statistics <- exp(change_statistics_base)
  
  
  # Compute the probability of the next state 
  # according to the Metropolis-Hastings algorithm
  
  prob_move <- min(1, change_statistics)
  
  # Select the next state: 

  if(runif(1) < prob_move){
    net[i, j] <- !net[i, j]
  }
  
  # Return the next state of the chain
  return(net)
}


calculate_statistics <- function(curr_net, nvertices, nodeAttr){
  num_edges <- sum(curr_net)
  num_reciprocal_dyads <- 0
  for(i in 1:(nvertices - 1)){
    for(j in 1:nvertices){
      if(curr_net[i, j] == 1 && curr_net[j, i] == 1){
        num_reciprocal_dyads <- num_reciprocal_dyads + 1
      }
    }
  }
  num_homophily_dyads <- 0
  for(i in 1:nvertices){
    for(j in 1:nvertices){
      if(i != j && curr_net[i, j] == 1 && nodeAttr[i] == nodeAttr[j]){
        num_homophily_dyads <- num_homophily_dyads + 1
      }
    }
  }
  return(c(num_edges, num_reciprocal_dyads, num_homophily_dyads))
}


# Markov Chain simulation -------------------------------------------------
#' The function MarkovChain simulates the networks from the ERGM with 
#' edge, mutual and nodematch statistics
#'
#' @param net an object of class `matrix`. Adjacency matrix of the network.
#' @param nodeAttr a character or numeric vector. The node attribute. 
#' @param theta1 a numeric value. The value of the edge parameter of the ERGM.
#' @param theta2 a numeric value. The value of the mutual parameter of the ERGM.
#' @param theta3 a numeric value. The value of the nodematch parameter of the ERGM.
#' @param burnin an integer value.
#'   Number of steps to reach the stationary distribution.
#' @param thinning an integer value. Number of steps between simulated networks.
#' @param nNet an integer value. Number of simulated networks to return as output.
#'
#' @return a named list:
#'   - netSim: an `array` with the adjancency matrices of the simulated networks.
#'   - statSim: a `matrix` with the value of the statistic defining the ERGM.
#'
#' @examples
#' MarkovChain(
#'   matrix(c(0, 1, 0, 0, 0, 0, 1, 1, 0), nrow = 3, ncol = 3),
#'   c("v", "g", "g"),
#'   -log(0.5), log(0.4), log(.8)
#' )
MarkovChain <- function(net, nodeAttr, theta1, theta2, theta3,
                        burnin = 10000, thinning = 1000, nNet = 1000){
  
  # Burnin phase: repeating the steps of the chain "burnin" times  
  nvertices <- nrow(net)
  burninStep <- 1 # counter for the number of burnin steps
  
  # Perform the burnin steps
  for (i in 1:burnin) {
    net <- MHstep(net, nodeAttr, theta1, theta2, theta3)
  }
  
  # After the burnin phase we draw the networks
  # The simulated networks and statistics are stored in the objects
  # netSim and statSim
  netSim <- array(0, dim = c(nvertices, nvertices, nNet))
  statSim <- matrix(0, nNet, 3)
  thinningSteps <- 0 # counter for the number of thinning steps
  netCounter <- 1 # counter for the number of simulated network
  
  netSim[, , netCounter] = net
  statSim[netCounter, ] = (calculate_statistics)(net, nvertices, nodeAttr)
  
  
  while(netCounter < nNet){
    net <- MHstep(net, nodeAttr, theta1, theta2, theta3)
    if(thinningSteps == thinning - 1){ # alternatively, == thinning, depending on interpretation
      netSim[, , netCounter] = net
      statSim[netCounter, ] = (calculate_statistics)(net, nvertices, nodeAttr)
      netCounter <- netCounter + 1
      thinningSteps <- 0
    }
    else{
      thinningSteps <- thinningSteps + 1
    }
  }

  # Return the simulated networks and the statistics
  return(list(netSim = netSim, statSim = statSim))
}


test_thetas <- function(theta1, theta2, theta3, burnin = 10000, thinning = 1000, nNet = 1000){
  
  load(file='friend_net.Rda')
  obs_net <- as.matrix(friend_net)
  obs_attr = get.node.attr(friend_net, 'sex')
  nvertices <- length(obs_attr)
  start_net = matrix(0, nvertices, nvertices)
  tmp <- MarkovChain(start_net, obs_attr, theta1, theta2, theta3, burnin, thinning, nNet)
  netSim <- tmp[["netSim"]]
  statSim <- tmp[["statSim"]]
  obs_stats <- (calculate_statistics)(obs_net, nvertices, obs_attr)
  print(obs_stats)
  for(i in 1:3){
    feature_list = statSim[, i]
    num_smaller = length(feature_list[feature_list < obs_stats[i]])
    num_larger = length(feature_list[feature_list > obs_stats[i]])
    two_sided_p_value = min(num_smaller, num_larger) / length(feature_list)
    cat(two_sided_p_value, num_smaller, num_larger, '\n')
  }
}

test_thetas(-2.76, 0.68, 1.21)
test_thetas(-2.76, 0.78, 1.21)

