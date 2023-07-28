#### Main script for project with Guga on folded opinion, EXP4
#### In this version we avoid latitude of acceptance and diversified memorizing of info.
#### Despite the Folding point is given by issue, let's just look
#### what happens when we employ some individual variability into it.

## Encoding: windows-1250
## Created:  2023-06-29 Fran»esko
## Edited:   2023-07-27 Fran»esko

## NOTES:
## 1) What vars we do need for agents?
##    ID
##    folding point
##    infoPos
##    infoNeg
##    attention
##    opinion
##


# head --------------------------------------------------------------------

# clearing environment
rm(list = ls())

# Packages
library(tidyverse)
library(scatterplot3d)
library(rgl)
library(cusp)



# matrix with agents ------------------------------------------------------

createPublic =
  function(opDistribution = "Black Pete", N = 1000,
           foldingPoint = 0.05, folding_sd = 0.1,
           attentionDenom = sqrt(2)) {
  # Needed constants
  N = N  # number of agents
  v = 6  # number of agents' variables/columns
  foldingPoint = foldingPoint  # Average folding point
  folding_sd = folding_sd  # SD of average folding point
  # forgeting = forgeting  # Rate of information kept in info silos
  attentionDenom = attentionDenom  # Denominator for calculating Attention

  ## Constants for "Black Pete" scenario
  # # Let's hadwire it for Black Pete...
  majFrac = 0.90  # Fraction of the majority
  majPosMin = .05  # Minimum of positive information of majority
  majPosMax = .15  # Maximum of positive information of majority
  majNegMin = .05  # Minimum of negative information of majority
  majNegMax = .15  # Maximum of negative information of majority
  minPosMin = .05  # Minimum of positive information of minority
  minPosMax = .15  # Maximum of positive information of minority
  minNegMin = .80  # Minimum of negative information of minority
  minNegMax = 1.0  # Maximum of negative information of minority
  majInOpAv = 0.00   # Average initial opinion of majority
  majInOpSD = 0.01   # SD of initial opinion of majority
  minInOpAv = -0.8   # Average initial opinion of minority
  minInOpSD = 0.15   # SD of initial opinion of minority


  # init of empty matrix
  am = matrix(data = rep(1:N, times = v, N * v),
              ncol = v, byrow = F)  # We initialize whole matrix by IDs in every column.
  colnames(am) = c("ID", "folding", "infoPos", "infoNeg", "attention", "opinion")

  # Creating values in vars 2:6
  for (i in 1:N) {
    # Acceptance
    x = rnorm(n = 1, mean = foldingPoint, sd = folding_sd)
    while (x <= 0 | x >= 1) {
      x = rnorm(n = 1, mean = foldingPoint, sd = folding_sd)
    }
    am[i, "folding"] = x

    # Information
    am[i, "infoPos"] = if_else(
      i < N * majFrac,
      runif(n = 1, min = majPosMin, max = majPosMax),
      runif(n = 1, min = minPosMin, max = minPosMax)
    )
    if (am[i, "infoPos"] > 1) am[i, "infoPos"] = 1
    am[i, "infoNeg"] = if_else(
      i < N * majFrac,
      runif(n = 1, min = majNegMin, max = majNegMax),
      runif(n = 1, min = minNegMin, max = minNegMax)
    )
    if (am[i, "infoNeg"] > 1) am[i, "infoNeg"] = 1

    # Attention
    am[i, "attention"] = (am[i, "infoPos"] + am[i, "infoNeg"]) / attentionDenom
    if (am[i, "attention"] > 1) am[i, "attention"] = 1

    # Opinion
    # Initial random opinion
    if (i < N * majFrac) {
      x = rnorm(n = 1, mean = majInOpAv, sd = majInOpSD)
      while (x < -1 | x > 1) {
        x = rnorm(n = 1, mean = majInOpAv, sd = majInOpSD)
      }
      am[i, "opinion"] = x
    } else {
      x = rnorm(n = 1, mean = minInOpAv, sd = minInOpSD)
      while (x < -1 | x > 1) {
        x = rnorm(n = 1, mean = minInOpAv, sd = minInOpSD)
      }
      am[i, "opinion"] = x
    }
  }
  # Initially processed opinion
  am[, "opinion"] = -am[, "opinion"] ^ 3 + (am[, "infoPos"] + am[, "infoNeg"] - am[, "folding"]) * (am[, "opinion"]) + (am[, "infoPos"] - am[, "infoNeg"])
  am[am[, "opinion"] > 1, "opinion"] = 1
  am[am[, "opinion"] < -1, "opinion"] = -1
  return(am)
}



# communication matrix ----------------------------------------------------

# Note: this will be very probably completely rewritten, but for now...
# For now we create directed network, i.e. A can initiate conversation
# with B, but B can't do the same with A.

createMatrix = function(NN = 1000, neis = 6.1, nsd = 1.2) {
  # Again, constants first
  # neis = neis  # Average number of directed neighbors
  # nsd = nsd  # SD of number of directed neighbors
  # N = N  # Number of agents

  # Creation of vector 'V' of IDs, where the number of IDs represents
  # how many connections the respective agent will have:
  V = map(1:NN, ~ rep(.x, ceiling(rnorm(1, neis, nsd)))) %>% unlist()

  # And now we can go through the vector and
  # randomly draw for the ID its comm. partner:
  C = map(V, ~ sample(setdiff(1:NN, .x), size = 1)) %>% unlist()

  # Now using this vector for initialization of matrix of directed links:
  return(matrix(data = c(V, C), ncol = 2, byrow = F))
}



# agents rules ------------------------------------------------------------

## Round
# select initiative agents
# select partners for initiative agents
# for each communicating pair: exchange information
# for each agent from communicating pair immediately:
#          update information sila,
#          attention and
#          opinion of both agents

# Now we build function for a round, which takes comm. matrix, agent matrix and
# gives us updated agent matrix.
# Parameters:
#   agm = agent matrix
#   cmm = communication matrix
#   pia = fraction of agents selected for initializing communication
  simRound = function(agm = agm, cmm = cmm, forgeting = 0.5, pia = 0.5, bootstrap = T, reinforce = T) {
    # Let's start with defining needed constants:
    NN = nrow(agm)

    # select initiative agents
    ia = sample(1:NN, size = ceiling(pia * NN),
                replace = bootstrap)
    # Note: if 'bootstrap' == T, some agents might be selected more than once!

    ## Let's do the rest inside FOR cycle:
    for (i in ia) {
      # select partners for initiative agents and store their copies
      p = sample(cmm[cmm[, 1] == i, 2], size = 1)
      a1 = agm[i,]  # NOTE-BEWARE!!! 'a1' becomes vector, not matrix! Same a2...
      a2 = agm[p,]

      ## for each communicating pair: exchange information and update info sila
      ## NOTE: In this version I prepared only "buying opinion" possibility,
      ## just in case of need of further extension by possibility of direct
      ## exchange of information I let the buying of opinion embeded in
      ## if() function. So double-czech that 'buyingOpinion = T':
         ### Block for accomodating InfoBias according others opinion:
        ## Firstly, we do agent'i':
          if (a2["opinion"] > 0) {
            agm[i, "infoPos"] = (agm[i, "infoPos"] + a2["opinion"])
            if (agm[i, "infoPos"] > 1) agm[i, "infoPos"] = 1
          } else {
            agm[i, "infoNeg"] = (agm[i, "infoNeg"] - a2["opinion"])
            if (agm[i, "infoNeg"] > 1) agm[i, "infoNeg"] = 1
          }
        # According to Han van der Maas we have also do reinforcement of
        # 'i' opinion, so we have to add its opinion to respective info silo:
        if (reinforce) {
          if (a1["opinion"] >= 0) {
            # If agent 'i' has positive opinion, adds it to positive silo:
            agm[i, "infoPos"] = (agm[i, "infoPos"] + a1["opinion"])
            if (agm[i, "infoPos"] > 1) agm[i, "infoPos"] = 1  # Cut-off, of course!
          } else {
            # If agent 'i' has negative opinion, adds it to negative silo:
            agm[i, "infoNeg"] = (agm[i, "infoNeg"] - a1["opinion"])
            if (agm[i, "infoNeg"] > 1) agm[i, "infoNeg"] = 1  # Cut-off, of course!
          }
        }
        ##  Secondly, we do agent'p':
          if (a1["opinion"] > 0) {
            agm[p, "infoPos"] = (agm[p, "infoPos"] + a1["opinion"])
            if (agm[p, "infoPos"] > 1) agm[p, "infoPos"] = 1
          } else {
            agm[p, "infoNeg"] = (agm[p, "infoNeg"] - a1["opinion"])
            if (agm[p, "infoNeg"] > 1) agm[p, "infoNeg"] = 1
          }
        # According to Han van der Maas we have also do same reinforcement of
        # 'p' opinion, so we have to add its opinion to respective info silo:
        if (reinforce) {
          if (a2["opinion"] >= 0) {
            # If agent 'p' has positive opinion, adds it to positive silo:
            agm[p, "infoPos"] = (agm[p, "infoPos"] + a2["opinion"])
            if (agm[p, "infoPos"] > 1) agm[p, "infoPos"] = 1  # Cut-off, of course!
          } else {
            # If agent 'ip' has negative opinion, adds it to negative silo:
            agm[p, "infoNeg"] = (agm[p, "infoNeg"] - a2["opinion"])
            if (agm[p, "infoNeg"] > 1) agm[p, "infoNeg"] = 1  # Cut-off, of course!
          }
        }
    }
    ## Now for each agent update:
    #          attention and
    agm[, "attention"] = (agm[, "infoPos"] + agm[, "infoNeg"]) / attentionDenom
    agm[agm[, "attention"] > 1, "attention"] = 1

    #          opinion of all agents
    agm[, "opinion"] = -agm[, "opinion"] ^ 3 + (agm[, "infoPos"] + agm[, "infoNeg"] - agm[, "folding"]) * (agm[, "opinion"]) + (agm[, "infoPos"] - agm[, "infoNeg"])
    agm[agm[, "opinion"] > 1, "opinion"] = 1
    agm[agm[, "opinion"] < -1, "opinion"] = -1

    #          memory/information
    agm[, "infoPos"] = agm[, "infoPos"] * forgeting
    agm[, "infoNeg"] = agm[, "infoNeg"] * forgeting

    ## Giving back updated matrix
    agm
  }



# Experiment function -----------------------------------------------------

experiment =
  function(opDistributionX = "Black Pete", rounds = 100, Nx = 1000, seedX = 1,
           reinforceX = reinforce, forgetingX = forgeting,
           foldingX = foldingPoint,  folding_sdX = folding_sd,
           communicationRateX = communicationRate) {
  # Setting seed:
  set.seed(seedX)

  # Initialization of the 'world' and network:
  agm = createPublic(N = Nx, opDistribution = opDistributionX,
                     foldingPoint = foldingX, folding_sd = folding_sdX)
  cmm = createMatrix(NN = Nx)
  # print("Initialized!")

  # Simulation:
  for (r in 1:rounds) {
    agm = simRound(agm = agm, cmm = cmm, forgeting = forgetingX,
                   pia = communicationRateX, bootstrap = T, reinforce = reinforceX)
  }

  # Calculating polarization:
  op =  sort(agm[, "opinion"])
  SD = sd(op)
  manhattan = sum(abs(op)) / Nx
  op1 = op[1:(Nx / 2)]
  op2 = op[((Nx / 2) + 1):Nx]
  ESBG = (mean(op2) - mean(op1)) / (sd(op1) + sd(op2) + 1)

  # Printing:
  print(paste0("Results: ESBG=", round(ESBG / 2, 2), ", SD=", round(SD, 2), ", Manhattan=", round(manhattan, 2)))

  # Returning the tibble with inputs and results:
  return(tibble(seed = seedX, reinforce = reinforceX,
                communicationRate = communicationRateX, forgeting = forgetingX,
                foldingPoint = foldingX, folding_sd = folding_sdX,
                SD, manhattan, ESBG, opDistribution = opDistributionX))
}



# Running experiment ------------------------------------------------------

# Constants for sure:
N = 1000
v = 6
attentionDenom = sqrt(2)
opDistribution = "Black Pete"
reinforce = TRUE

# Set of FOR cycles
startTime = Sys.time()
SIM = 1
for (seed in 1:50) {
  # Creating empty tibble for storing results:
  results = tibble(seed = NA_integer_, reinforce = NA,
                   communicationRate = NA_real_, forgeting = NA_real_,
                   foldingPoint = NA_real_, folding_sd = NA_real_,
                   SD = NA_real_, manhattan = NA_real_,
                   ESBG = NA_real_, opDistribution = NA_character_)
  for (communicationRate in c(.05, .15, .25, .35, .45, .55, .65, .75, .85, .95)) {
    for (forgeting in c(.2, .45, .7, .95)) {
      for (foldingPoint in c(.05, .15, .25, .35, .45, .55, .65, .75, .85, .95)) {
        for (folding_sd in c(0, 0.025, 0.05, 0.1, 0.15)) {
              results = results %>%
                add_row(
                  experiment(
                    seedX = seed, reinforceX = reinforce,
                    communicationRateX = communicationRate,
                    forgetingX = forgeting, folding_sdX = folding_sd,
                    foldingX = foldingPoint,
                    rounds = 100, Nx = 1000, opDistributionX = "Black Pete"
                  )
                )
              print(
                paste0("Simulation ", SIM," just done, time elapsed: ",
                       round(Sys.time() - startTime, 2), ". (seed=", seed,
                       ", forgeting=", forgeting, ", folding=", foldingPoint,
                       ", folding_sd=", folding_sd,
                       ", comm.rate=", communicationRate, ", rounds=100).")
              )
              SIM = SIM + 1
        }
      }
    }
  }
# Saving ------------------------------------------------------------------
  save(results, file = paste0("results4_seeds_", seed, ".RData"))
}



