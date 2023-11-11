#### Main script for project with Guga on folded opinion, EXP9
#### In this version we play with 'Black Pete' scenario -- initial opinion of majority.

## Encoding: windows-1250
## Created:  2023-06-29 Fran»esko
## Edited:   2023-08-09 Fran»esko

## NOTES:
## 1) What vars we do need for agents?
##    ID
##    weight of own opinion (average and SD)
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
           foldingPoint = 0.05, meanWeight = 1, sdFractionWeight = sdFractionWeightX, minWeight = 0.05,
           majInOpAv = majInOpAvX, majInOpSD = majInOpSDX, attentionDenom = sqrt(2)) {
  # Needed constants
  N = N  # number of agents
  v = 6  # number of agents' variables/columns
  foldingPoint = foldingPoint  # Average folding point
  # folding_sd = folding_sd  # SD of average folding point
  # forgeting = forgeting  # Rate of information kept in info silos
  attentionDenom = attentionDenom  # Denominator for calculating Attention
  sdFractionWeight = 0  # Just for sure in EXP 9 we set this var hard-wired

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
  # majInOpAv = 0.02   # Average initial opinion of majority
  # majInOpSD = 0.01   # SD of initial opinion of majority
  minInOpAv = -0.8   # Average initial opinion of minority
  minInOpSD = 0.15   # SD of initial opinion of minority

  # Changing the same constants in case of "fair" scenario
  # if (opDistribution == "Fair") {
  #   majFrac = 0.50  # Fraction of the majority
  #   majPosMin = .00  # Minimum of positive information of majority
  #   majPosMax = .25  # Maximum of positive information of majority
  #   majNegMin = .00  # Minimum of negative information of majority
  #   majNegMax = .25  # Maximum of negative information of majority
  #   minPosMin = .00  # Minimum of positive information of minority
  #   minPosMax = .25  # Maximum of positive information of minority
  #   minNegMin = .00  # Minimum of negative information of minority
  #   minNegMax = .25  # Maximum of negative information of minority
  #   majInOpAv = 0.00   # Average initial opinion of majority
  #   majInOpSD = 0.05   # SD of initial opinion of majority
  #   minInOpAv = 0.00   # Average initial opinion of minority
  #   minInOpSD = 0.05   # SD of initial opinion of minority
  # }

  # init of empty matrix
  am = matrix(data = rep(1:N, times = v, N * v),
              ncol = v, byrow = F)  # We initialize whole matrix by IDs in every column.
  colnames(am) = c("ID", "infoPos", "infoNeg", "attention", "opinion", "ownOpWeight")

  # Creating values in vars 2:5
  for (i in 1:N) {
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

    # Own opinion weight for reinforcing sub-model/algorithm
    x = rnorm(n = 1, mean = meanWeight, sd = meanWeight * sdFractionWeight)
    while (x <= minWeight) {
      x = rnorm(n = 1, mean = meanWeight, sd = meanWeight * sdFractionWeight)
    }
    am[i, "ownOpWeight"] = x
  }
  # Initially processed opinion
  am[, "opinion"] = -am[, "opinion"] ^ 3 + (am[, "infoPos"] + am[, "infoNeg"] - foldingPoint) * (am[, "opinion"]) + (am[, "infoPos"] - am[, "infoNeg"])
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
  # NN = N  # Number of agents

  # Creation of vector 'V' of IDs, where the number of IDs represents
  # how many connections the respective agent will have:
  nContacts = map(1:NN, ~ ceiling(rnorm(1, neis, nsd))) %>% unlist()
  nContacts[nContacts < 1] = 1
  V = map2(1:NN, nContacts, ~ rep(.x, .y)) %>% unlist()

  # And now we can go through the vector and
  # randomly draw for the ID its comm. partner:
  C = map2(1:NN, nContacts, ~ sample(setdiff(1:NN, .x), size = .y)) %>% unlist()

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
  simRound = function(agm = agm, cmm = cmm, forgeting = 0.5, pia = 0.5, bootstrap = T,
                      reinforce = T, foldingPoint = foldingX) {
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
            agm[i, "infoPos"] = (agm[i, "infoPos"] + (a1["opinion"] * a1["ownOpWeight"]))
            if (agm[i, "infoPos"] > 1) agm[i, "infoPos"] = 1  # Cut-off, of course!
          } else {
            # If agent 'i' has negative opinion, adds it to negative silo:
            agm[i, "infoNeg"] = (agm[i, "infoNeg"] - (a1["opinion"] * a1["ownOpWeight"]))
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
            agm[p, "infoPos"] = (agm[p, "infoPos"] + (a2["opinion"] * a2["ownOpWeight"]))
            if (agm[p, "infoPos"] > 1) agm[p, "infoPos"] = 1  # Cut-off, of course!
          } else {
            # If agent 'ip' has negative opinion, adds it to negative silo:
            agm[p, "infoNeg"] = (agm[p, "infoNeg"] - (a2["opinion"] * a2["ownOpWeight"]))
            if (agm[p, "infoNeg"] > 1) agm[p, "infoNeg"] = 1  # Cut-off, of course!
          }
        }
    }
    ## Now for each agent update:
    #          attention and
    agm[, "attention"] = (agm[, "infoPos"] + agm[, "infoNeg"]) / attentionDenom
    agm[agm[, "attention"] > 1, "attention"] = 1

    #          opinion of all agents
    agm[, "opinion"] = -agm[, "opinion"] ^ 3 + (agm[, "infoPos"] + agm[, "infoNeg"] - foldingPoint) * (agm[, "opinion"]) + (agm[, "infoPos"] - agm[, "infoNeg"])
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
           reinforceX = reinforce, forgetingX = forgeting, meanWeightX = meanWeight,
           foldingX = foldingPoint, majInOpAvX = majInOpAv, majInOpSDX = majInOpSD,
           communicationRateX = communicationRate, sdFractionWeightX = sdFractionWeight) {
  # Setting seed:
  set.seed(seedX)

  # Initialization of the 'world' and network:
  agm = createPublic(N = Nx, opDistribution = opDistributionX, meanWeight = meanWeightX,
                     sdFractionWeight = sdFractionWeightX,
                     majInOpAv = majInOpAvX, majInOpSD = majInOpSDX, foldingPoint = foldingX)
  cmm = createMatrix(NN = Nx, neis = 6.1, nsd = 1.2)
  # print("Initialized!")

  # Storing initial distribution
  y1 = (hist(agm[, "opinion"], breaks = seq(from = -1.1, to = 1.1, by = 0.2), plot = F))$count
  y2 = (hist(agm[, "attention"], breaks = seq(from = -.05, to = 1.05, by = 0.1), plot = F))$count
  y3 = (hist(agm[, "infoPos"], breaks = seq(from = -.05, to = 1.05, by = 0.1), plot = F))$count
  y4 = (hist(agm[, "infoNeg"], breaks = seq(from = -.05, to = 1.05, by = 0.1), plot = F))$count
  y6 = (hist((agm[, "infoPos"] + agm[, "infoNeg"]), breaks = seq(from = -0.1, to = 2.1, by = 0.2), plot = F))$count
  y5 = (hist((agm[, "infoPos"] - agm[, "infoNeg"]), breaks = seq(from = -1.1, to = 1.1, by = 0.2), plot = F))$count

  # Simulation:
  for (r in 1:rounds) {
    agm = simRound(agm = agm, cmm = cmm, forgeting = forgetingX, foldingPoint = foldingX,
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
  x1 = (hist(agm[, "opinion"], breaks = seq(from = -1.1, to = 1.1, by = 0.2), plot = F))$count
  x2 = (hist(agm[, "attention"], breaks = seq(from = -.05, to = 1.05, by = 0.1), plot = F))$count
  x3 = (hist(agm[, "infoPos"], breaks = seq(from = -.05, to = 1.05, by = 0.1), plot = F))$count
  x4 = (hist(agm[, "infoNeg"], breaks = seq(from = -.05, to = 1.05, by = 0.1), plot = F))$count
  x6 = (hist((agm[, "infoPos"] + agm[, "infoNeg"]), breaks = seq(from = -0.1, to = 2.1, by = 0.2), plot = F))$count
  x5 = (hist((agm[, "infoPos"] - agm[, "infoNeg"]), breaks = seq(from = -1.1, to = 1.1, by = 0.2), plot = F))$count
  # print(paste0("Results: ESBG=", round(ESBG / 2, 2), "; SD=", round(SD, 2), "; Manhattan=", round(manhattan, 2),
  #              "; Init. Op.: ", paste(y1, collapse = ", "), "; Fin. Op.: ", paste(x1, collapse = ", ")))


  # Returning the tibble with inputs and results:
  return(tibble(seed = seedX, opDistribution = opDistributionX, reinforce = reinforceX,
                communicationRate = communicationRateX, forgeting = forgetingX,
                foldingPoint = foldingX, meanWeight = meanWeightX, sdFractionWeight = sdFractionWeightX,
                majInOpAv = majInOpAvX, majInOpSD = majInOpSDX,
                b_io_1 = y1[1], b_io_2 = y1[2], b_io_3 = y1[3], b_io_4 = y1[4], b_io_5 = y1[5], b_io_6 = y1[6],
                b_io_7 = y1[7], b_io_8 = y1[8], b_io_9 = y1[9], b_io_10 = y1[10], b_io_11 = y1[11],
                b_ia_1 = y2[1], b_ia_2 = y2[2], b_ia_3 = y2[3], b_ia_4 = y2[4], b_ia_5 = y2[5], b_ia_6 = y2[6],
                b_ia_7 = y2[7], b_ia_8 = y2[8], b_ia_9 = y2[9], b_ia_10 = y2[10], b_ia_11 = y2[11],
                b_ipi_1 = y3[1], b_ipi_2 = y3[2], b_ipi_3 = y3[3], b_ipi_4 = y3[4], b_ipi_5 = y3[5], b_ipi_6 = y3[6],
                b_ipi_7 = y3[7], b_ipi_8 = y3[8], b_ipi_9 = y3[9], b_ipi_10 = y3[10], b_ipi_11 = y3[11],
                b_ini_1 = y4[1], b_ini_2 = y4[2], b_ini_3 = y4[3], b_ini_4 = y4[4], b_ini_5 = y4[5], b_ini_6 = y4[6],
                b_ini_7 = y4[7], b_ini_8 = y4[8], b_ini_9 = y4[9], b_ini_10 = y4[10], b_ini_11 = y4[11],
                b_isi_1 = y6[1], b_isi_2 = y6[2], b_isi_3 = y6[3], b_isi_4 = y6[4], b_isi_5 = y6[5], b_isi_6 = y6[6],
                b_isi_7 = y6[7], b_isi_8 = y6[8], b_isi_9 = y6[9], b_isi_10 = y6[10], b_isi_11 = y6[11],
                b_ii_1 = y5[1], b_ii_2 = y5[2], b_ii_3 = y5[3], b_ii_4 = y5[4], b_ii_5 = y5[5], b_ii_6 = y5[6],
                b_ii_7 = y5[7], b_ii_8 = y5[8], b_ii_9 = y5[9], b_ii_10 = y5[10], b_ii_11 = y5[11],
                SD, manhattan, ESBG,
                b_fo_1 = x1[1], b_fo_2 = x1[2], b_fo_3 = x1[3], b_fo_4 = x1[4], b_fo_5 = x1[5], b_fo_6 = x1[6],
                b_fo_7 = x1[7], b_fo_8 = x1[8], b_fo_9 = x1[9], b_fo_10 = x1[10], b_fo_11 = x1[11],
                b_fa_1 = x2[1], b_fa_2 = x2[2], b_fa_3 = x2[3], b_fa_4 = x2[4], b_fa_5 = x2[5], b_fa_6 = x2[6],
                b_fa_7 = x2[7], b_fa_8 = x2[8], b_fa_9 = x2[9], b_fa_10 = x2[10], b_fa_11 = x2[11],
                b_fpi_1 = x3[1], b_fpi_2 = x3[2], b_fpi_3 = x3[3], b_fpi_4 = x3[4], b_fpi_5 = x3[5], b_fpi_6 = x3[6],
                b_fpi_7 = x3[7], b_fpi_8 = x3[8], b_fpi_9 = x3[9], b_fpi_10 = x3[10], b_fpi_11 = x3[11],
                b_fni_1 = x4[1], b_fni_2 = x4[2], b_fni_3 = x4[3], b_fni_4 = x4[4], b_fni_5 = x4[5], b_fni_6 = x4[6],
                b_fni_7 = x4[7], b_fni_8 = x4[8], b_fni_9 = x4[9], b_fni_10 = x4[10], b_fni_11 = x4[11],
                b_fsi_1 = x6[1], b_fsi_2 = x6[2], b_fsi_3 = x6[3], b_fsi_4 = x6[4], b_fsi_5 = x6[5], b_fsi_6 = x6[6],
                b_fsi_7 = x6[7], b_fsi_8 = x6[8], b_fsi_9 = x6[9], b_fsi_10 = x6[10], b_fsi_11 = x6[11],
                b_fi_1 = x5[1], b_fi_2 = x5[2], b_fi_3 = x5[3], b_fi_4 = x5[4], b_fi_5 = x5[5], b_fi_6 = x5[6],
                b_fi_7 = x5[7], b_fi_8 = x5[8], b_fi_9 = x5[9], b_fi_10 = x5[10], b_fi_11 = x5[11]))
}



# Running experiment ------------------------------------------------------

# Constants for sure:
N = 1000
v = 6
attentionDenom = sqrt(2)
reinforce = TRUE
sdFractionWeight = 0
opDistribution = "Black Pete"

# Set of FOR cycles
startTime = Sys.time()
SIM = 1
y1 = rep(-1, 11); y2 = y1; y3 = y1; y4 = y1; y5 = y1; y6 = y1
x1 = y1; x2 = y1; x3 = y1; x4 = y1; x5 = y1; x6 = y1
for (seed in c(1:50)) {
  # Creating empty tibble for storing results:
  results = tibble(seed = NA_integer_, opDistribution = NA_character_, reinforce = NA,
                   communicationRate = NA_real_, forgeting = NA_real_,
                   foldingPoint = NA_real_, meanWeight = NA_real_, sdFractionWeight = 0,
                   majInOpAv = NA_real_, majInOpSD = NA_real_,
                   b_io_1 = y1[1], b_io_2 = y1[2], b_io_3 = y1[3], b_io_4 = y1[4], b_io_5 = y1[5], b_io_6 = y1[6],
                   b_io_7 = y1[7], b_io_8 = y1[8], b_io_9 = y1[9], b_io_10 = y1[10], b_io_11 = y1[11],
                   b_ia_1 = y2[1], b_ia_2 = y2[2], b_ia_3 = y2[3], b_ia_4 = y2[4], b_ia_5 = y2[5], b_ia_6 = y2[6],
                   b_ia_7 = y2[7], b_ia_8 = y2[8], b_ia_9 = y2[9], b_ia_10 = y2[10], b_ia_11 = y2[11],
                   b_ipi_1 = y3[1], b_ipi_2 = y3[2], b_ipi_3 = y3[3], b_ipi_4 = y3[4], b_ipi_5 = y3[5], b_ipi_6 = y3[6],
                   b_ipi_7 = y3[7], b_ipi_8 = y3[8], b_ipi_9 = y3[9], b_ipi_10 = y3[10], b_ipi_11 = y3[11],
                   b_ini_1 = y4[1], b_ini_2 = y4[2], b_ini_3 = y4[3], b_ini_4 = y4[4], b_ini_5 = y4[5], b_ini_6 = y4[6],
                   b_ini_7 = y4[7], b_ini_8 = y4[8], b_ini_9 = y4[9], b_ini_10 = y4[10], b_ini_11 = y4[11],
                   b_isi_1 = y6[1], b_isi_2 = y6[2], b_isi_3 = y6[3], b_isi_4 = y6[4], b_isi_5 = y6[5], b_isi_6 = y6[6],
                   b_isi_7 = y6[7], b_isi_8 = y6[8], b_isi_9 = y6[9], b_isi_10 = y6[10], b_isi_11 = y6[11],
                   b_ii_1 = y5[1], b_ii_2 = y5[2], b_ii_3 = y5[3], b_ii_4 = y5[4], b_ii_5 = y5[5], b_ii_6 = y5[6],
                   b_ii_7 = y5[7], b_ii_8 = y5[8], b_ii_9 = y5[9], b_ii_10 = y5[10], b_ii_11 = y5[11],
                   SD = NA_real_, manhattan = NA_real_, ESBG = NA_real_,
                   b_fo_1 = x1[1], b_fo_2 = x1[2], b_fo_3 = x1[3], b_fo_4 = x1[4], b_fo_5 = x1[5], b_fo_6 = x1[6],
                   b_fo_7 = x1[7], b_fo_8 = x1[8], b_fo_9 = x1[9], b_fo_10 = x1[10], b_fo_11 = x1[11],
                   b_fa_1 = x2[1], b_fa_2 = x2[2], b_fa_3 = x2[3], b_fa_4 = x2[4], b_fa_5 = x2[5], b_fa_6 = x2[6],
                   b_fa_7 = x2[7], b_fa_8 = x2[8], b_fa_9 = x2[9], b_fa_10 = x2[10], b_fa_11 = x2[11],
                   b_fpi_1 = x3[1], b_fpi_2 = x3[2], b_fpi_3 = x3[3], b_fpi_4 = x3[4], b_fpi_5 = x3[5], b_fpi_6 = x3[6],
                   b_fpi_7 = x3[7], b_fpi_8 = x3[8], b_fpi_9 = x3[9], b_fpi_10 = x3[10], b_fpi_11 = x3[11],
                   b_fni_1 = x4[1], b_fni_2 = x4[2], b_fni_3 = x4[3], b_fni_4 = x4[4], b_fni_5 = x4[5], b_fni_6 = x4[6],
                   b_fni_7 = x4[7], b_fni_8 = x4[8], b_fni_9 = x4[9], b_fni_10 = x4[10], b_fni_11 = x4[11],
                   b_fsi_1 = x6[1], b_fsi_2 = x6[2], b_fsi_3 = x6[3], b_fsi_4 = x6[4], b_fsi_5 = x6[5], b_fsi_6 = x6[6],
                   b_fsi_7 = x6[7], b_fsi_8 = x6[8], b_fsi_9 = x6[9], b_fsi_10 = x6[10], b_fsi_11 = x6[11],
                   b_fi_1 = x5[1], b_fi_2 = x5[2], b_fi_3 = x5[3], b_fi_4 = x5[4], b_fi_5 = x5[5], b_fi_6 = x5[6],
                   b_fi_7 = x5[7], b_fi_8 = x5[8], b_fi_9 = x5[9], b_fi_10 = x5[10], b_fi_11 = x5[11])
  for (communicationRate in c(.05, .35, .65, .95)) {
    for (forgeting in c(.2, .45, .7, .95)) {
      for (foldingPoint in c(.5, .35, .65, .95)) {
        for (meanWeight in c(0.5, 1, 1.5)) {
          for (majInOpAv in c(0.05, 0.1, 0.25)) {
            for (majInOpSD in c(0.05, 0.1, 0.2)) {
            results = results %>%
                add_row(
                  experiment(
                    seedX = seed, reinforceX = reinforce,
                    communicationRateX = communicationRate,
                    forgetingX = forgeting, meanWeightX = meanWeight,
                    sdFractionWeightX = sdFractionWeight, foldingX = foldingPoint,
                    rounds = 100, Nx = 1000, opDistributionX = opDistribution
                  ))
              print(
                paste0("Simulation ", SIM," just done, time elapsed: ",
                       round(Sys.time() - startTime, 2), ". (seed=", seed,
                       ", forgeting=", forgeting, ", folding=", foldingPoint,
                       ", comm.rate=", communicationRate, ", meanWeight=", meanWeight,
                       ", majInOpAv=", majInOpAv, ", majInOpSD=", majInOpSD,")."))
              SIM = SIM + 1
            }
          }
        }
      }
    }
  }
# Saving ------------------------------------------------------------------
  save(results, file = paste0("results9_seeds_", seed, ".RData"))
}



