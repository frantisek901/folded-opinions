#### Main script for project with Guga on folded opinion

## Encoding: windows-1250
## Created:  2023-06-29 FranÈesko
## Edited:   2023-06-29 FranÈesko

## NOTES:
## 1) What vars we do need for agents?
##    ID
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



# matrix with agents ------------------------------------------------------

# Needed constants
N = 1000  # number of agents
v = 5  # number of agents' variables/columns
maxInfo = 1  # Maximum caparity of the information silo

# init of empty matrix
am = matrix(data = rep(1:N, times = v, byrow = F, N * v),
            ncol = v)  # We initialize whole matrix by IDs in every column.
colnames(am) = c("ID", "infoPos", "infoNeg", "attention", "opinion")

# Creating values in vars 2:5
for (i in 1:N) {
  am[i, 2] = runif(n = 1, max = maxInfo)
  am[i, 3] = runif(n = 1, max = maxInfo)
  am[i, 4] = runif(n = 1, max = 2 / sqrt(2))
  am[i, 5] = rnorm(n = 1, mean = 0, sd = 0.25)
}
am
mx = am

# communication matrix ----------------------------------------------------

# Note: this will be very probably completely rewritten, but for now...
# For now we create directed network, i.e. A can initiate conversation
# with B, but B can't do the same with A.

# Again, constants first
neis = 6.1  # Average number of directed neighbors
nsd = 1.2  # SD of number of directed neighbors

# Creation of vector 'V' of IDs, where the number of IDs represents
# how many connections the respective agent will have:
V = map(1:N, ~rep(.x, ceiling(rnorm(1, neis, nsd)))) %>% unlist()

# And now we can go through the vector and
# randomly draw for the ID its comm. partner:
C = map(V, ~sample(setdiff(1:N, .x), size = 1)) %>% unlist()

# Now using this vector for initialization of matrix of directed links:
cm = matrix(data = c(V, C), ncol = 2, byrow = F)
cm



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
simRound = function(agm = am, cmm = cm, buyingOpinion = T,
                    pia = 0.5, bootstrap = T, forgeting = 0.8) {
  # Let's start with defining needed constants:
  NN = nrow(agm)

  # select initiative agents
  ia = sample(1:NN, size = ceiling(pia * NN),
             replace = bootstrap)
  # Note: if 'bootstrap' == T, some agents might be selected more than once!

  ## Let's do the rest inside FOR cycle:
  for (i in ia) {
    # select partners for initiative agents and store their copies
    p = sample(cm[cm[,1] == i, 2], size = 1)
    a1 = agm[i, ]  # NOTE-BEWARE!!! 'a1' becomes vector, not matrix! Same a2...
    a2 = agm[p, ]

    ## for each communicating pair: exchange information and update info sila
  if (buyingOpinion) {
    # Firstly, we do agent'i':
    if (a2["opinion"] > 0) {
      agm[i, "infoPos"] = (agm[i, "infoPos"] + a2["opinion"])
      if (agm[i, "infoPos"] > 1) agm[i, "infoPos"] = 1
    } else {
      agm[i, "infoNeg"] = (agm[i, "infoNeg"] - a2["opinion"])
      if (agm[i, "infoNeg"] > 1) agm[i, "infoNeg"] = 1
    }
    #  Secondly, we do agent'p':
    if (a1["opinion"] > 0) {
      agm[p, "infoPos"] = (agm[p, "infoPos"] + a1["opinion"])
      if (agm[p, "infoPos"] > 1) agm[p, "infoPos"] = 1
    } else {
      agm[p, "infoNeg"] = (agm[p, "infoNeg"] - a1["opinion"])
      if (agm[p, "infoNeg"] > 1) agm[p, "infoNeg"] = 1
    }
  } else {
    #  Firstly, we do agent'i':
    if (a2["infoNeg"] < a2["infoPos"]) {
      agm[i, "infoPos"] = (agm[i, "infoPos"] + a2["infoPos"] - a2["infoNeg"]) #* forgeting
      #agm[i, "infoNeg"] =  agm[i, "infoNeg"] * forgeting
      if (agm[i, "infoPos"] > 1) agm[i, "infoPos"] = 1
    } else {
      agm[i, "infoNeg"] = (agm[i, "infoNeg"] + a2["infoNeg"] - a2["infoPos"]) #* forgeting
      #agm[i, "infoPos"] =  agm[i, "infoPos"] * forgeting
      if (agm[i, "infoNeg"] > 1) agm[i, "infoNeg"] = 1
    }
    #  Secondly, we do agent'p':
    if (a1["infoNeg"] < a1["infoPos"]) {
      agm[p, "infoPos"] = (agm[p, "infoPos"] + a1["infoPos"] - a1["infoNeg"]) #* forgeting
      #agm[p, "infoNeg"] =  agm[p, "infoNeg"] * forgeting
      if (agm[p, "infoPos"] > 1) agm[p, "infoPos"] = 1
    } else {
      agm[p, "infoNeg"] = (agm[p, "infoNeg"] + a1["infoNeg"] - a1["infoPos"]) #* forgeting
      #agm[p, "infoPos"] =  agm[p, "infoPos"] * forgeting
      if (agm[p, "infoNeg"] > 1) agm[p, "infoNeg"] = 1
    }
  }
  }
  # Now for each agent update:

  #          attention and
  agm[, "attention"] = (agm[, "infoPos"] + agm[, "infoNeg"]) #/ sqrt(2)
  agm[agm[, "attention"] > 1, "attention"] = 1

  #          opinion of both agents
  agm[, "opinion"] = (agm[, "infoPos"] - agm[, "infoNeg"]) * (agm[, "attention"])

  #          memory/information
  agm[, "infoPos"] = agm[, "infoPos"] * forgeting
  agm[, "infoNeg"] = agm[, "infoNeg"] * forgeting


  ## Ploting opinion
  print(opHist())

  ## Giving back updated matrix
  agm
}


# Graphs ------------------------------------------------------------------

## Histograms
# Preparing function for histogram  of opinions
opHist = function(mtrx = am, col = v, bw = 0.2, fill = "orange", xlab = "opinion") {
  mtrx[, col] %>%
    as_tibble() %>%
    rename(opinion = value) %>%
    ggplot(aes(x = opinion)) +
    geom_histogram(fill = fill, binwidth = bw, closed = "right", center = 0) +
    labs(x = xlab) +
    theme_classic()
}

## Using function
# Plotting opinion
opHist()

# Plotting # of connections
cm %>%
  as.vector() %>%
  as.matrix(ncol = 1) %>%
  as_tibble() %>%
  count(V1) %>% arrange(desc(n)) %>%
  as.matrix() %>%
  opHist(col = 2, bw = 1, fill = "steelblue", xlab = "Communication connections")



# Testing -----------------------------------------------------------------


# am = mx
# am == mx
# am == simRound()

am = mx
for (i in 1:30) {
  am = simRound(pia = 0.82, forgeting = 0.9, buyingOpinion = F, bootstrap = T)
  print(paste(i, ":", sum(am[, 2:3])))
}



