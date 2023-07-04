#### Main script for project with Guga on folded opinion

## Encoding: windows-1250
## Created:  2023-06-29 Fran»esko
## Edited:   2023-07-04 Fran»esko

## NOTES:
## 1) What vars we do need for agents?
##    ID
##    acceptance
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

# Needed constants
it = Sys.time()  # Initialization time
N = 1000  # number of agents
v = 6  # number of agents' variables/columns
maxInfo = 1  # Maximum capacity of the information silo
aa = 0.55    # Average acceptance of others infoBias/opinion
aasd = 0.05  # SD of average acceptance
# majFrac = 0.75  # Fraction of the majority
# majPosMin = .05  # Minimum of positive information of majority
# majPosMax = .15  # Maximum of positive information of majority
# majNegMin = .05  # Minimum of negative information of majority
# majNegMax = .15  # Maximum of negative information of majority
# minPosMin = .05  # Minimum of positive information of minority
# minPosMax = .15  # Maximum of positive information of minority
# minNegMin = .80  # Minimum of negative information of minority
# minNegMax = 1.0  # Maximum of negative information of minority
# majInOpAv = 0.00   # Average initial opinion of majority
# majInOpSD = 0.01   # SD of initial opinion of majority
# minInOpAv = -0.8   # Average initial opinion of minority
# minInOpSD = 0.15   # SD of initial opinion of minority
# foldingPoint = 0.05       # Folding point
# forgeting = 0.95          # Rate of information kept in info silos
# communicationRate = 0.45  # How many agents initiate communication
# attentionDenom = sqrt(2)  # Denominator for calculating Attention


majFrac = 0.50  # Fraction of the majority
majPosMin = .00  # Minimum of positive information of majority
majPosMax = .25  # Maximum of positive information of majority
majNegMin = .00  # Minimum of negative information of majority
majNegMax = .25  # Maximum of negative information of majority
minPosMin = .00  # Minimum of positive information of minority
minPosMax = .25  # Maximum of positive information of minority
minNegMin = .00  # Minimum of negative information of minority
minNegMax = .25  # Maximum of negative information of minority
majInOpAv = 0.00   # Average initial opinion of majority
majInOpSD = 0.05   # SD of initial opinion of majority
minInOpAv = 0.00   # Average initial opinion of minority
minInOpSD = 0.05   # SD of initial opinion of minority
foldingPoint = 0.95       # Folding point
forgeting = 0.95          # Rate of information kept in info silos
communicationRate = 0.15  # How many agents initiate communication
attentionDenom = sqrt(2)  # Denominator for calculating Attention



# init of empty matrix
am = matrix(data = rep(1:N, times = v, byrow = F, N * v),
            ncol = v)  # We initialize whole matrix by IDs in every column.
colnames(am) = c("ID", "acceptance", "infoPos", "infoNeg", "attention", "opinion")

# Creating values in vars 2:6
for (i in 1:N) {
  # Acceptance
  x = rnorm(n = 1, mean = aa, sd = aasd)
  while(x <= 0) {print(paste0("Value of ACCEPTANCE(", i,") is ", x, ", we have to draw again...")); x = rnorm(n = 1, mean = aa, sd = aasd)}
  am[i, "acceptance"] = x

  # Information
  am[i, "infoPos"] = if_else(i < N * majFrac,
                             runif(n = 1, min = majPosMin, max = majPosMax),
                             runif(n = 1, min = minPosMin, max = minPosMax))
  am[am[, "infoPos"] > 1, "infoPos"] = 1
  am[i, "infoNeg"] = if_else(i < N * majFrac,
                             runif(n = 1, min = majNegMin, max = majNegMax),
                             runif(n = 1, min = minNegMin, max = minNegMax))
  am[am[, "infoNeg"] > 1, "infoNeg"] = 1

  # Attention
  am[, "attention"] = (am[, "infoPos"] + am[, "infoNeg"]) / attentionDenom
  am[am[, "attention"] > 1, "attention"] = 1

  # Opinion
  # Initial random opinion
  if (i < N * majFrac) {
    x = rnorm(n = 1, mean = majInOpAv, sd = majInOpSD)
    while(x < -1 | x > 1) {print(paste0("Value of OPINION(", i,") is ", x, ", we have to draw again...")); x = rnorm(n = 1, mean = majInOpAv, sd = majInOpSD)}
    am[, "opinion"] = x
  } else {
    x = rnorm(n = 1, mean = minInOpAv, sd = minInOpSD)
    while(x < -1 | x > 1) {print(paste0("Value of OPINION(", i,") is ", x, ", we have to draw again...")); x = rnorm(n = 1, mean = minInOpAv, sd = minInOpSD)}
    am[, "opinion"] = x
  }
  am[, "opinion"] = -am[, "opinion"]^3 + (am[, "infoPos"] + am[, "infoNeg"] - foldingPoint) * (am[, "opinion"]) + (am[, "infoPos"] - am[, "infoNeg"])
  am[am[, "opinion"] > 1, "opinion"] = 1
  am[am[, "opinion"] < -1, "opinion"] = -1

  # Information update
  # am[, "infoPos"] = am[, "infoPos"] * forgeting
  # am[, "infoNeg"] = am[, "infoNeg"] * forgeting
}
# am
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
# cm



# Graphs ------------------------------------------------------------------

## Histograms
# Preparing function for histogram  of opinions
opHist = function(mtrx = am, col = v, bw = 0.2, fill = "orange", xlab = "opinion",
                  tit = "", limits = c(-1, 1)) {
  mtrx[, col] %>%
    as_tibble() %>%
    rename(opinion = value) %>%
    ggplot(aes(x = opinion)) +
    geom_density(col = fill, fill = fill, alpha = 0.3) +
    # geom_histogram(fill = fill, binwidth = bw, closed = "right", center = 0) +
    scale_x_continuous(limits = limits) +
    # scale_y_continuous(limits = c(0, 50)) +
    labs(x = xlab, title = tit) +
    theme_classic()
}

infoHist = function(mtrx = am, cols = 3:4, bw = 0.2, fill = "steelblue",
                    xlab = "information bias", tit = "", limits = c(-1, 1)) {
  mtrx[, cols] %>%
    as_tibble() %>%
    rename(infoBias = 1) %>%
    mutate(infoBias = infoBias - infoNeg) %>%
    ggplot(aes(x = infoBias)) +
    geom_density(col = fill, fill = fill, alpha = 0.3) +
    # geom_histogram(fill = fill, binwidth = bw, closed = "right", center = 0) +
    scale_x_continuous(limits = limits) +
    # scale_y_continuous(limits = c(0, 50)) +
    labs(x = xlab, title = tit) +
    theme_classic()
}

opInfoPlot = function(mtrx = am, cols = 3:6, bw = 0.2, fill = "steelblue",
                      ylab = "information bias", xlab = "opinion",
                      tit = "", limits = c(-1, 1)) {
  mtrx[, cols] %>%
    as_tibble() %>%
    rename(infoBias = 1) %>%
    mutate(infoBias = infoBias - infoNeg) %>%
    ggplot(aes(y = infoBias, x = opinion, col = attention)) +
    geom_point(size = 2, alpha = 0.3) +
    scale_x_continuous(limits = limits) +
    scale_y_continuous(limits = limits) +
    scale_color_gradient2(midpoint = 0.5, mid = "green") +
    labs(x = xlab, y = ylab, title = tit) +
    theme_classic()
}

infoPlot = function(mtrx = am, cols = c(3, 4)) {
  mtrx[, cols] %>%
    as_tibble() %>%
    ggplot() +
    aes(x = infoPos, y = infoNeg) +
    geom_point(alpha = 0.2) +
    theme_classic()
}

plot3dS = function(mtrx = am, cols = 3:v, tit = tit) {
  tb = mtrx[, cols] %>%
    as_tibble()
  scatterplot3d(z = (tb$infoPos - tb$infoNeg), y = tb$attention, x = tb$opinion,
                highlight.3d = T, main = tit, col.grid = "lightblue",
                col.axis = "lightblue")

}

plot3dR = function(mtrx = am, cols = 2:v) {
  tb = mtrx[, cols] %>%
    as_tibble()
  plot3d(z = (tb$infoPos - tb$infoNeg), y = tb$attention, x = tb$opinion,
         type = "s", size = .5, col = tb$acceptance)#"steelblue")

}
# plot3dR()

## Using functions
# Plotting information
infoPlot()
opInfoPlot()

# Plotting opinion
opHist()
plot3dS(tit = "")
plot3dR() %>% print()

# Plotting acceptance
opHist(col = 2, xlab = "Acceptance", limits = c(0, 3))

# Plotting # of connections
cm %>%
  as.vector() %>%
  as.matrix(ncol = 1) %>%
  as_tibble() %>%
  count(V1) %>% arrange(desc(n)) %>%
  as.matrix() %>%
  opHist(col = 2, bw = 1, fill = "steelblue", xlab = "Communication connections",
         limits = c(0, 25))



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
simRound = function(agm = am, cmm = cm, buyingOpinion = T, Amin = 0.5,
                    pia = 0.5, bootstrap = T, forgeting = 0.8, titleStart = "",
                    plotting = T, oh = T, p3d = T, oip = T) {
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
    ## Block for accomodating InfoBias according others opinion:
    # Firstly, we do agent'i':
    if (abs(a1["opinion"] - a2["opinion"]) <= a1["acceptance"]) {
      if (a2["opinion"] > 0) {
        agm[i, "infoPos"] = (agm[i, "infoPos"] + a2["opinion"])
        if (agm[i, "infoPos"] > 1) agm[i, "infoPos"] = 1
      } else {
        agm[i, "infoNeg"] = (agm[i, "infoNeg"] - a2["opinion"])
        if (agm[i, "infoNeg"] > 1) agm[i, "infoNeg"] = 1
      }
    }
    #  Secondly, we do agent'p':
    if (abs(a2["opinion"] - a1["opinion"]) <= a2["acceptance"]) {
      if (a1["opinion"] > 0) {
        agm[p, "infoPos"] = (agm[p, "infoPos"] + a1["opinion"])
        if (agm[p, "infoPos"] > 1) agm[p, "infoPos"] = 1
      } else {
        agm[p, "infoNeg"] = (agm[p, "infoNeg"] - a1["opinion"])
        if (agm[p, "infoNeg"] > 1) agm[p, "infoNeg"] = 1
      }
    }
  } else {
    ## Block for accomodating InfoBias according others opinion:
    #  Firstly, we do agent'i':
    if (abs(a1["infoPos"] - a1["infoNeg"] -
            a2["infoPos"] + a2["infoNeg"]) <= a1["acceptance"]) {
      if (a2["infoNeg"] < a2["infoPos"]) {
        agm[i, "infoPos"] = (agm[i, "infoPos"] + a2["infoPos"] - a2["infoNeg"]) #* forgeting
        #agm[i, "infoNeg"] =  agm[i, "infoNeg"] * forgeting
        if (agm[i, "infoPos"] > 1) agm[i, "infoPos"] = 1
      } else {
        agm[i, "infoNeg"] = (agm[i, "infoNeg"] + a2["infoNeg"] - a2["infoPos"]) #* forgeting
        #agm[i, "infoPos"] =  agm[i, "infoPos"] * forgeting
        if (agm[i, "infoNeg"] > 1) agm[i, "infoNeg"] = 1
      }
    }
      #  Secondly, we do agent'p':
    if (abs(a2["infoPos"] - a2["infoNeg"] -
            a1["infoPos"] + a1["infoNeg"]) <= a2["acceptance"]) {
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
}
  ## Now for each agent update:
  #          attention and
  agm[, "attention"] = (agm[, "infoPos"] + agm[, "infoNeg"]) / attentionDenom
  agm[agm[, "attention"] > 1, "attention"] = 1

  #          opinion of all agents
  agm[, "opinion"] = -agm[, "opinion"]^3 + (agm[, "infoPos"] + agm[, "infoNeg"] - Amin) * (agm[, "opinion"]) + (agm[, "infoPos"] - agm[, "infoNeg"])
  agm[agm[, "opinion"] > 1, "opinion"] = 1
  agm[agm[, "opinion"] < -1, "opinion"] = -1

  #          memory/information
  agm[, "infoPos"] = agm[, "infoPos"] * forgeting
  agm[, "infoNeg"] = agm[, "infoNeg"] * forgeting


  ## Ploting
  if (plotting) {
    if (oip) print(opInfoPlot(tit = titleStart))
    if (oh)  print(opHist(tit = paste(titleStart, "; Sum of information:", round(sum(agm[, c("infoNeg", "infoPos")]), 2), "=", round(sum(agm[, "infoNeg"]), 2), "+", round(sum(agm[, "infoPos"]), 2))))
    if (p3d) print(plot3dS(tit = titleStart))
  }

  ## Giving back updated matrix
  agm
}



# Experiment function -----------------------------------------------------

experiment = function() {



}


# Testing -----------------------------------------------------------------


# am = mx
# am == mx
# am == simRound()

am = mx
opInfoPlot(tit = paste("Round 0; Sum of information:", round(sum(am[, c("infoNeg", "infoPos")]), 2), "=", round(sum(am[, "infoNeg"]), 2), "+", round(sum(am[, "infoPos"]), 2)))
infoHist(tit = paste("Round 0; Sum of information:", round(sum(am[, c("infoNeg", "infoPos")]), 2), "=", round(sum(am[, "infoNeg"]), 2), "+", round(sum(am[, "infoPos"]), 2)))
#doBlock(steps = 100, poop = c(F, F, T, F))
st = Sys.time()
for (i in 1:100) {
  am = simRound(Amin = foldingPoint, pia = communicationRate, forgeting = forgeting,
                buyingOpinion = T, bootstrap = T,
                titleStart = paste0("Round ", i),
                plotting = F, oh = F, oip = T, p3d = F)
  print(paste0(i, ": (sum)", round(sum(am[, c("infoNeg", "infoPos")]), 2),
              "; (-)", round(sum(am[, "infoNeg"]), 2),
              "; (+)", round(sum(am[, "infoPos"]), 2)))
}
print(Sys.time() - st)
plot3dR()
opHist()
plot3dS(tit = "Now")
print(Sys.time() - it)
opHist(col = 2, xlab = "Acceptance", limits = c(0, 3))
infoPlot()
opInfoPlot()

