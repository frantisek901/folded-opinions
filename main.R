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
  am[i, 5] = rnorm(n = 1, mean = 0, sd = 0.2)
}



# Graphs ------------------------------------------------------------------

## Histogram of opinions
# Preparing function
opHist = function(mtrx = am, col = v) {
  mtrx[,v] %>%
    as_tibble() %>%
    rename(opinion = value) %>%
    ggplot(aes(x = opinion)) +
    geom_histogram(binwidth = 0.1) +
    theme_classic()
}

# Using function
opHist()

