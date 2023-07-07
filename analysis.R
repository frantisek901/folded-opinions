#### Analysis

##
##
##

## NOTES
##


# Head --------------------------------------------------------------------

rm(list = ls())

# Packages
library(tidyverse)
library(stargazer)


# Loading data ------------------------------------------------------------

load("results_seeds_01-04.RData")

tb = results %>%
  drop_na() %>%
  filter(seed < 5)

load("results_seeds_05-06.RData")

tb = tb %>%
  add_row(results %>% drop_na() %>% filter(seed < 7))

load("results_seeds_07.RData")

tb = tb %>%
  add_row(results %>% drop_na() %>% filter(seed < 8))



# Analysis ----------------------------------------------------------------

tb %>%
  select(SD:fractured) %>%
  pivot_longer(cols = everything(), names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_wrap(vars(Measure), ncol = 2, scales = "free_x") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  scale_x_continuous(limits = c(0, 2)) +
  theme_classic()



# regression --------------------------------------------------------------

ma = lm(SD ~ opDistribution + foldingPoint + aa + aasd + forgeting + communicationRate,
        data = tb)
mb = lm(manhattan ~ opDistribution + foldingPoint + aa + aasd + forgeting + communicationRate,
        data = tb)
mc = lm(ESBG ~ opDistribution + foldingPoint + aa + aasd + forgeting + communicationRate,
        data = tb)
md = lm(fractured ~ opDistribution + foldingPoint + aa + aasd + forgeting + communicationRate,
        data = tb)

stargazer(ma, mb, mc, md, type = "text")










