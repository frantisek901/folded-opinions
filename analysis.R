#### Analysis

## Encoding: windows-1250
## Created:  2023-07-05 FranÈesko
## Edited:   2023-07-13 FranÈesko

## NOTES
##


# Head --------------------------------------------------------------------

rm(list = ls())

# Packages
library(tidyverse)
library(stargazer)
library(rgl)
library(scatterplot3d)
library(entropy)
library(infotheo)
library(philentropy)

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

load("results_seeds_08.RData")

tb = tb %>%
  add_row(results %>% drop_na() %>% filter(seed < 9))


load("results_seeds_09-11.RData")

tb = tb %>%
  add_row(results %>% drop_na() %>% filter(seed < 12, seed > 8)) %>% unique()

load("results_seeds_12-13.RData")

tb = tb %>%
  add_row(results %>% drop_na() %>% filter(seed < 14, seed > 11))


for (f in 14:22) {
  load(paste0("results_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb = tb %>%
    add_row(results)
}




# Normalization of ESBG and factorisation of other variables:
tb = tb %>%
  mutate(
    ESBG = ESBG / 2,
    opDistribution = factor(opDistribution),
    foldingPoint = factor(foldingPoint),
    aa = factor(aa),
    aasd = factor(aasd),
    forgeting = factor(forgeting),
    communicationRate = factor(communicationRate),
    seed = factor(seed))



# Analysis ----------------------------------------------------------------

tb %>%
  select(SD:fractured) %>%
  pivot_longer(cols = everything(), names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_wrap(vars(Measure), ncol = 2, scales = "free_x") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  # scale_x_continuous(limits = c(0, 1.8)) +
  theme_classic()



# regression --------------------------------------------------------------

ma = lm(SD ~ foldingPoint + forgeting + communicationRate + aa + aasd + opDistribution,
        data = tb)
xa = lm(SD ~ foldingPoint + forgeting + communicationRate,
        data = tb)
ya = lm(SD ~ foldingPoint * forgeting * communicationRate,
        data = tb)
za = lm(SD ~ forgeting * communicationRate,
        data = tb)
mb = lm(manhattan ~ foldingPoint + forgeting + communicationRate + aa + aasd + opDistribution,
        data = tb)
xb = lm(manhattan ~ foldingPoint + forgeting + communicationRate,
        data = tb)
yb = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
        data = tb)
zb = lm(manhattan ~ forgeting * communicationRate,
        data = tb)
mc = lm(ESBG ~ foldingPoint + forgeting + communicationRate + aa + aasd + opDistribution,
        data = tb)
xc = lm(ESBG ~ foldingPoint + forgeting + communicationRate,
        data = tb)
yc = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
        data = tb)
zc = lm(ESBG ~ forgeting * communicationRate,
        data = tb)
# md = lm(fractured ~ foldingPoint + forgeting + communicationRate + aa + aasd + opDistribution + seed,
#         data = tb)

ma1 = lm(SD ~ forgeting + foldingPoint * communicationRate + aa + aasd + opDistribution,
         data = tb)
mb1 = lm(manhattan ~ forgeting + foldingPoint * communicationRate + aa + aasd + opDistribution,
         data = tb)
mc1 = lm(ESBG ~ forgeting + foldingPoint * communicationRate + aa + aasd + opDistribution,
         data = tb)

ma2 = lm(SD ~ foldingPoint + forgeting * communicationRate + aa + aasd + opDistribution,
        data = tb)
mb2 = lm(manhattan ~ foldingPoint + forgeting * communicationRate + aa + aasd + opDistribution,
        data = tb)
mc2 = lm(ESBG ~ foldingPoint + forgeting * communicationRate + aa + aasd + opDistribution,
        data = tb)

ma3 = lm(SD ~ foldingPoint * forgeting + communicationRate + aa + aasd + opDistribution,
        data = tb)
mb3 = lm(manhattan ~ foldingPoint * forgeting + communicationRate + aa + aasd + opDistribution,
        data = tb)
mc3 = lm(ESBG ~ foldingPoint * forgeting + communicationRate + aa + aasd + opDistribution,
        data = tb)

ma4 = lm(SD ~ foldingPoint * forgeting * communicationRate + aa + aasd + opDistribution,
        data = tb)
mb4 = lm(manhattan ~ foldingPoint * forgeting * communicationRate + aa + aasd + opDistribution,
        data = tb)
mc4 = lm(ESBG ~ foldingPoint * forgeting * communicationRate + aa + aasd + opDistribution,
        data = tb)

# stargazer(ma, mb, mc, #md,
#           type = "text")

stargazer(xa, ma, ma1, ma2, ma3, ma4,
          omit.stat = c("f", "ser"), type = "text")

stargazer(xb, mb, mb1, mb2, mb3, mb4,
          omit.stat = c("f", "ser"), type = "text")

stargazer(xc, mc, mc1, mc2, mc3, mc4,
          omit.stat = c("f", "ser"), type = "text")

## Final tables
stargazer(za, xa, ya,
          omit.stat = c("f"), type = "text", omit = 12:85,
          add.lines = "Coefficients for interactions were supressed!")

stargazer(zb, xb, yb,
          omit.stat = c("f"), type = "text", omit = 12:85,
          add.lines = "Coefficients for interactions were supressed!")

stargazer(zc, xc, yc,
          omit.stat = c("f"), type = "text", omit = 12:85,
          add.lines = "Coefficients for interactions were supressed!")



# Visualizations ----------------------------------------------------------

tx = tibble(tb, pra = predict(ya), prb = predict(yb), prc = predict(yc)) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)

# plot3d(x = tx$communicationRate, y = tx$forgeting, z = tx$pr, type = "s", size = .5, col = hcl.colors(5, palette = "Tropic"))
# plot3d(x = tx$communicationRate, y = tx$forgeting, z = tx$manhattan, type = "s", size = .5, col = tx$foldingPoint)
#
# ty = tb %>%
#   mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
#          forgeting = as.character(forgeting) %>% as.numeric()) %>%
#   group_by(communicationRate, forgeting, foldingPoint) %>%
#   summarise(SD = mean(SD), manhattan = mean(manhattan), ESBG = mean(ESBG)) %>%
#   arrange(ESBG)
#
# plot3d(x = ty$communicationRate, y = ty$forgeting, z = ty$ESBG, type = "s", size = .5, col = rainbow(4))
#
# plot3d(x = tb$SD, y = tb$manhattan, z = tb$ESBG, type = "s", size = .5, col = rainbow(25600))


## Final plots
# All polarization measures:
png("01-allMeasures.png")
tb %>%  select(manhattan, SD, ESBG) %>%
  scatterplot3d(highlight.3d = T, main = "All polarization measures")
dev.off()

png("02-SD.png")
tx %>%  select(forgeting, communicationRate, pra) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: SD", zlab = "Prediction")
dev.off()

png("03-Manhattan.png")
tx %>%  select(forgeting, communicationRate, prb) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: Manhattan", zlab = "Prediction")
dev.off()

png("04-ESBG.png")
tx %>%  select(forgeting, communicationRate, prc) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: ESBG", zlab = "Prediction")
dev.off()


## Some other plots
tx %>%
  ggplot() +
  aes(x = SD, y = manhattan) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = SD, y = ESBG) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = ESBG, y = manhattan) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = diff_a, y = diff_b) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = diff_a, y = diff_c) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = diff_c, y = diff_b) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = (SD), y = (pra)) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = (manhattan), y = (prb)) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = (ESBG), y = (prc)) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()



# Information measures ----------------------------------------------------

# Discretization of dataset
td = tx %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth")

# Shannon entropy of results:
for (v in 1:12) {
  td[, v] %>% infotheo::entropy() %>% round(2) %>%
    paste0("Entropy of variable ", names(td)[v], " is ", .) %>% print()
}

## Conditional information
# SD
condinformation(td[, 3], td[, 4])
condinformation(td[, 3], td[, 4], td[, 2])
condinformation(td[, 3], td[, 4], td[, 1])
condinformation(td[, 3], td[, 4], td[, 10])
condinformation(td[, 2], td[, 4])
condinformation(td[, 2], td[, 4], td[, 3])
condinformation(td[, 2], td[, 4], td[, 1])
condinformation(td[, 2], td[, 4], td[, 10])
condinformation(td[, 1], td[, 4])
condinformation(td[, 1], td[, 4], td[, 3])
condinformation(td[, 1], td[, 4], td[, 2])
condinformation(td[, 1], td[, 4], td[, 10])

# Manhattan
condinformation(td[, 3], td[, 5])
condinformation(td[, 3], td[, 5], td[, 2])
condinformation(td[, 3], td[, 5], td[, 1])
condinformation(td[, 3], td[, 5], td[, 11])
condinformation(td[, 2], td[, 5])
condinformation(td[, 2], td[, 5], td[, 3])
condinformation(td[, 2], td[, 5], td[, 1])
condinformation(td[, 2], td[, 5], td[, 11])
condinformation(td[, 1], td[, 5])
condinformation(td[, 1], td[, 5], td[, 3])
condinformation(td[, 1], td[, 5], td[, 2])
condinformation(td[, 1], td[, 5], td[, 11])

# ESBG
condinformation(td[, 3], td[, 6])
condinformation(td[, 3], td[, 6], td[, 2])
condinformation(td[, 3], td[, 6], td[, 1])
condinformation(td[, 3], td[, 6], td[, 12])
condinformation(td[, 2], td[, 6])
condinformation(td[, 2], td[, 6], td[, 3])
condinformation(td[, 2], td[, 6], td[, 1])
condinformation(td[, 2], td[, 6], td[, 12])
condinformation(td[, 1], td[, 6])
condinformation(td[, 1], td[, 6], td[, 3])
condinformation(td[, 1], td[, 6], td[, 2])
condinformation(td[, 1], td[, 6], td[, 12])


# Tests
(rnorm(1000000) %>% entropy::discretize(10) %>% entropy::entropy(unit = "log10")) / (runif(1000000) %>% entropy::discretize(10) %>% entropy::entropy(unit = "log10"))
(rnorm(1000000) %>% entropy::discretize(100) %>% entropy::entropy(unit = "log10")) / (runif(1000000) %>% entropy::discretize(100) %>% entropy::entropy(unit = "log10"))
(rnorm(1000000) %>% entropy::discretize(1000) %>% entropy::entropy(unit = "log10")) / (runif(1000000) %>% entropy::discretize(1000) %>% entropy::entropy(unit = "log10"))
