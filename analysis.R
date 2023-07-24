#### Analysis

## Encoding: windows-1250
## Created:  2023-07-05 FranÈesko
## Edited:   2023-07-24 FranÈesko

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


# Experiment #1 -----------------------------------------------------------

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


for (f in 14:50) {
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

za = lm(SD ~ forgeting * communicationRate,
        data = tb)
xa = lm(SD ~ foldingPoint + forgeting + communicationRate,
        data = tb)
ya = lm(SD ~ foldingPoint * forgeting * communicationRate,
        data = tb)
zb = lm(manhattan ~ forgeting * communicationRate,
        data = tb)
xb = lm(manhattan ~ foldingPoint + forgeting + communicationRate,
        data = tb)
yb = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
        data = tb)
zc = lm(ESBG ~ forgeting * communicationRate,
        data = tb)
xc = lm(ESBG ~ foldingPoint + forgeting + communicationRate,
        data = tb)
yc = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
        data = tb)

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
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)

ty = tb %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric()) %>%
  group_by(communicationRate, forgeting, foldingPoint) %>%
  summarise(SD = mean(SD), manhattan = mean(manhattan), ESBG = mean(ESBG)) %>%
  arrange(communicationRate, foldingPoint, forgeting)

# 3D graphs
plot3d(x = ty$communicationRate, y = ty$foldingPoint, z = ty$ESBG, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty$communicationRate, y = ty$foldingPoint, z = ty$SD, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty$communicationRate, y = ty$foldingPoint, z = ty$manhattan, type = "s", size = .5, col = rainbow(4))


## Final plots
# All polarization measures:
png("01-allMeasures.png")
tb %>%  select(manhattan, SD, ESBG) %>%
  scatterplot3d(highlight.3d = T, main = "All polarization measures")
dev.off()

png("02-SD.png")
tx %>%  select(forgeting, communicationRate, pra) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: SD", zlab = "Prediction (R^2: 86.7%)")
dev.off()

png("03-Manhattan.png")
tx %>%  select(forgeting, communicationRate, prb) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: Manhattan", zlab = "Prediction (R^2: 95.8%)")
dev.off()

png("04-ESBG.png")
tx %>%  select(forgeting, communicationRate, prc) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: ESBG", zlab = "Prediction (R^2: 76.0%)")
dev.off()



# Information measures ----------------------------------------------------

# Discretization of dataset
td = tx %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = communicationRate * 10000 + forgeting * 100 + foldingPoint )

# Normalized mutual information
exp(mutinformation(td[, 13], td[, 5])) %>% log(base = 80) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td[, 13], td[, 4])) %>% log(base = 80) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td[, 13], td[, 6])) %>% log(base = 80) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(5, 4, 6)) {
  val = (100 * mutinformation(td[, 13], td[, v]) / mutinformation(td[, v], td[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb, ya, yc, type = "text", omit = 1:85,
          add.lines = "All coefficients were supressed!")
# Note: Ah yeah... normalized mutual info < fraction of mutual info < R^2.
#       Why it is so? Why is so apparent difference between R^2 and info measures?
#       R^2 tells us how much variability from mean we explained, so if in case of
#       'Manhattan' explained 96%, why the fraction of mutual information is only 63%?
#       Is it the case how entropy works, i.e. for explaining the resting 4% of variability
#       we need the resting 37%? Or information does not explain only variability from mean,
#       but also the mean itself and for this we need that 30+%?
#       Information seems more relaxed than R^2, but in case of our mode it works otherwise...
#



# Experiment #2 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

load("results2_seeds_1.RData")
tb2 = results %>% drop_na()

for (f in 2:50) {
  load(paste0("results2_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb2 = tb2 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb2 = tb2 %>%
  mutate(
    ESBG = ESBG / 2,
    reinforce = factor(reinforce),
    foldingPoint = factor(foldingPoint),
    forgeting = factor(forgeting),
    communicationRate = factor(communicationRate))



# Analysis ----------------------------------------------------------------

tb2 %>%
  select(reinforce, SD:ESBG) %>%
  pivot_longer(cols = SD:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_grid(vars(reinforce), vars(Measure), scales = "free", labeller = "label_both") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  theme_classic()

# NOTE: Ah yeah, seems Han van der Maas is true -- reinforcing of own opinion makes difference.
#       ESBG is more concentrated around 0.2, but also having heavy long tail up to 0.6,
#       Manhattans is not changed that much,
#       SD has two combined distributions, one very sharp (high curtosis) around 0.35 seems stable,
#       but the other the wider is after reinforcing even wider and mean is moved toward 0.55.
#
#       So we know that 'reinforce' works, makes difference and is needed.
#       Then let's filter out reinforce==FALSE, i.e. let' keep only reinforce==TRUE:
#

tb2 = filter(tb2, reinforce == "TRUE")
# tb2 = filter(tb2, reinforce == "FALSE")


# regression --------------------------------------------------------------

za2 = lm(SD ~ forgeting * communicationRate,
        data = tb2)
xa2 = lm(SD ~ foldingPoint + forgeting + communicationRate,
        data = tb2)
ya2 = lm(SD ~ foldingPoint * forgeting * communicationRate,
        data = tb2)
zb2 = lm(manhattan ~ forgeting * communicationRate,
        data = tb2)
xb2 = lm(manhattan ~ foldingPoint + forgeting + communicationRate,
        data = tb2)
yb2 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
        data = tb2)
zc2 = lm(ESBG ~ forgeting * communicationRate,
        data = tb2)
xc2 = lm(ESBG ~ foldingPoint + forgeting + communicationRate,
        data = tb2)
yc2 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
        data = tb2)

## Final tables
stargazer(za2, xa2, ya2,
          omit.stat = c("f"), type = "text", omit = 27:1605,
          add.lines = "Coefficients for interactions were supressed!")

stargazer(zb2, xb2, yb2,
          omit.stat = c("f"), type = "text", omit = 27:1605,
          add.lines = "Coefficients for interactions were supressed!")

stargazer(zc2, xc2, yc2,
          omit.stat = c("f"), type = "text", omit = 27:1605,
          add.lines = "Coefficients for interactions were supressed!")



# Visualizations ----------------------------------------------------------

tx2 = tibble(tb2, pra = predict(ya2), prb = predict(yb2), prc = predict(yc2)) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)

ty2 = tb2 %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric()) %>%
  group_by(communicationRate, forgeting, foldingPoint) %>%
  summarise(SD_sd = sd(SD), manhattan_sd = sd(manhattan), ESBG_sd = sd(ESBG),
            SD = mean(SD), manhattan = mean(manhattan), ESBG = mean(ESBG)) %>%
  arrange(communicationRate, foldingPoint, forgeting)

# 3D graphs
plot3d(x = ty2$communicationRate, y = ty2$foldingPoint, z = ty2$ESBG, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty2$communicationRate, y = ty2$foldingPoint, z = ty2$SD, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty2$communicationRate, y = ty2$foldingPoint, z = ty2$manhattan, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty2$SD, y = ty2$manhattan, z = ty2$ESBG, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty2$communicationRate, y = ty2$foldingPoint, z = ty2$ESBG_sd, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty2$communicationRate, y = ty2$foldingPoint, z = ty2$SD_sd, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty2$communicationRate, y = ty2$foldingPoint, z = ty2$manhattan_sd, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty2$SD_sd, y = ty2$manhattan_sd, z = ty2$ESBG_sd, type = "s", size = .5, col = rainbow(8))



## Final plots
# All polarization measures:
png("11-allMeasures.png")
tb2 %>%  select(manhattan, SD, ESBG) %>%
  scatterplot3d(highlight.3d = T, main = "All polarization measures")
dev.off()

png("12-SD.png")
tx2 %>%  select(forgeting, communicationRate, pra) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: SD", zlab = "Prediction (R^2: 98.8%)")
dev.off()

png("13-Manhattan.png")
tx2 %>%  select(forgeting, communicationRate, prb) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: Manhattan", zlab = "Prediction (R^2: 99.9%)")
dev.off()

png("14-ESBG.png")
tx2 %>%  select(forgeting, communicationRate, prc) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: ESBG", zlab = "Prediction (R^2: 98.8%)")
dev.off()



# Information measures ----------------------------------------------------

# Discretization of dataset
td2 = tx2 %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = communicationRate * 10000 + forgeting * 100 + foldingPoint )

# Normalized mutual information
exp(mutinformation(td2[, 13], td2[, 5])) %>% log(base = 800) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td2[, 13], td2[, 4])) %>% log(base = 800) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td2[, 13], td2[, 6])) %>% log(base = 800) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(5, 4, 6)) {
  val = (100 * mutinformation(td2[, 13], td2[, v]) / mutinformation(td2[, v], td2[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td2)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb2, ya2, yc2, type = "text", omit = 1:805,
          add.lines = "All coefficients were supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.
#       But now with one exception: SD --> R^2 < fraction of mutual info.
#       Why it is so? Why is so apparent difference between R^2 and info measures?
#
#       Now, normalized mutual info is much lower, but it's probably because in EXP1 the
#       base for log was 80, now in EXP2 it is 800, and we know that higher base/more bars means
#       the entropy of normal distribution is closer to entropy of uniform distribution,
#       i.e. entropy is higher and so information is lower.
#
#       Fraction of possible information explained is now higher in comparison with EXP1. Why?
#       I hope it is cos' we now work in smaller parameter space and mutual info is more precise.
#       But is it true?
#
#       R^2 is now similar except SD, there is very different, by 27 pcp lower, so
#       we are able still predict manhattan and ESBG, but SD is very poorer predicted.
#       Why? evidently there is some variability in overall SD, which we now hardly predict.
#       But why it is not so in ESBG and Manhattan?
#       I looked at standard deviation ('sd') for SD and Manhattan and I found that with lowering
#       folding point to 0 'sd' of SD increases dramatically and systematically,
#       but not 'sd' of Manhattan, and I hope I know why:
#       We might be sure that for low folding points the population will be polarized,
#       i.e. all agents ends up on extreme poles, regression models predict it correctly,
#       so Manhattan has high R^2, but what is hard to predict is ration, how agents
#       divide to one pole or another and that's why 'sd' of SD is high. So:
#       we can predict well the position of two poles where agents will end up,
#       but it is hard to predict ratio of these two polarized groups/sub-populations.
#
#       BTW, predictability is very dependent on 'reinforcing': When we add it to the
#       model, R^2 is 99%! So, yeah, reinforcing makes difference, so for now I adapted code
#       that I filtered out all data without reinforcing (I leave just the initial graph showing
#       how distributions of ESBG, Manhattan and SD changes with reinforcing).
#
#       R^2 tells us how much variability from mean we explained, so if in case of
#       'Manhattan' explained 96%, why the fraction of mutual information is only 63%?
#       Is it the case how entropy works, i.e. for explaining the resting 4% of variability
#       we need the resting 37%? Or information does not explain only variability from mean,
#       but also the mean itself and for this we need that 30+%?
#       Information seems more relaxed than R^2, but in case of our models it works otherwise...
#
#       OK! One important piece of info was missing:
#       We also tested utility of reinforcing algorithm suggested by Han van der Maas!
#       If we include this variable into models, they improve rapidly.
#       We should also include it into info measures...
#       Since our main motivation was just test whether reinforcing submodel makde difference and
#       we proved it made, we can now filter out data without reinforcing of own opinion.
#       So I made it and then reanalyze everything again, just on data using the reinforcing.
#       So, how it is on filtered data from EXP2?
#
#       Firstly, now we use for prediction the only used parameters: forgeting, folding point and commRate.
#       So the prediction should be better since now variance in 'aa' and 'aasd'.
#
#       Except normalized mutual information everything is better. The explanation, why normalized info
#       is slightly worse than in EXP1 holds still true -- higher base increase entropy and lowers information.
#
#       Fraction of possible explained information is now higher, much higher (21--29 pcpt),
#       probably because we used all varied predictors and we operate in narrowwer but denser state space.
#
#       R^2 is almost 100% because we use all used parameters and probably because reinforcing of own opinion makes
#       final position of agent much more predictable than without it. 'sd' of all three resulting measures
#       for all parameters combinations is very low 0.06 max, but usually close to 0.01!

ty2 %>%
  ungroup() %>%
  select(SD_sd:ESBG_sd) %>%
  pivot_longer(cols = everything(), names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_wrap(vars(Measure), scales = "free_y", ncol = 2) +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  labs(title = "SD of polarization measures for each combination of parameters (w/ reinforcing)") +
  # labs(title = "SD of polarization measures for each combination of parameters (W/OUT reinforcing)") +
  theme_classic()
ggsave("15-withReinforcingSD.png", width = 7.5, height = 5.5)
# ggsave("16-withoutReinforcingSD.png", width = 7.5, height = 5.5)

# NOTE: That w/out reinforcing Manhattan is predicted same -- same R^2, fraction of
#       maximum possible explained information and also the normalized mutual information.
#       But SD and ESBG are predicted even better: R^2 is still high, but fraction and mutual information
#       are higher than w/ reinforcing.
#
#       The result is that both modes of simulation (w/ or w/out reinforcing) are of same predictability,
#       but when we mix them together and not use 'reinforce' as parameter, then the predictability is very
#       lowered, especially in case of SD.
#



# Extra code  -------------------------------------------------------------

ma = lm(SD ~ foldingPoint + forgeting + communicationRate + aa + aasd + opDistribution,
        data = tb)
mb = lm(manhattan ~ foldingPoint + forgeting + communicationRate + aa + aasd + opDistribution,
        data = tb)
mc = lm(ESBG ~ foldingPoint + forgeting + communicationRate + aa + aasd + opDistribution,
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



# plot3d(x = tx$communicationRate, y = tx$forgeting, z = tx$pr, type = "s", size = .5, col = hcl.colors(5, palette = "Tropic"))
# plot3d(x = tx$communicationRate, y = tx$forgeting, z = tx$manhattan, type = "s", size = .5, col = tx$foldingPoint)
#

plot3d(x = tb$SD, y = tb$manhattan, z = tb$ESBG, type = "s", size = .5, col = rainbow(80000))

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

tx %>%
  ggplot() +
  aes(x = (pra), y = (diff_a)) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = (prb), y = (diff_b)) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()

tx %>%
  ggplot() +
  aes(x = (prc), y = (diff_c)) +
  geom_bin_2d(binwidth = c(0.025, 0.025)) +
  scale_fill_viridis_c() +
  theme_classic()



### Information measures
# Shannon entropy of results:
for (v in 1:13) {
  td[, v] %>% infotheo::entropy() %>% round(2) %>%
    paste0("Entropy of variable ", names(td)[v], " is ", .) %>% print()
}

## Conditional information
# SD
exp(condinformation(td[, 13], td[, 4])) %>% log(base = 80)
exp(condinformation(td[, 3], td[, 4])) %>% log(base = 5)
condinformation(td[, 3], td[, 4], td[, 2])
condinformation(td[, 3], td[, 4], td[, 1])
condinformation(td[, 3], td[, 4], td[, 10])
exp(condinformation(td[, 2], td[, 4])) %>% log(base = 4)
condinformation(td[, 2], td[, 4], td[, 3])
condinformation(td[, 2], td[, 4], td[, 1])
condinformation(td[, 2], td[, 4], td[, 10])
exp(condinformation(td[, 1], td[, 4])) %>% log(base = 4)
condinformation(td[, 1], td[, 4], td[, 3])
condinformation(td[, 1], td[, 4], td[, 2])
condinformation(td[, 1], td[, 4], td[, 10])

# Manhattan
exp(condinformation(td[, 13], td[, 5])) %>% log(base = 80)
exp(condinformation(td[, 3], td[, 5])) %>% log(base = 5)
condinformation(td[, 3], td[, 5], td[, 2])
condinformation(td[, 3], td[, 5], td[, 1])
condinformation(td[, 3], td[, 5], td[, 11])
exp(condinformation(td[, 2], td[, 5])) %>% log(base = 4)
condinformation(td[, 2], td[, 5], td[, 3])
condinformation(td[, 2], td[, 5], td[, 1])
condinformation(td[, 2], td[, 5], td[, 11])
exp(condinformation(td[, 1], td[, 5])) %>% log(base = 4)
condinformation(td[, 1], td[, 5], td[, 3])
condinformation(td[, 1], td[, 5], td[, 2])
condinformation(td[, 1], td[, 5], td[, 11])

# ESBG
exp(condinformation(td[, 13], td[, 6])) %>% log(base = 80)
exp(condinformation(td[, 3], td[, 6])) %>% log(base = 5)
exp(condinformation(td[, 3], td[, 6], td[, 2])) %>% log(base = 5)  ## Is this base correct?
exp(condinformation(td[, 3], td[, 6], td[, 1])) %>% log(base = 5)  ## Why is then possible that 2 vars predicts more than 3?
exp(condinformation(td[, 3], td[, 6], td[, 12])) %>% log(base = 5)
exp(condinformation(td[, 2], td[, 6])) %>% log(base = 4)
exp(condinformation(td[, 2], td[, 6], td[, 3])) %>% log(base = 4)
exp(condinformation(td[, 2], td[, 6], td[, 1])) %>% log(base = 4)
exp(condinformation(td[, 2], td[, 6], td[, 12])) %>% log(base = 4)
exp(condinformation(td[, 1], td[, 6])) %>% log(base = 4)
exp(condinformation(td[, 1], td[, 6], td[, 3])) %>% log(base = 4)
exp(condinformation(td[, 1], td[, 6], td[, 2])) %>% log(base = 4)
exp(condinformation(td[, 1], td[, 6], td[, 12])) %>% log(base = 4)


## Combination -- which fraction of full information explains?
# Mutual information
mutinformation(td[, c(1:13)]) %>% round(2)




## Testing materials
(rnorm(1000000) %>% entropy::discretize(3) %>% entropy::entropy(unit = "log10")) / (runif(1000000) %>% entropy::discretize(3) %>% entropy::entropy(unit = "log10"))
(rnorm(1000000) %>% entropy::discretize(10) %>% entropy::entropy(unit = "log10")) / (runif(1000000) %>% entropy::discretize(10) %>% entropy::entropy(unit = "log10"))
(rnorm(1000000) %>% entropy::discretize(100) %>% entropy::entropy(unit = "log10")) / (runif(1000000) %>% entropy::discretize(100) %>% entropy::entropy(unit = "log10"))
(rnorm(1000000) %>% entropy::discretize(1000) %>% entropy::entropy(unit = "log10")) / (runif(1000000) %>% entropy::discretize(1000) %>% entropy::entropy(unit = "log10"))
(rnorm(1000000) %>% entropy::discretize(10000) %>% entropy::entropy(unit = "log10")) / (runif(1000000) %>% entropy::discretize(10000) %>% entropy::entropy(unit = "log10"))

# All fractions
for (v in 4:12) {
  val = (100 * mutinformation(td[, 13], td[, v]) / mutinformation(td[, v], td[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td)[v], "'.") %>% print()
}

# Single 2D discretization in use
discretize2d(tb$manhattan, tb$SD, 10, 10, c(0, 1), c(0, 1))
discretize2d(tb$manhattan, tb$SD, 10, 10, c(0, 1), c(0, 1)) %>%
  mi.plugin(unit = "log10")
discretize2d(tb$ESBG, tb$SD, 10, 10, c(0, 1), c(0, 1)) %>%
  mi.plugin(unit = "log10")
discretize2d(tb$manhattan, tb$ESBG, 10, 10, c(0, 1), c(0, 1)) %>%
  mi.plugin(unit = "log10")







