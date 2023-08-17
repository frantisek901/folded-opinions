#### Analysis

## Encoding: windows-1250
## Created:  2023-07-05 FranÈesko
## Edited:   2023-08-11 FranÈesko

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



# Experiment #3 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 50

# Core of 'tb3' consists of relevant variables and observations from 'tb2'
# which stores results of second experiment:
tb3 = tb2 %>%
  filter(
    seed <= completedSeeds,
    reinforce == "TRUE",
    foldingPoint %in% c(.05, .35, .65, .95)) %>%
  select(
    "seed", "reinforce", "communicationRate", "forgeting",
    "foldingPoint", "SD", "manhattan", "ESBG","opDistribution") %>%
  mutate(
    forget_sd = 0,
    ESBG = ESBG * 2,  # We must de-normalize ESBG, because we will normalized joint dataset.
    reinforce = as.logical(reinforce),
    across(communicationRate:foldingPoint, ~ as.character(.x) %>% as.numeric())) %>%
  relocate(forget_sd, .after = forgeting)

# Adding data from the third experiment:
for (f in 1:completedSeeds) {
  load(paste0("results3_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb3 = tb3 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb3 = tb3 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:foldingPoint, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

tb3 %>%
  select(forgeting, forget_sd, SD:ESBG) %>%
  pivot_longer(cols = SD:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_grid(vars(#forgeting,
                  forget_sd), vars(Measure), scales = "free", labeller = "label_both") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  theme_classic()
# ggsave("00-try.png", height = 40, width = 6)

# NOTE: Mhmmm... that's interesting -- there is decline of 'zero polarization' isle
#       (ESBG==0, SD==0, Manhattan==0) with increase of 'forget_sd',
#       but from analysis at the end of this script it is
#       obvious, that with higher SD of remembering/forgetting we get lower average remembering/forgetting.
#       So, on the average we get lower remembering/higher forgetting with higher SD,
#       I thought from previous analyses that higher remembering/lower forgeting anticipates
#       higher polarization, but it is not like that here.
#       Also in the following graph we show that paradoxically higher remembering/lower forgeting
#       brings LOWER polarization. My intuition was otherwise, if people remember more (forget less),
#       then they more remember + and - info, which brings more attention and forces them into polarization.
#       But as noted, we see that optimal value for remembering forgeting is between 0.45 and 0.7,
#       under and up of these values polarization measures are higher.
tb3 %>%
  mutate(forget_sd = factor(forget_sd)) %>%
  group_by(forget_sd, forgeting) %>%
  summarise(ESBG = mean(ESBG), SD = mean(SD), Manhattan = mean(manhattan)) %>%
  ungroup() %>%
  pivot_longer(Manhattan:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = forgeting, y = forget_sd, size = value * 30,
      label = round(value, 2), col = Measure) +
  facet_grid(vars(Measure)) +
  geom_point(alpha = 0.6) +
  geom_text(col = "black", size = 5) +
  scale_size_identity() +
  guides(col = "none") +
  theme_classic()
# We also see

# regression --------------------------------------------------------------

za3 = lm(SD ~  forgeting * communicationRate,
         data = tb3)
xa3 = lm(SD ~ foldingPoint + forgeting + communicationRate,
         data = tb3)
ya3 = lm(SD ~ foldingPoint * forgeting * communicationRate,
         data = tb3)
zb3 = lm(manhattan ~ forgeting * communicationRate,
         data = tb3)
xb3 = lm(manhattan ~ foldingPoint + forgeting + communicationRate,
         data = tb3)
yb3 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
         data = tb3)
zc3 = lm(ESBG ~ forgeting * communicationRate,
         data = tb3)
xc3 = lm(ESBG ~ foldingPoint + forgeting + communicationRate,
         data = tb3)
yc3 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
         data = tb3)

## Final tables
stargazer(za3, xa3, ya3,
          omit.stat = c("f"), type = "text", omit = 21:1605,
          add.lines = "Coefficients for interactions were supressed!")

stargazer(zb3, xb3, yb3,
          omit.stat = c("f"), type = "text", omit = 21:1605,
          add.lines = "Coefficients for interactions were supressed!")

stargazer(zc3, xc3, yc3,
          omit.stat = c("f"), type = "text", omit = 21:1605,
          add.lines = "Coefficients for interactions were supressed!")
# Note: OK, it seems that omitting 'forget_sd' makes no difference,
#       so we might not play with this variable...
#



# Visualizations ----------------------------------------------------------

tx3 = tibble(tb3, pra = predict(ya3), prb = predict(yb3), prc = predict(yc3)) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)

ty3 = tb3 %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric()) %>%
  group_by(communicationRate, forgeting, forget_sd, foldingPoint) %>%
  summarise(SD_sd = sd(SD), manhattan_sd = sd(manhattan), ESBG_sd = sd(ESBG),
            SD = mean(SD), manhattan = mean(manhattan), ESBG = mean(ESBG)) %>%
  arrange(communicationRate, foldingPoint, forget_sd, forgeting)

# 3D graphs
plot3d(x = ty3$communicationRate, y = ty3$foldingPoint, z = ty3$ESBG, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty3$communicationRate, y = ty3$foldingPoint, z = ty3$SD, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty3$communicationRate, y = ty3$foldingPoint, z = ty3$manhattan, type = "s", size = .5, col = rainbow(8))
plot3d(x = ty3$SD, y = ty3$manhattan, z = ty2$ESBG, type = "s", size = .5, col = rainbow(8))
# plot3d(x = ty3$communicationRate, y = ty3$foldingPoint, z = ty3$ESBG_sd, type = "s", size = .5, col = rainbow(8))
# plot3d(x = ty3$communicationRate, y = ty3$foldingPoint, z = ty3$SD_sd, type = "s", size = .5, col = rainbow(8))
# plot3d(x = ty3$communicationRate, y = ty3$foldingPoint, z = ty3$manhattan_sd, type = "s", size = .5, col = rainbow(8))
# plot3d(x = ty3$SD_sd, y = ty3$manhattan_sd, z = ty3$ESBG_sd, type = "s", size = .5, col = rainbow(8))



## Final plots
# All polarization measures:
png("21-allMeasures.png")
tb3 %>%  select(manhattan, SD, ESBG) %>%
  scatterplot3d(highlight.3d = T, main = "All polarization measures")
dev.off()

png("22-SD.png")
tx3 %>%  select(forgeting, communicationRate, pra) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: SD", zlab = "Prediction (R^2: 98.8%)")
dev.off()

png("23-Manhattan.png")
tx3 %>%  select(forgeting, communicationRate, prb) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: Manhattan", zlab = "Prediction (R^2: 99.9%)")
dev.off()

png("24-ESBG.png")
tx3 %>%  select(forgeting, communicationRate, prc) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: ESBG", zlab = "Prediction (R^2: 98.8%)")
dev.off()



# Information measures ----------------------------------------------------

# Discretization of dataset
td3 = tx3 %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = communicationRate * 10000 + forgeting * 100 + foldingPoint )

# Normalized mutual information
exp(mutinformation(td3[, 13], td3[, 5])) %>% log(base = 320) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td3[, 13], td3[, 4])) %>% log(base = 320) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td3[, 13], td3[, 6])) %>% log(base = 320) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(5, 4, 6)) {
  val = (100 * mutinformation(td3[, 13], td3[, v]) / mutinformation(td3[, v], td3[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td3)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb3, ya3, yc3, type = "text", omit = 1:805,
          add.lines = "All coefficients were supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.
#
#       Now, normalized mutual info is much lower than in EXP1, but it's probably because in EXP1 the
#       base for log was 80, now in EXP3 it is 320, and we know that higher base/more bars means
#       the entropy of normal distribution is closer to entropy of uniform distribution,
#       i.e. entropy is higher and so information is lower.
#
#       Fraction of possible information explained is now higher in comparison with EXP1. Why?
#       I hope it is cos' we now work in smaller parameter space and mutual info is more precise.
#       But is it true?
#
#       R^2 is now around 97%. Mutual information (fraction, normalized) is little bit
#       lower in EXP3 than in EXP2. Probably it is because that memory/forgetting variance
#       brings in EXP3 extra variability which is not included in EXP2
#       (after filtering out reinforce==FALSE there are no other sources of variability,
#       we use in EXP2 only same parameters which we use for model estimation)
#
#       The main result is that 'forget_sd' brings some variability to model behavior,
#       but nothing systematic. The model is still dominated only by:
#       Folding point, Memory/Forgetting and Communication rate.
#
#       So, model is dominated by system parameters:
#       -- Folding point is very probably given by issue
#       -- Communication rate is given by the system
#          (OK, it might be agregated, how much agents want to communicate)
#       -- Memory/Forgetting is given by human nature and used technology
#
#
#



# Experiment #4 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 42

# Loading results of the first seed
load("results4_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb4 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results4_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb4 = tb4 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb4 = tb4 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:folding_sd, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

tb4 %>%
  select(foldingPoint, folding_sd, SD:ESBG) %>%
  pivot_longer(cols = SD:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_grid(vars(#foldingPoint,
    folding_sd), vars(Measure), scales = "free", labeller = "label_both") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  theme_classic()
# ggsave("00-try.png", height = 49, width = 6)

# NOTE: Mhmmm... Still it doesn't seem the diversity makes difference...
#
tb4 %>%
  group_by(folding_sd, foldingPoint) %>%
  summarise(ESBG = mean(ESBG), SD = mean(SD), Manhattan = mean(manhattan)) %>%
  ungroup() %>%
  pivot_longer(Manhattan:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = foldingPoint, y = folding_sd, size = value * 30,
      label = round(value, 2), col = Measure) +
  facet_grid(vars(Measure)) +
  geom_point(alpha = 0.6) +
  geom_text(col = "black", size = 5) +
  scale_size_identity() +
  guides(col = "none") +
  theme_classic()
# We also see it here...



# regression --------------------------------------------------------------

za4 = lm(SD ~  forgeting * communicationRate,
         data = tb4)
xa4 = lm(SD ~ foldingPoint + forgeting + communicationRate,
         data = tb4)
ya4 = lm(SD ~ foldingPoint * forgeting * communicationRate,
         data = tb4)
zb4 = lm(manhattan ~ forgeting * communicationRate,
         data = tb4)
xb4 = lm(manhattan ~ foldingPoint + forgeting + communicationRate,
         data = tb4)
yb4 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
         data = tb4)
zc4 = lm(ESBG ~ forgeting * communicationRate,
         data = tb4)
xc4 = lm(ESBG ~ foldingPoint + forgeting + communicationRate,
         data = tb4)
yc4 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
         data = tb4)

## Final tables
stargazer(za4, xa4, ya4,
          omit.stat = c("f"), type = "text", omit = 23:1605,
          add.lines = "Coefficients for interactions were supressed!")

stargazer(zb4, xb4, yb4,
          omit.stat = c("f"), type = "text", omit = 23:1605,
          add.lines = "Coefficients for interactions were supressed!")

stargazer(zc4, xc4, yc4,
          omit.stat = c("f"), type = "text", omit = 23:1605,
          add.lines = "Coefficients for interactions were supressed!")
# Note: OK, it seems that omitting 'folding_sd' makes no difference,
#       so we might not play with this variable...
#



# Visualizations ----------------------------------------------------------

tx4 = tibble(tb4, pra = predict(ya4), prb = predict(yb4), prc = predict(yc4)) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)

ty4 = tb4 %>% #filter(forgeting == "0.2") %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric()) %>%
  group_by(communicationRate, forgeting, #folding_sd,
           foldingPoint) %>%
  summarise(SD_sd = sd(SD), manhattan_sd = sd(manhattan), ESBG_sd = sd(ESBG),
            SD = mean(SD), manhattan = mean(manhattan), ESBG = mean(ESBG)) %>%
  arrange(communicationRate, foldingPoint, #folding_sd,
          forgeting)

# 3D graphs
plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$ESBG, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$SD, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$manhattan, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty4$SD, y = ty4$manhattan, z = ty4$ESBG, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$SD_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$manhattan_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$SD_sd, y = ty4$manhattan_sd, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))



## Final plots
# All polarization measures:
png("31-allMeasures.png")
tb4 %>%  select(manhattan, SD, ESBG) %>%
  scatterplot3d(highlight.3d = T, main = "All polarization measures")
dev.off()

png("32-SD.png")
tx4 %>%  select(forgeting, communicationRate, pra) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: SD", zlab = "Prediction (R^2: 98.8%)")
dev.off()

png("33-Manhattan.png")
tx4 %>%  select(forgeting, communicationRate, prb) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: Manhattan", zlab = "Prediction (R^2: 99.9%)")
dev.off()

png("34-ESBG.png")
tx4 %>%  select(forgeting, communicationRate, prc) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: ESBG", zlab = "Prediction (R^2: 98.8%)")
dev.off()



# Information measures ----------------------------------------------------

# Discretization of dataset
td4 = tx4 %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = communicationRate * 10000 + forgeting * 100 + foldingPoint )

# Normalized mutual information
exp(mutinformation(td4[, 13], td4[, 5])) %>% log(base = 400) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td4[, 13], td4[, 4])) %>% log(base = 400) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td4[, 13], td4[, 6])) %>% log(base = 400) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(5, 4, 6)) {
  val = (100 * mutinformation(td4[, 13], td4[, v]) / mutinformation(td4[, v], td4[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td4)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb4, ya4, yc4, type = "text", omit = 1:805,
          add.lines = "All coefficients were supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.
#
#       The main result is that 'folding_sd' brings some variability to model behavior,
#       but nothing systematic. The model is still dominated only by:
#       Folding point, Memory/Forgetting and Communication rate.
#
#       So, model is dominated by system parameters:
#       -- Folding point is very probably given by issue or societal perception of it
#       -- Communication rate is given by the system
#          (OK, it might be agregated, how much agents want to communicate)
#       -- Memory/Forgetting is given by human nature and used technology
#
#
#



# Experiment #5 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 48

# Loading results of the first seed
load("results5_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb5 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results5_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb5 = tb5 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb5 = tb5 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:neis_sd, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

tb5 %>%
  select(communicationRate, neis_sd, SD:ESBG) %>%
  pivot_longer(cols = SD:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_grid(vars(#communicationRate,
    neis_sd), vars(Measure), scales = "free", labeller = "label_both") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  theme_classic()
# ggsave("00-try.png", height = 49, width = 6)

# NOTE: Mhmmm... Still it doesn't seem the diversity makes difference...
#
tb5 %>%
  group_by(neis_sd, communicationRate) %>%
  summarise(ESBG = mean(ESBG), SD = mean(SD), Manhattan = mean(manhattan)) %>%
  ungroup() %>%
  pivot_longer(Manhattan:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = communicationRate, y = neis_sd, size = value * 30,
      label = round(value, 2), col = Measure) +
  facet_grid(vars(Measure)) +
  geom_point(alpha = 0.6) +
  geom_text(col = "black", size = 5) +
  scale_size_identity() +
  guides(col = "none") +
  theme_classic()
# We also see it here...



# regression --------------------------------------------------------------

# za5 = lm(SD ~  forgeting * communicationRate,
#          data = tb4)
# xa5 = lm(SD ~ foldingPoint + forgeting + communicationRate,
#          data = tb4)
ya5 = lm(SD ~ foldingPoint * forgeting * communicationRate,
         data = tb5)
# zb4 = lm(manhattan ~ forgeting * communicationRate,
#          data = tb4)
# xb4 = lm(manhattan ~ foldingPoint + forgeting + communicationRate,
#          data = tb4)
yb5 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
         data = tb5)
# zc4 = lm(ESBG ~ forgeting * communicationRate,
#          data = tb4)
# xc4 = lm(ESBG ~ foldingPoint + forgeting + communicationRate,
#          data = tb4)
yc5 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
         data = tb5)

## Final tables
stargazer(#za4, xa4,
          ya5, yb5, yc5,
          omit.stat = c("f"), type = "text", omit = 13:1605,
          add.lines = "Coefficients for interactions were supressed!")

# stargazer(#zb4, xb4,
#           yb5,
#           omit.stat = c("f"), type = "text", omit = 23:1605,
#           add.lines = "Coefficients for interactions were supressed!")
#
# stargazer(#zc4, xc4,
#           yc5,
#           omit.stat = c("f"), type = "text", omit = 23:1605,
#           add.lines = "Coefficients for interactions were supressed!")
# Note: OK, it seems that omitting 'folding_sd' makes no difference,
#       so we might not play with this variable...
#



# Visualizations ----------------------------------------------------------

tx5 = tibble(tb5, pra = predict(ya5), prb = predict(yb5), prc = predict(yc5)) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)

ty5 = tb5 %>% #filter(forgeting == "0.2") %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric()) %>%
  group_by(communicationRate, forgeting, neis_sd,
           foldingPoint) %>%
  summarise(SD_sd = sd(SD), manhattan_sd = sd(manhattan), ESBG_sd = sd(ESBG),
            SD = mean(SD), manhattan = mean(manhattan), ESBG = mean(ESBG)) %>%
  arrange(communicationRate, foldingPoint, neis_sd,
          forgeting)

# 3D graphs
plot3d(x = ty5$communicationRate, y = ty5$foldingPoint, z = ty5$ESBG, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty5$communicationRate, y = ty5$foldingPoint, z = ty5$SD, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty5$communicationRate, y = ty5$foldingPoint, z = ty5$manhattan, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty5$SD, y = ty5$manhattan, z = ty5$ESBG, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$SD_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$manhattan_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$SD_sd, y = ty4$manhattan_sd, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))



## Final plots
# All polarization measures:
png("41-allMeasures.png")
tb5 %>%  select(manhattan, SD, ESBG) %>%
  scatterplot3d(highlight.3d = T, main = "All polarization measures")
dev.off()

png("42-SD.png")
tx5 %>%  select(forgeting, communicationRate, pra) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: SD", zlab = "Prediction (R^2: 98.8%)")
dev.off()

png("43-Manhattan.png")
tx5 %>%  select(forgeting, communicationRate, prb) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: Manhattan", zlab = "Prediction (R^2: 99.9%)")
dev.off()

png("44-ESBG.png")
tx5 %>%  select(forgeting, communicationRate, prc) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: ESBG", zlab = "Prediction (R^2: 98.8%)")
dev.off()



# Information measures ----------------------------------------------------

# Discretization of dataset
td5 = tx5 %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = communicationRate * 10000 + forgeting * 100 + foldingPoint )

# Normalized mutual information
exp(mutinformation(td5[, 13], td5[, 5])) %>% log(base = 100) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td5[, 13], td5[, 4])) %>% log(base = 100) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td5[, 13], td5[, 6])) %>% log(base = 100) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(5, 4, 6)) {
  val = (100 * mutinformation(td5[, 13], td5[, v]) / mutinformation(td5[, v], td5[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td5)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb5, ya5, yc5, type = "text", omit = 1:805,
          add.lines = "All coefficients were supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.
#
#       The main result is that 'neis_sd' brings some variability to model behavior,
#       but nothing systematic. The model is still dominated only by:
#       Folding point, Memory/Forgetting and Communication rate.
#
#       So, model is dominated by system parameters:
#       -- Folding point is very probably given by issue
#       -- Communication rate is given by the system
#          (OK, it might be agregated, how much agents want to communicate)
#       -- Memory/Forgetting is given by human nature and used technology
#
#       So, where to go now?
#       a) structure/topology of communication network
#       b) initialization data -- details of "Black Pete scenario"
#       c) weight of own opinion in the reinforcement sub-model/sub-algorithm
#
#



# Experiment #6 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 48

# Loading results of the first seed
load("results6_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb6 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results6_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb6 = tb6 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb6 = tb6 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:neis, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

tb6 %>%
  select(communicationRate, neis, SD:ESBG) %>%
  pivot_longer(cols = SD:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_grid(vars(#communicationRate,
    neis), vars(Measure), scales = "free", labeller = "label_both") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  theme_classic()
# ggsave("00-try.png", height = 49, width = 6)

# NOTE: Mhmmm... It does seem the diversity makes some difference...
#
tb6 %>%
  group_by(neis, communicationRate) %>%
  summarise(ESBG = sum(ESBG > 0.3), SD = sum(SD > 0.6), Manhattan = sum(manhattan > 0.7)) %>%
  ungroup() %>%
  pivot_longer(Manhattan:ESBG, names_to = "Measure") %>%
  mutate(value = if_else(value == 0, NA_real_, value)) %>%
  ggplot() +
  aes(x = communicationRate, y = neis, size = value * 600 * (1 / (completedSeeds * 500)),
      label = round(value, 2), col = Measure) +
  facet_grid(vars(Measure)) +
  geom_point(alpha = 0.6) +
  geom_text(col = "black", size = 5) +
  scale_size_identity() +
  labs(title = "How frequently model reached over critical polarization",
       subtitle = "ESBG > 0.3, SD > 0.6, Manhattan > 0.7; counts equal to 0 are suppressed") +
  guides(col = "none") +
  theme_classic()
tb6 %>%
  group_by(neis, communicationRate) %>%
  summarise(ESBG = sum(ESBG < 0.175), SD = sum(SD < 0.3), Manhattan = sum(manhattan < 0.25)) %>%
  ungroup() %>%
  pivot_longer(Manhattan:ESBG, names_to = "Measure") %>%
  mutate(value = if_else(value == 0, NA_real_, value)) %>%
  ggplot() +
  aes(x = communicationRate, y = neis, size = value * 400 * (1 / (completedSeeds * 500)),
      label = round(value, 2), col = Measure) +
  facet_grid(vars(Measure)) +
  geom_point(alpha = 0.6) +
  geom_text(col = "black", size = 5) +
  scale_size_identity() +
  labs(title = "How frequently model reached over low polarization",
       subtitle = "ESBG < 0.175, SD < 0.3, Manhattan < 0.25; counts equal to 0 are suppressed") +
  guides(col = "none") +
  theme_classic()
# But it seems the average doesn't reflect it...
# Even going over/under limits doesn't reflect it well...
# But from all experiments the 'neis' value has highest effect spotted!


# regression --------------------------------------------------------------

# za5 = lm(SD ~  forgeting * communicationRate,
#          data = tb4)
# xa5 = lm(SD ~ foldingPoint + forgeting + communicationRate,
#          data = tb4)
ya6 = lm(SD ~ foldingPoint * forgeting * communicationRate,
         data = tb6)
# zb4 = lm(manhattan ~ forgeting * communicationRate,
#          data = tb4)
# xb4 = lm(manhattan ~ foldingPoint + forgeting + communicationRate,
#          data = tb4)
yb6 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
         data = tb6)
# zc4 = lm(ESBG ~ forgeting * communicationRate,
#          data = tb4)
# xc4 = lm(ESBG ~ foldingPoint + forgeting + communicationRate,
#          data = tb4)
yc6 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
         data = tb6)

## Final tables
stargazer(#za4, xa4,
  ya6, yb6, yc6,
  omit.stat = c("f"), type = "text", omit = 13:1605,
  add.lines = "Coefficients for interactions were supressed!")

# stargazer(#zb4, xb4,
#           yb5,
#           omit.stat = c("f"), type = "text", omit = 23:1605,
#           add.lines = "Coefficients for interactions were supressed!")
#
# stargazer(#zc4, xc4,
#           yc5,
#           omit.stat = c("f"), type = "text", omit = 23:1605,
#           add.lines = "Coefficients for interactions were supressed!")
# Note: OK, it seems that omitting 'folding_sd' makes no difference,
#       so we might not play with this variable...
#



# Visualizations ----------------------------------------------------------

tx6 = tibble(tb6, pra = predict(ya6), prb = predict(yb6), prc = predict(yc6)) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)

ty6 = tb6 %>% #filter(forgeting == "0.2") %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric()) %>%
  group_by(communicationRate, forgeting, neis,
           foldingPoint) %>%
  summarise(SD_sd = sd(SD), manhattan_sd = sd(manhattan), ESBG_sd = sd(ESBG),
            SD = mean(SD), manhattan = mean(manhattan), ESBG = mean(ESBG)) %>%
  arrange(communicationRate, foldingPoint, neis,
          forgeting)

# 3D graphs
plot3d(x = ty6$communicationRate, y = ty6$foldingPoint, z = ty6$ESBG, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty6$communicationRate, y = ty6$foldingPoint, z = ty6$SD, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty6$communicationRate, y = ty6$foldingPoint, z = ty6$manhattan, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty6$SD, y = ty6$manhattan, z = ty6$ESBG, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$SD_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$manhattan_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$SD_sd, y = ty4$manhattan_sd, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))



## Final plots
# All polarization measures:
png("51-allMeasures.png")
tb6 %>%  select(manhattan, SD, ESBG) %>%
  scatterplot3d(highlight.3d = T, main = "All polarization measures")
dev.off()

png("52-SD.png")
tx6 %>%  select(forgeting, communicationRate, pra) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: SD", zlab = "Prediction (R^2: 98.8%)")
dev.off()

png("53-Manhattan.png")
tx6 %>%  select(forgeting, communicationRate, prb) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: Manhattan", zlab = "Prediction (R^2: 99.9%)")
dev.off()

png("54-ESBG.png")
tx6 %>%  select(forgeting, communicationRate, prc) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: ESBG", zlab = "Prediction (R^2: 98.8%)")
dev.off()



# Information measures ----------------------------------------------------

# Discretization of dataset
td6 = tx6 %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = communicationRate * 10000 + forgeting * 100 + foldingPoint )

# Normalized mutual information
exp(mutinformation(td6[, 13], td6[, 5])) %>% log(base = 100) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td6[, 13], td6[, 4])) %>% log(base = 100) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td6[, 13], td6[, 6])) %>% log(base = 100) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(5, 4, 6)) {
  val = (100 * mutinformation(td6[, 13], td6[, v]) / mutinformation(td6[, v], td6[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td6)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb6, ya6, yc6, type = "text", omit = 1:805,
          add.lines = "All coefficients were supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.
#
#       The main result is that 'neis' brings more variability to model behavior,
#       but despite it is systematic (it brings variability for certain regions and
#       certain not) the effect of 'neis' is secondary, the model is still dominated only by:
#       Folding point, Memory/Forgetting and Communication rate.
#
#       So, model is dominated by system parameters:
#       -- Folding point is very probably given by issue
#       -- Communication rate is given by the system
#          (OK, it might be agregated, how much agents want to communicate)
#       -- Memory/Forgetting is given by human nature and used technology
#       -- and average number of communication partners is of secondary effect
#
#       So, where to go now?
#       Definitely the structure/topology of communication network!
#       Secondary effect of average comm. partners shows that network structure matters!
#
#



# Experiment #7 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 50

# Loading results of the first seed
load("results7_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb7 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results7_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb7 = tb7 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb7 = tb7 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:sdFractionWeight, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

tb7 %>%
  select(meanWeight, sdFractionWeight, SD:ESBG) %>%
  pivot_longer(cols = SD:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = value) +
  facet_grid(vars(meanWeight, sdFractionWeight), vars(Measure), scales = "free", labeller = "label_both") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  theme_classic()
# ggsave("00-try.png", height = 49, width = 6)

# NOTE: Mhmmm... It does seem the diversity makes some difference...
#
tb7 %>%
  group_by(meanWeight, sdFractionWeight) %>%
  summarise(ESBG = mean(ESBG), SD = mean(SD), Manhattan = mean(manhattan)) %>%
  ungroup() %>%
  pivot_longer(Manhattan:ESBG, names_to = "Measure") %>%
  ggplot() +
  aes(x = meanWeight, y = sdFractionWeight, size = value * 60,
      label = round(value, 2), col = Measure) +
  facet_grid(vars(Measure)) +
  geom_point(alpha = 0.6) +
  geom_text(col = "black", size = 5) +
  scale_size_identity() +
  labs(title = "Mean of polarization measures") +
  guides(col = "none") +
  theme_classic()
# tb6 %>%
#   group_by(neis, communicationRate) %>%
#   summarise(ESBG = sum(ESBG < 0.175), SD = sum(SD < 0.3), Manhattan = sum(manhattan < 0.25)) %>%
#   ungroup() %>%
#   pivot_longer(Manhattan:ESBG, names_to = "Measure") %>%
#   mutate(value = if_else(value == 0, NA_real_, value)) %>%
#   ggplot() +
#   aes(x = communicationRate, y = neis, size = value * 400 * (1 / (completedSeeds * 500)),
#       label = round(value, 2), col = Measure) +
#   facet_grid(vars(Measure)) +
#   geom_point(alpha = 0.6) +
#   geom_text(col = "black", size = 5) +
#   scale_size_identity() +
#   labs(title = "How frequently model reached over low polarization",
#        subtitle = "ESBG < 0.175, SD < 0.3, Manhattan < 0.25; counts equal to 0 are suppressed") +
#   guides(col = "none") +
#   theme_classic()
# But it seems the average doesn't reflect it...
# Even going over/under limits doesn't reflect it well...
# But from all experiments the 'neis' value has highest effect spotted!


# regression --------------------------------------------------------------

ya7 = lm(SD ~ foldingPoint * forgeting * communicationRate,
         data = tb7)
yb7 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
         data = tb7)
yc7 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
         data = tb7)
# wa7 = lm(SD ~ foldingPoint * forgeting * communicationRate * meanWeight, data = tb7)
# wb7 = lm(manhattan ~ foldingPoint * forgeting * communicationRate * meanWeight, data = tb7)
# wc7 = lm(ESBG ~ foldingPoint * forgeting * communicationRate * meanWeight, data = tb7)
# va7 = lm(SD ~ foldingPoint * forgeting * communicationRate * sdFractionWeight, data = tb7)
# vb7 = lm(manhattan ~ foldingPoint * forgeting * communicationRate * sdFractionWeight, data = tb7)
# vc7 = lm(ESBG ~ foldingPoint * forgeting * communicationRate * sdFractionWeight, data = tb7)
# ua7 = lm(SD ~ foldingPoint * forgeting * communicationRate * meanWeight * sdFractionWeight, data = tb7)
# ub7 = lm(manhattan ~ foldingPoint * forgeting * communicationRate * meanWeight * sdFractionWeight, data = tb7)
# uc7 = lm(ESBG ~ foldingPoint * forgeting * communicationRate * meanWeight * sdFractionWeight, data = tb7)

## Final tables
stargazer(ya7, yb7, yc7,
  omit.stat = c("f"), type = "text", omit = 13:1605,
  add.lines = "Coefficients for interactions were supressed!")
# stargazer(wa7, wb7, wc7,
#           omit.stat = c("f"), type = "text", omit = 15:1605,
#           add.lines = "Coefficients for interactions were supressed!")
# stargazer(va7, vb7, vc7,
#           omit.stat = c("f"), type = "text", omit = 15:1605,
#           add.lines = "Coefficients for interactions were supressed!")
# stargazer(ua7, ub7, uc7,
#           omit.stat = c("f"), type = "text", omit = 17:1605,
#           add.lines = "Coefficients for interactions were supressed!")
# OK! So, this weight doesn't explain polarization independently,
# despite the original models can't explain cca 15% in case of SD and ESBG.
# And the main contributor is meanWeight, not SD of it.


# Visualizations ----------------------------------------------------------

tx7 = tibble(tb7, pra = predict(ya7), prb = predict(yb7), prc = predict(yc7)) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)

ty7 = tb7 %>% #filter(forgeting == "0.2") %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric()) %>%
  group_by(communicationRate, forgeting, meanWeight, sdFractionWeight, foldingPoint) %>%
  summarise(SD_sd = sd(SD), manhattan_sd = sd(manhattan), ESBG_sd = sd(ESBG),
            SD = mean(SD), manhattan = mean(manhattan), ESBG = mean(ESBG)) %>%
  arrange(communicationRate, foldingPoint, sdFractionWeight, meanWeight, forgeting)

# 3D graphs
plot3d(x = ty7$communicationRate, y = ty7$foldingPoint, z = ty7$ESBG, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty7$communicationRate, y = ty7$foldingPoint, z = ty7$SD, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty7$communicationRate, y = ty7$foldingPoint, z = ty7$manhattan, type = "s", size = .5, col = rainbow(4))
plot3d(x = ty7$SD, y = ty7$manhattan, z = ty7$ESBG, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$SD_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$manhattan_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty7$SD_sd, y = ty7$manhattan_sd, z = ty7$ESBG_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = tx7$diff_a, y = tx7$diff_b, z = tx7$diff_c, type = "s", size = .5, col = rainbow(4))



## Final plots
# All polarization measures:
png("61-allMeasures.png")
tb7 %>%  select(manhattan, SD, ESBG) %>%
  scatterplot3d(highlight.3d = T, main = "All polarization measures")
dev.off()

png("62-SD.png")
tx7 %>%  select(forgeting, communicationRate, pra) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: SD", zlab = "Prediction (R^2: 98.8%)")
dev.off()

png("63-Manhattan.png")
tx7 %>%  select(forgeting, communicationRate, prb) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: Manhattan", zlab = "Prediction (R^2: 99.9%)")
dev.off()

png("64-ESBG.png")
tx7 %>%  select(forgeting, communicationRate, prc) %>%
  scatterplot3d(highlight.3d = T, main = "Polarization measure: ESBG", zlab = "Prediction (R^2: 98.8%)")
dev.off()



# Information measures ----------------------------------------------------

# Discretization of dataset
td7 = tx7 %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = communicationRate * 10000 + forgeting * 100 + foldingPoint )

# Normalized mutual information
exp(mutinformation(td7[, 13], td7[, 5])) %>% log(base = 100) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td7[, 13], td7[, 4])) %>% log(base = 100) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td7[, 13], td7[, 6])) %>% log(base = 100) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(5, 4, 6)) {
  val = (100 * mutinformation(td7[, 13], td7[, v]) / mutinformation(td7[, v], td7[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td7)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb7, ya7, yc7, type = "text", omit = 1:805,
          add.lines = "All coefficients were supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.
#
#       The main result is that 'weight of reinforced opinion'
#       brings more variability to model behavior,
#       but despite it is systematic (it brings variability for certain regions and
#       certain not) the effect is still secondary, the model is still dominated only by:
#       Folding point, Memory/Forgetting and Communication rate.
#
#       So, model is dominated by system parameters:
#       -- Folding point is very probably given by issue
#       -- Communication rate is given by the system
#          (OK, it might be aggregated, how much agents want to communicate)
#       -- Memory/Forgetting is given by human nature and used technology
#       -- and average number of communication partners is of secondary effect
#       -- also secondary, but much bigger effect has the weight of reinforced opinion, mainly the average of it
#
#       So, where to go now?
#       a) structure/topology of communication network
#       b) initialization data -- details of "Black Pete scenario"
#
#



# Experiment #8 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 50

# Loading results of the first seed
load("results8_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb8 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results8_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb8 = tb8 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb8 = tb8 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:sdFractionWeight, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

# Computing mean values of opinion, information and attention
df = tb8 %>% select(1:2, 4:7, SD:ESBG, starts_with(paste0("b_f", c("o", "a", "pi", "ni", "si", "i") , "_" ))) %>%
  pivot_longer(cols = 10:75, names_prefix = "b_f", names_sep = "_",
               names_to = c("Measure", "weight"), names_transform = list(weight = as.integer)) %>%
  mutate(weight = case_when(
           Measure == "o" ~ -1.2 + weight * 0.2,
           Measure == "a" ~ -0.1 + weight * 0.1,
           Measure == "pi" ~ -0.1 + weight * 0.1,
           Measure == "ni" ~ -0.1 + weight * 0.1,
           Measure == "si" ~ -0.2 + weight * 0.2,
           Measure == "i" ~ -1.2 + weight * 0.2),
         value = value * round(weight, 2) * 0.001,
         Measure = case_match(Measure, "a" ~ "Attention", "i" ~ "Information", "pi" ~ "Positive",
                                       "ni" ~ "Negative", "si" ~ "Sum","o" ~ "Opinion")) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting, SD, manhattan, ESBG, Measure, opDistribution) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  pivot_wider(id_cols = 1:9, names_from = "Measure") %>%
  # mutate(diff = Sum - Positive - Negative) %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, opDistribution)#%>%
  # filter(communicationRate == 0.65, forgeting == 0.45)

# Graph 3D
plot3d(x = df$Information, y = df$Attention, z = df$ESBG, type = "s", size = .5, col = rainbow(2))
plot3d(x = df$Sum, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(2))
plot3d(x = df$SD, y = df$manhattan, z = df$ESBG, type = "s", size = .5, col = rainbow(2))
plot3d(x = df$manhattan, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(2))
plot3d(x = df$SD, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(2))
plot3d(x = df$ESBG, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(2))
plot3d(x = df$Positive, y = df$Negative, z = df$ESBG, type = "s", size = .5, col = rainbow(2))
plot3d(x = df$Positive, y = df$Negative, z = df$Opinion, type = "s", size = .5, col = rainbow(2))

# Just jitter points
df %>%
  ggplot(aes(x = Positive, y = Negative, col = opDistribution)) +
  geom_jitter(alpha = 0.1, size = 1.5) +
  theme_classic()

df %>%
  ggplot(aes(x = Positive, y = Negative, col = opDistribution)) +
  facet_grid(rows = vars(meanWeight, communicationRate), cols = vars(forgeting)) +
  geom_jitter(alpha = 0.3, size = 1.5) +
  theme_classic()

df %>%
  ggplot(aes(x = Sum, y = Attention, col = opDistribution)) +
  facet_grid(rows = vars(meanWeight, communicationRate), cols = vars(forgeting)) +
  geom_jitter(alpha = 0.3, size = 1.5) +
  theme_classic()


# Computing agents with opinion 0.5+
dp = tb8 %>% select(1:2, 4:7, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 10:20, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight >= 9, #value > 0
         #, opDistribution == "Black Pete"
         ) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting, opDistribution, SD, manhattan, ESBG) %>%
  summarise(extreme_positive_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, opDistribution)

# Computing agents with opinion less than -0.5
dn = tb8 %>% select(1:2, 4:7, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 10:20, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight <= 3, #value > 0
         #, opDistribution == "Black Pete"
  ) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting, opDistribution, SD, manhattan, ESBG) %>%
  summarise(extreme_negative_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, opDistribution)

# Joining both extremes:
do = dn %>% right_join(dp)

# Graphs
do %>%
  select(Distribution = 6, Negative = 10, Positive = 11) %>%
  filter(Negative > 0 | Positive > 0) %>%
  pivot_longer(cols = 2:3, names_to = "Polarity", values_to = "Count_extremes") %>%
  ggplot() +
  aes(x = Count_extremes, fill = Distribution) +
  facet_grid(rows = vars(Distribution), cols = vars(Polarity), scales = "free_y") +
  geom_histogram(alpha = 0.4, show.legend = F, binwidth = 50) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:6) * 200) +
  labs(title = "Comparison of extreme opinions distribution according scenario",
       subtitle = "Scenrios apparently differ", y = "Count of simulations",
       x = "How many extreme agents we spotted in the simulation?") +
  theme_light()
ggsave("distributions.png", width = 7, height = 6)


do %>%
  select(Distribution = 6, Negative = 10, Positive = 11) %>%
  ggplot() +
  aes(x = Positive, y = Negative, col = Distribution) +
  geom_point(alpha = 0.1) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:5) * 200) +
  theme_light()



# Experiment #9 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 38

# Loading results of the first seed
load("results9_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb9 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results9_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb9 = tb9 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb9 = tb9 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:majInOpSD, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

# Computing mean values of opinion, information and attention
df = tb9 %>% select(1:2, 4:7, 9:10, SD:ESBG,
                    starts_with(paste0("b_f", c("o", "a", "pi", "ni", "si", "i") , "_" ))) %>%
  pivot_longer(cols = 12:77, names_prefix = "b_f", names_sep = "_",
               names_to = c("Measure", "weight"), names_transform = list(weight = as.integer)) %>%
  mutate(weight = case_when(
    Measure == "o" ~ -1.2 + weight * 0.2,
    Measure == "a" ~ -0.1 + weight * 0.1,
    Measure == "pi" ~ -0.1 + weight * 0.1,
    Measure == "ni" ~ -0.1 + weight * 0.1,
    Measure == "si" ~ -0.2 + weight * 0.2,
    Measure == "i" ~ -1.2 + weight * 0.2),
    value = value * round(weight, 2) * 0.001,
    Measure = case_match(Measure, "a" ~ "Attention", "i" ~ "Information", "pi" ~ "Positive",
                         "ni" ~ "Negative", "si" ~ "Sum","o" ~ "Opinion")) %>%
  group_by(seed, opDistribution, meanWeight, foldingPoint, communicationRate,
           forgeting, majInOpAv, majInOpSD,
           SD, manhattan, ESBG, Measure) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  pivot_wider(id_cols = 1:11, names_from = "Measure") %>%
  # mutate(diff = Sum - Positive - Negative) %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, majInOpAv, majInOpSD, majInOpAv)#%>%
# filter(communicationRate == 0.65, forgeting == 0.45)

# Graph 3D
# plot3d(x = df$Information, y = df$Attention, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$Sum, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$SD, y = df$manhattan, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$manhattan, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$SD, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$ESBG, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$Positive, y = df$Negative, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
plot3d(x = df$Positive, y = df$Negative, z = df$Opinion, type = "s", size = .5, col = rainbow(3))

# Just jitter points
df %>%
  ggplot(aes(x = Positive, y = Negative, col = majInOpAv)) +
  geom_jitter(alpha = 0.1, size = 1.5) +
  theme_classic()

df %>%
  ggplot(aes(x = Positive, y = Negative, col = majInOpAv)) +
  facet_grid(rows = vars(meanWeight, communicationRate), cols = vars(forgeting)) +
  geom_jitter(alpha = 0.3, size = 1.5) +
  theme_classic()

df %>%
  ggplot(aes(x = Sum, y = Attention, col = majInOpAv)) +
  facet_grid(rows = vars(meanWeight, communicationRate), cols = vars(forgeting)) +
  geom_jitter(alpha = 0.3, size = 1.5) +
  theme_classic()


# Computing agents with opinion 0.5+
dp = tb9 %>% select(1:2, 4:7, 9:10, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 12:22, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight >= 9, #value > 0
         #, opDistribution == "Black Pete"
  ) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting, opDistribution,
           majInOpAv, majInOpSD, SD, manhattan, ESBG) %>%
  summarise(extreme_positive_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, majInOpAv, majInOpSD, opDistribution)

# Computing agents with opinion less than -0.5
dn = tb9 %>% select(1:2, 4:7, 9:10, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 12:22, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight <= 3, #value > 0
         #, opDistribution == "Black Pete"
  ) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting,
           majInOpAv, majInOpSD, opDistribution, SD, manhattan, ESBG) %>%
  summarise(extreme_negative_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, opDistribution, majInOpAv, majInOpSD)

# Joining both extremes:
do = dn %>% right_join(dp)

# Graphs
do %>%
  select(initOpinion = 6, OpinionSD = 7, Negative = 12, Positive = 13) %>%
  filter(Negative > 0 | Positive > 0) %>%
  pivot_longer(cols = 4:3, names_to = "Polarity", values_to = "Count_extremes") %>%
  ggplot() +
  aes(x = Count_extremes, fill = initOpinion) +
  facet_grid(rows = vars(OpinionSD), cols = vars(initOpinion, Polarity), scales = "free_x") +
  geom_histogram(alpha = 0.4, show.legend = F, binwidth = 50) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:6) * 400) +
  labs(title = "Comparison of extreme opinions distribution according 'SD initOp'",
       subtitle = "Scenrios apparently differ", y = "Count of simulations",
       x = "How many extreme agents we spotted in the simulation?") +
  theme_light()

do %>%
  select(initOpinion = 6, Negative = 12, Positive = 13) %>%
  filter(Negative > 0 | Positive > 0) %>%
  pivot_longer(cols = 2:3, names_to = "Polarity", values_to = "Count_extremes") %>%
  ggplot() +
  aes(x = Count_extremes, fill = initOpinion) +
  facet_grid(rows = vars(initOpinion), cols = vars(Polarity), scales = "free_y") +
  geom_histogram(alpha = 0.4, show.legend = F, binwidth = 50) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:12) * 600) +
  labs(title = "Comparison of extreme opinions distribution according 'average initOp'",
       subtitle = "Scenrios apparently differ", y = "Count of simulations",
       x = "How many extreme agents we spotted in the simulation?") +
  theme_light()

# Checking distribution of final opinion
df %>%
  ggplot() +
  aes(x = Opinion, fill = majInOpAv) +
  facet_grid(rows = vars(majInOpSD), cols = vars(majInOpAv), scales = "free_y") +
  geom_histogram(alpha = 0.4, show.legend = F, binwidth = 0.1) +
  scale_x_continuous(breaks = (-5:1) * 0.200) +
  labs(title = "Distribution of final average opinion") +
  theme_light()
ggsave("distributions_exp9b.png", width = 7, height = 6)


do %>%
  select(initOpinion = 6, OpinionSD = 7, Negative = 12, Positive = 13) %>%
  ggplot() +
  aes(x = Positive, y = Negative, col = initOpinion) +
  facet_grid(rows = vars(initOpinion), cols = vars(OpinionSD), labeller = "label_both") +
  geom_abline(slope = 1, col = "skyblue") +
  geom_abline(slope = -1, intercept = 1000, col = "grey50") +
  geom_abline(slope = (1 / 1.25), intercept = 00, col = "orange") +
  geom_abline(slope = (1.25), intercept = 00, col = "orange") +
  geom_point(alpha = 0.1, show.legend = F) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:5) * 200) +
  labs(title = "Distribution of counts of extreme negative and positive opinions") +
  theme_light()
ggsave("distributions_exp9c.png", width = 7, height = 6)



# regression --------------------------------------------------------------

ya9 = lm(SD ~ foldingPoint * forgeting * communicationRate,
         data = tb9)
yb9 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
         data = tb9)
yc9 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
         data = tb9)
# za9 = lm(SD ~ foldingPoint * forgeting * communicationRate * meanWeight,
#          data = tb9)
# zb9 = lm(manhattan ~ foldingPoint * forgeting * communicationRate * meanWeight,
#          data = tb9)
# zc9 = lm(ESBG ~ foldingPoint * forgeting * communicationRate  * meanWeight,
#          data = tb9)

## Final tables
stargazer(ya9,# za9,
          yb9,# zb9,
          yc9,# zc9,
          omit.stat = c("f", "ser"), type = "text", omit = 11:16005,
          add.lines = "Coefficients for interactions were supressed!")

tx9 = tibble(tb9, pra = predict(ya9), prb = predict(yb9), prc = predict(yc9)) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         diff_a = SD - pra,
         diff_b = manhattan - prb,
         diff_c = ESBG - prc)


# Information measures ----------------------------------------------------

# Discretization of dataset
td9 = tx9 %>%
  select(foldingPoint, forgeting, communicationRate, SD, manhattan, ESBG,
         pra, prb, prc, diff_a, diff_b, diff_c) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = communicationRate * 10000 + forgeting * 100 + foldingPoint )

# Normalized mutual information
exp(mutinformation(td9[, 13], td9[, 5])) %>% log(base = 64) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td9[, 13], td9[, 4])) %>% log(base = 64) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td9[, 13], td9[, 6])) %>% log(base = 64) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(5, 4, 6)) {
  val = (100 * mutinformation(td9[, 13], td9[, v]) / mutinformation(td9[, v], td9[, v])) %>% round(2)
  paste0("Combination CrFFp explains: ", val,
         "% of posible information of variable '", names(td9)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb9, ya9, yc9, type = "text", omit = 1:805,
          add.lines = "All coefficients were supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.



# Experiment #10 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 232

# Loading results of the first seed
load("results10_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb10 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results10_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb10 = tb10 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb10 = tb10 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:acceptanceSD, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

# Computing mean values of opinion, information and attention
df = tb10 %>% select(1:2, 4:7, 9:12, SD:ESBG,
                    starts_with(paste0("b_f", c("o", "a", "pi", "ni", "si", "i") , "_" ))) %>%
  pivot_longer(cols = 14:79, names_prefix = "b_f", names_sep = "_",
               names_to = c("Measure", "weight"), names_transform = list(weight = as.integer)) %>%
  mutate(weight = case_when(
    Measure == "o" ~ -1.2 + weight * 0.2,
    Measure == "a" ~ -0.1 + weight * 0.1,
    Measure == "pi" ~ -0.1 + weight * 0.1,
    Measure == "ni" ~ -0.1 + weight * 0.1,
    Measure == "si" ~ -0.2 + weight * 0.2,
    Measure == "i" ~ -1.2 + weight * 0.2),
    value = value * round(weight, 2) * 0.001,
    Measure = case_match(Measure, "a" ~ "Attention", "i" ~ "Information", "pi" ~ "Positive",
                         "ni" ~ "Negative", "si" ~ "Sum","o" ~ "Opinion")) %>%
  group_by(seed, opDistribution, meanWeight, foldingPoint, communicationRate,
           forgeting, acceptanceAv, acceptanceSD,
           SD, manhattan, ESBG, Measure) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  pivot_wider(id_cols = 1:11, names_from = "Measure") %>%
  # mutate(diff = Sum - Positive - Negative) %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, acceptanceSD, acceptanceAv)#%>%
# filter(communicationRate == 0.65, forgeting == 0.45)

# Graph 3D
# plot3d(x = df$Information, y = df$Attention, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$Sum, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$SD, y = df$manhattan, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$manhattan, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$SD, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$ESBG, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$Positive, y = df$Negative, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
plot3d(x = df$Positive, y = df$Negative, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$communicationRate, x = df$foldingPoint, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$forgeting, x = df$foldingPoint, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$communicationRate, x = df$forgeting, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$communicationRate, x = df$meanWeight, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$meanWeight, x = df$foldingPoint, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$forgeting, x = df$meanWeight, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$meanWeight, x = df$foldingPoint, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$communicationRate, x = df$meanWeight, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$meanWeight, x = df$forgeting, type = "s", size = .5, col = rainbow(3))

# Just jitter points
# df %>%
#   ggplot(aes(x = Positive, y = Negative, col = acceptanceAv)) +
#   geom_jitter(alpha = 0.1, size = 1.5) +
#   geom_abline(slope = 1, col = "skyblue") +
#   geom_abline(slope = -1, intercept = 1, col = "grey50") +
#   geom_abline(slope = (1 / 1.25), intercept = 00, col = "orange") +
#   geom_abline(slope = (1.25), intercept = 00, col = "orange") +
#   theme_classic()
#
# df %>%
#   ggplot(aes(x = Positive, y = Negative, col = acceptanceAv)) +
#   facet_grid(rows = vars(meanWeight, communicationRate), cols = vars(forgeting), labeller = "label_both") +
#   geom_jitter(alpha = 0.3, size = 1.5) +
#   theme_light()
#
# df %>%
#   ggplot(aes(x = Sum, y = Attention, col = acceptanceAv)) +
#   facet_grid(rows = vars(meanWeight, communicationRate), cols = vars(forgeting), labeller = "label_both") +
#   geom_jitter(alpha = 0.3, size = 1.5) +
#   theme_light()


# Computing agents with opinion 0.5+
dp = tb10 %>% select(1:2, 4:7, 9:12, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 14:24, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight >= 9, #value > 0
         #, opDistribution == "Black Pete"
  ) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting, opDistribution,
           acceptanceAv, acceptanceSD, SD, manhattan, ESBG) %>%
  summarise(extreme_positive_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, acceptanceAv, acceptanceSD, opDistribution)

# Computing agents with opinion less than -0.5
dn = tb10 %>% select(1:2, 4:7, 9:12, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 14:24, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight <= 3, #value > 0
         #, opDistribution == "Black Pete"
  ) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting,
           acceptanceAv, acceptanceSD, opDistribution, SD, manhattan, ESBG) %>%
  summarise(extreme_negative_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, opDistribution, acceptanceAv, acceptanceSD)

# Joining both extremes:
do = dn %>% right_join(dp)

# Graphs
# do %>%
#   select(acceptanceAv = 6, acceptanceSD = 7, Negative = 12, Positive = 13) %>%
#   filter(Negative > 0 | Positive > 0) %>%
#   pivot_longer(cols = 4:3, names_to = "Polarity", values_to = "Count_extremes") %>%
#   ggplot() +
#   aes(x = Count_extremes, fill = acceptanceAv) +
#   facet_grid(rows = vars(acceptanceSD), cols = vars(acceptanceAv, Polarity),
#              scales = "free_y", labeller = "label_both") +
#   geom_histogram(alpha = 0.4, show.legend = F, binwidth = 100) +
#   scale_x_continuous(breaks = (0:5) * 200) +
#   scale_y_continuous(breaks = (0:6) * 10) +
#   labs(title = "Comparison of extreme opinions distribution according 'latitude of acceptance avg/SD'",
#        subtitle = "Acceptance apparently brings some kind of symmetry", y = "Count of simulations",
#        x = "How many extreme agents we spotted in the simulation?") +
#   theme_light()

do %>%
  select(acceptanceAv = 6, Negative = 12, Positive = 13) %>%
  filter(Negative > 0 | Positive > 0) %>%
  pivot_longer(cols = 2:3, names_to = "Polarity", values_to = "Count_extremes") %>%
  ggplot() +
  aes(x = Count_extremes, fill = acceptanceAv) +
  facet_grid(rows = vars(acceptanceAv), cols = vars(Polarity),
             scales = "free_y", labeller = "label_both") +
  geom_histogram(alpha = 0.4, show.legend = F, binwidth = 50) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:15) * 400) +
  labs(title = "Comparison of extreme opinions distribution according 'latitude of acceptance avg/SD'",
       subtitle = "Acceptance apparently brings some kind of symmetry", y = "Count of simulations",
       x = "How many extreme agents we spotted in the simulation?") +
  theme_light()
ggsave("distributions_exp10.png", width = 7, height = 6)

# Checking distribution of final opinion
df %>%
  ggplot() +
  aes(x = Opinion, fill = acceptanceAv) +
  facet_grid(rows = vars(acceptanceSD), cols = vars(acceptanceAv),
             scales = "free_y", labeller = "label_both") +
  geom_histogram(alpha = 0.4, show.legend = F, binwidth = 0.1) +
  scale_x_continuous(breaks = (-5:2) * 0.200) +
  labs(title = "Distribution of final average opinion") +
  theme_light()
ggsave("distributions_exp10a.png", width = 7, height = 6)


do %>%
  select(acceptanceAv = 6, acceptanceSD = 7, Negative = 12, Positive = 13) %>%
  ggplot() +
  aes(x = Positive, y = Negative, col = acceptanceAv) +
  facet_grid(rows = vars(acceptanceAv), cols = vars(acceptanceSD), labeller = "label_both") +
  geom_abline(slope = 1, col = "skyblue") +
  geom_abline(slope = (1 / 1.25), intercept = 00, col = "orange") +
  geom_abline(slope = (1.25), intercept = 00, col = "orange") +
  geom_abline(slope = -1, intercept = 1000, col = "grey50") +
  geom_point(alpha = 0.1, show.legend = F) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:5) * 200) +
  labs(title = "Distribution of counts of extreme negative and positive opinions") +
  theme_light()
ggsave("distributions_exp10b.png", width = 7, height = 6)



# Measuring 'Black Pete' --------------------------------------------------

bp = do %>% rename(Negative = 12, Positive = 13) %>%
  mutate(blackPete = Positive >= 300 & (Positive > (Negative * 1.25)),
         revPete = Negative >= 300 & (Negative > (Positive * 1.25)),
         Pete = case_when(
           blackPete ~ "Black",
           revPete ~ "reversed",
           TRUE ~ "other"
         ) %>% factor())

sum(bp$blackPete) / nrow(bp)
sum(bp$revPete) / nrow(bp)

# How much is Black Pete Result polarized?
bp %>%
  ggplot() +
  aes(x = ESBG, fill = Pete) +
  geom_density(alpha = 0.6, binwidth = 0.03) +
  labs(title = "Distribution of ESBG of simulations\naccording fullfilling 'Black Pete result'",
       caption = "'Black Pete result': at least 30% of agents are extremely positive (opinion >=0.5) and\nthere are at least of 25% more extremely positive than extremely negative agents\n'Reversed Pete Result' is ther way round than 'Black Pete Result'.") +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave("blackPete_exp10_ESBG.png", width = 7, height = 6)

# Which parameters predict 'Black Pete Result'?
model = glm(blackPete~foldingPoint*communicationRate*forgeting*acceptanceAv, family="binomial", data=bp)
options(scipen=999)
summary(model)
pscl::pR2(model)["McFadden"]


# Graph represinting these parameters' influence:
sbp = bp %>% group_by(foldingPoint, communicationRate, forgeting, acceptanceAv) %>%
  mutate(N = n()) %>%
  group_by(foldingPoint, communicationRate, forgeting, acceptanceAv, N) %>%
  summarise(blackPete = sum(blackPete)) %>% ungroup() %>%
  mutate(p = round(100 * blackPete / N, 1),
         foldingPoint = fct_rev(foldingPoint))
sbp %>%
  ggplot() +
  aes(y = p, fill = acceptanceAv, x = forgeting) +
  facet_grid(rows = vars(foldingPoint), cols = vars(communicationRate), labeller = 'label_both') +
  geom_col(position = position_dodge()) +
  labs(y = "probability of 'Black Pete Result'") +
  theme_classic()
ggsave("blackPete_exp10_params.png", width = 7, height = 6)



# regression --------------------------------------------------------------

ya10 = lm(SD ~ foldingPoint * forgeting * communicationRate,
         data = tb10)
yb10 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
         data = tb10)
yc10 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
         data = tb10)
za9 = lm(SD ~ foldingPoint * forgeting * communicationRate * acceptanceAv * meanWeight,
         data = tb10)
zb9 = lm(manhattan ~ foldingPoint * forgeting * communicationRate * acceptanceAv * meanWeight,
         data = tb10)
zc9 = lm(ESBG ~ foldingPoint * forgeting * communicationRate  * acceptanceAv * meanWeight,
         data = tb10)

## Final tables
stargazer(ya10, za9,
          yb10, zb9,
          yc10, zc9,
          omit.stat = c("f", "ser"), type = "text", omit = 10:16005,
          add.lines = "Coefficients for interactions were supressed!")

# tx10 = tibble(tb10, pra = predict(ya10), prb = predict(yb10), prc = predict(yc10)) %>%
#   mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
#          foldingPoint = as.character(foldingPoint) %>% as.numeric(),
#          forgeting = as.character(forgeting) %>% as.numeric(),
#          meanWeight = as.character(meanWeight) %>% as.numeric(),
#          acceptanceAv = as.character(acceptanceAv) %>% as.numeric(),
#          diff_a = SD - pra,
#          diff_b = manhattan - prb,
#          diff_c = ESBG - prc)


# Information measures ----------------------------------------------------

# Discretization of dataset
td10 = tb10 %>%
  select(foldingPoint, forgeting, communicationRate, meanWeight, acceptanceAv, SD, manhattan, ESBG) %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         meanWeight = as.character(meanWeight) %>% as.numeric(),
         acceptanceAv = as.character(acceptanceAv) %>% as.numeric()) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = factor(communicationRate * 10000 + forgeting * 100 + foldingPoint),
         combinationAll = factor(100000000 * acceptanceAv + 1000000 * meanWeight + communicationRate * 10000 + forgeting * 100 + foldingPoint))

# Normalized mutual information
exp(mutinformation(td10[, 9], td10[, 7])) %>% log(base = 18) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td10[, 9], td10[, 6])) %>% log(base = 18) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td10[, 9], td10[, 8])) %>% log(base = 18) %>% round(3) %>% paste0("ESBG: ", .)
exp(mutinformation(td10[, 10], td10[, 7])) %>% log(base = 108) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td10[, 10], td10[, 6])) %>% log(base = 108) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td10[, 10], td10[, 8])) %>% log(base = 108) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(7, 6, 8)) {
  val = (100 * mutinformation(td10[, 9], td10[, v]) / mutinformation(td10[, v], td10[, v])) %>% round(2)
  val2 = (100 * mutinformation(td10[, 10], td10[, v]) / mutinformation(td10[, v], td10[, v])) %>% round(2)
  paste0("Combination CrFFp explains     : ", val,
         "% of posible information of variable '", names(td10)[v], "'.") %>% print()
  paste0("Combination LacMwCrFFp explains: ", val2,
         "% of posible information of variable '", names(td10)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb10, zb9, ya10, za9, yc10, zc9, type = "text", omit = 1:805,
          omit.stat = c("f", "ser"), add.lines = "Coeffs supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.





# Experiment #11 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 8

# Loading results of the first seed
load("results11_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb11 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results11_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb11 = tb11 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb11 = tb11 %>%
  mutate(
    ESBG = ESBG / 2,
    across(reinforce:acceptanceSD, ~ factor(.x)))



# Analysis ----------------------------------------------------------------

# Computing mean values of opinion, information and attention
df = tb11 %>% select(1, 4:6, 11, SD:ESBG,
                     starts_with(paste0("b_f", c("o", "a", "pi", "ni", "si", "i") , "_" ))) %>%
  pivot_longer(cols = 9:74, names_prefix = "b_f", names_sep = "_",
               names_to = c("Measure", "weight"), names_transform = list(weight = as.integer)) %>%
  mutate(weight = case_when(
    Measure == "o" ~ -1.2 + weight * 0.2,
    Measure == "a" ~ -0.1 + weight * 0.1,
    Measure == "pi" ~ -0.1 + weight * 0.1,
    Measure == "ni" ~ -0.1 + weight * 0.1,
    Measure == "si" ~ -0.2 + weight * 0.2,
    Measure == "i" ~ -1.2 + weight * 0.2),
    value = value * round(weight, 2) * 0.001,
    Measure = case_match(Measure, "a" ~ "Attention", "i" ~ "Information", "pi" ~ "Positive",
                         "ni" ~ "Negative", "si" ~ "Sum","o" ~ "Opinion")) %>%
  group_by(seed, foldingPoint, communicationRate, forgeting, acceptanceAv, SD, manhattan, ESBG, Measure) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  pivot_wider(id_cols = 1:8, names_from = "Measure") %>%
  # mutate(diff = Sum - Positive - Negative) %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, acceptanceAv)#%>%
# filter(communicationRate == 0.65, forgeting == 0.45)

# Graph 3D
# plot3d(x = df$Information, y = df$Attention, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$Sum, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$SD, y = df$manhattan, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$manhattan, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$SD, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$ESBG, y = df$Attention, z = df$Opinion, type = "s", size = .5, col = rainbow(3))
# plot3d(x = df$Positive, y = df$Negative, z = df$ESBG, type = "s", size = .5, col = rainbow(3))
plot3d(x = df$Positive, y = df$Negative, z = df$Opinion, type = "s", size = .5, col = rainbow(10))
# plot3d(z = df$Positive, y = df$communicationRate, x = df$foldingPoint, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$forgeting, x = df$foldingPoint, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$communicationRate, x = df$forgeting, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$communicationRate, x = df$meanWeight, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$meanWeight, x = df$foldingPoint, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$forgeting, x = df$meanWeight, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$meanWeight, x = df$foldingPoint, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$communicationRate, x = df$meanWeight, type = "s", size = .5, col = rainbow(3))
# plot3d(z = df$Positive, y = df$meanWeight, x = df$forgeting, type = "s", size = .5, col = rainbow(3))

# Just jitter points
df %>%
  ggplot(aes(x = Positive, y = Negative, col = acceptanceAv)) +
  geom_jitter(alpha = 0.1, size = 1.5) +
  geom_abline(slope = 1, col = "skyblue") +
  geom_abline(slope = -1, intercept = 1, col = "grey50") +
  geom_abline(slope = (1 / 1.25), intercept = 00, col = "orange") +
  geom_abline(slope = (1.25), intercept = 00, col = "orange") +
  theme_classic()

# df %>%
#   ggplot(aes(x = Positive, y = Negative, col = acceptanceAv)) +
#   facet_grid(rows = vars(meanWeight, communicationRate), cols = vars(forgeting), labeller = "label_both") +
#   geom_jitter(alpha = 0.3, size = 1.5) +
#   theme_light()
#
# df %>%
#   ggplot(aes(x = Sum, y = Attention, col = acceptanceAv)) +
#   facet_grid(rows = vars(meanWeight, communicationRate), cols = vars(forgeting), labeller = "label_both") +
#   geom_jitter(alpha = 0.3, size = 1.5) +
#   theme_light()


# Computing agents with opinion 0.5+
dp = tb11 %>% select(1:2, 4:7, 9:12, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 14:24, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight >= 9) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting, opDistribution,
           acceptanceAv, acceptanceSD, SD, manhattan, ESBG) %>%
  summarise(extreme_positive_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, acceptanceAv, acceptanceSD, opDistribution)

# Computing agents with opinion less than -0.5
dn = tb11 %>% select(1:2, 4:7, 9:12, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 14:24, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight <= 3) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting,
           acceptanceAv, acceptanceSD, opDistribution, SD, manhattan, ESBG) %>%
  summarise(extreme_negative_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, meanWeight, opDistribution, acceptanceAv, acceptanceSD)

# Joining both extremes:
do = dn %>% right_join(dp)

# Graphs
do %>%
  select(acceptanceAv = 6, Negative = 12, Positive = 13) %>%
  filter(Negative > 0 | Positive > 0) %>%
  pivot_longer(cols = 2:3, names_to = "Polarity", values_to = "Count_extremes") %>%
  ggplot() +
  aes(x = Count_extremes, fill = acceptanceAv) +
  facet_grid(rows = vars(acceptanceAv), cols = vars(Polarity),
             scales = "free_y", labeller = "label_value") +
  geom_histogram(alpha = 0.4, show.legend = F, binwidth = 50) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:15) * 400) +
  labs(title = "Comparison of extreme opinions distribution according 'latitude of acceptance avg/SD'",
       subtitle = "Acceptance apparently brings some kind of symmetry", y = "Count of simulations",
       x = "How many extreme agents we spotted in the simulation?") +
  theme_light()
ggsave("distributions_exp11.png", width = 7, height = 6)

# Checking distribution of final opinion
df %>%
  ggplot() +
  aes(x = Opinion, fill = acceptanceAv) +
  facet_grid(rows = vars(acceptanceAv),
             scales = "free_y", labeller = "label_value") +
  geom_histogram(alpha = 0.4, show.legend = F, binwidth = 0.1) +
  scale_x_continuous(breaks = (-5:2) * 0.200) +
  labs(title = "Distribution of final average opinion") +
  theme_light()
ggsave("distributions_exp11a.png", width = 7, height = 6)


do %>%
  select(acceptanceAv = 6, acceptanceSD = 7, Negative = 12, Positive = 13) %>%
  ggplot() +
  aes(x = Positive, y = Negative, col = acceptanceAv) +
  facet_wrap(vars(acceptanceAv), labeller = "label_value") +
  geom_abline(slope = 1, col = "skyblue") +
  geom_abline(slope = (1 / 1.25), intercept = 00, col = "orange") +
  geom_abline(slope = (1.25), intercept = 00, col = "orange") +
  geom_abline(slope = -1, intercept = 1000, col = "grey50") +
  geom_point(alpha = 0.1, show.legend = F) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:5) * 200) +
  labs(title = "Distribution of counts of extreme negative and positive opinions") +
  theme_light()
ggsave("distributions_exp11b.png", width = 7, height = 6)



# Measuring 'Black Pete' --------------------------------------------------

bp = do %>% rename(Negative = 12, Positive = 13) %>%
  mutate(blackPete = Positive >= 300 & (Positive > (Negative * 1.25)))
sum(bp$blackPete) / nrow(bp)

# How much is Black Pete Result polarized?
bp %>%
  ggplot() +
  aes(x = ESBG, fill = blackPete) +
  geom_histogram(alpha = 0.6, binwidth = 0.03) +
  labs(title = "Distribution of ESBG of simulations\naccording fullfilling 'Black Pete result'",
       caption = "'Black Pete result': at least 30% of agents are extremely positive (opinion >=0.5) and\nthere are at least of 25% more extremely positive than extremely negative agents.") +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave("blackPete_exp11_ESBG.png", width = 7, height = 6)

# Which parameters predict 'Black Pete Result'?
model = glm(blackPete~foldingPoint+communicationRate+forgeting+acceptanceAv, family="binomial", data=bp)
options(scipen=999)
summary(model)

# Graph represinting these parameters' influence:
sbp = bp %>% group_by(foldingPoint, communicationRate, forgeting, acceptanceAv) %>%
  mutate(N = n()) %>%
  group_by(foldingPoint, communicationRate, forgeting, acceptanceAv, N) %>%
  summarise(blackPete = sum(blackPete)) %>% ungroup() %>%
  mutate(p = round(100 * blackPete / N, 1),
         foldingPoint = fct_rev(foldingPoint))
sbp %>%
  ggplot() +
  aes(y = p, fill = acceptanceAv, x = forgeting) +
  facet_grid(rows = vars(foldingPoint), cols = vars(communicationRate), labeller = 'label_value') +
  geom_col(position = position_dodge()) +
  labs(y = "probability of 'Black Pete Result'") +
  theme_classic()
ggsave("blackPete_exp11_params.png", width = 7, height = 6)



# regression --------------------------------------------------------------

ya11 = lm(SD ~ foldingPoint * forgeting * communicationRate,
          data = tb11)
yb11 = lm(manhattan ~ foldingPoint * forgeting * communicationRate,
          data = tb11)
yc11 = lm(ESBG ~ foldingPoint * forgeting * communicationRate,
          data = tb11)
za11 = lm(SD ~ foldingPoint * forgeting * communicationRate * acceptanceAv,
         data = tb11)
zb11 = lm(manhattan ~ foldingPoint * forgeting * communicationRate * acceptanceAv,
         data = tb11)
zc11 = lm(ESBG ~ foldingPoint * forgeting * communicationRate  * acceptanceAv,
         data = tb11)

## Final tables
stargazer(ya11, za11,
          yb11, zb11,
          yc11, zc11,
          omit.stat = c("f", "ser"), type = "text", omit = 10:16005,
          add.lines = "Coefficients for interactions were supressed!")



# Information measures ----------------------------------------------------

# Discretization of dataset
td11 = tb11 %>%
  mutate(communicationRate = as.character(communicationRate) %>% as.numeric(),
         foldingPoint = as.character(foldingPoint) %>% as.numeric(),
         forgeting = as.character(forgeting) %>% as.numeric(),
         acceptanceAv = as.character(acceptanceAv) %>% as.numeric()) %>%
  select(foldingPoint, forgeting, communicationRate, acceptanceAv, SD, manhattan, ESBG) %>%
  infotheo::discretize(disc = "equalwidth") %>%
  mutate(combinationCrFFp = factor(communicationRate * 10000 + forgeting * 100 + foldingPoint),
         combinationAll = factor(acceptanceAv * 1000000 + communicationRate * 10000 + forgeting * 100 + foldingPoint))

# Normalized mutual information
exp(mutinformation(td11[, 8], td11[, 6])) %>% log(base = 192) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td11[, 8], td11[, 5])) %>% log(base = 192) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td11[, 8], td11[, 7])) %>% log(base = 192) %>% round(3) %>% paste0("ESBG: ", .)
exp(mutinformation(td11[, 9], td11[, 6])) %>% log(base = 1920) %>% round(3) %>% paste0("Manhattan: ", .)
exp(mutinformation(td11[, 9], td11[, 5])) %>% log(base = 1920) %>% round(3) %>% paste0("SD: ", .)
exp(mutinformation(td11[, 9], td11[, 7])) %>% log(base = 1920) %>% round(3) %>% paste0("ESBG: ", .)

# Fraction of variable-self mutual information
for (v in c(6, 5, 7)) {
  val = (100 * mutinformation(td11[, 8], td11[, v]) / mutinformation(td11[, v], td11[, v])) %>% round(2)
  val2 = (100 * mutinformation(td11[, 9], td11[, v]) / mutinformation(td11[, v], td11[, v])) %>% round(2)
  paste0("Combination CrFFp explains     : ", val,
         "% of posible information of variable '", names(td11)[v], "'.") %>% print()
  paste0("Combination LacMwCrFFp explains: ", val2,
         "% of posible information of variable '", names(td11)[v], "'.") %>% print()
}

# R^2 of regression models
stargazer(yb11, zb11, ya11, za11, yc11, zc11, type = "text", omit = 1:805,
          omit.stat = c("f", "ser"), add.lines = "Coeffs supressed!")
# Note: Ah yeah, still: normalized mutual info < fraction of mutual info < R^2.



# Experiment #12 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 15

# Loading results of the first seed
load("results12_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb12 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results12_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb12 = tb12 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb12 = tb12 %>%
  mutate(
    ESBG = ESBG / 2)

# Analysis of smooth data -------------------------------------------------
# Plan: We summarise data according pair of independent parameters of our interest,
#       we compute SD and average for each combination of values of these two params.
#       We have 5 parameters, so we get 10 pairs.
#       Firstly we prepare joint dataset 'jd' and
#       from this we will be doing pairwise summarisation.

# Computing mean values of opinion, information and attention
jd = tb12 %>% select(1, 4:7, 11, SD:ESBG,
                     starts_with(paste0("b_f", c("o", "a", "pi", "ni", "si", "i") , "_" ))) %>%
  pivot_longer(cols = 10:75, names_prefix = "b_f", names_sep = "_",
               names_to = c("Measure", "weight"), names_transform = list(weight = as.integer)) %>%
  mutate(weight = case_when(
    Measure == "o" ~ -1.2 + weight * 0.2,
    Measure == "a" ~ -0.1 + weight * 0.1,
    Measure == "pi" ~ -0.1 + weight * 0.1,
    Measure == "ni" ~ -0.1 + weight * 0.1,
    Measure == "si" ~ -0.2 + weight * 0.2,
    Measure == "i" ~ -1.2 + weight * 0.2),
    value = value * round(weight, 2) * 0.001,
    Measure = case_match(Measure, "a" ~ "Attention", "i" ~ "Information", "pi" ~ "Positive",
                         "ni" ~ "Negative", "si" ~ "Sum","o" ~ "Opinion")) %>%
  group_by(seed, foldingPoint, meanWeight, communicationRate, forgeting, acceptanceAv, SD, manhattan, ESBG, Measure) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  pivot_wider(id_cols = 1:9, names_from = "Measure") %>%
  # Now we filter conditions where 'Black Pete Scenario' happen:
  filter(foldingPoint < 0.15, forgeting > 0.45, acceptanceAv < 1.1)
# NICE! 'jd' is computed!


# Communicattion VS Forgetting --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(communicationRate, forgeting) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# Let's plot firstly SD, since it makes no sense to plot averages with high SD
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Opinion_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$ESBG_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Attention_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Sum_sd, type = "s", size = .5, col = rainbow(100))
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Communicattion VS Folding --------------------------------------------

df = jd %>%
  # mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(communicationRate, foldingPoint) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# Let's plot firstly SD, since it makes no sense to plot averages with high SD
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Opinion_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$ESBG_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Attention_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Sum_sd, type = "s", size = .5, col = rainbow(100))
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Communicattion VS Acceptance --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(communicationRate, acceptanceAv) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$acceptanceAv, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$acceptanceAv, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$acceptanceAv, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$acceptanceAv, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Communicattion VS MeanWeight --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(communicationRate, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$communicationRate, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$communicationRate, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$communicationRate, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(110))



# Forgetting VS Folding --------------------------------------------

df = jd %>%
  # mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(foldingPoint, forgeting) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$forgeting, y = df$foldingPoint, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(60))
plot3d(x = df$forgeting, y = df$foldingPoint, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(60))
plot3d(x = df$forgeting, y = df$foldingPoint, z = df$Attention_mean, type = "s", size = .5, col = rainbow(60))
plot3d(x = df$forgeting, y = df$foldingPoint, z = df$Sum_mean, type = "s", size = .5, col = rainbow(60))


# Forgetting VS Acceptance --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(forgeting, acceptanceAv) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$forgeting, y = df$acceptanceAv, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(150))
plot3d(x = df$forgeting, y = df$acceptanceAv, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$forgeting, y = df$acceptanceAv, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$forgeting, y = df$acceptanceAv, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))


# Forgetting VS MeanWeight --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(forgeting, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$forgeting, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$forgeting, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$forgeting, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$forgeting, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))


# Folding VS Acceptance --------------------------------------------

df = jd %>%
  # mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(foldingPoint, acceptanceAv) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$foldingPoint, y = df$acceptanceAv, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(350))
plot3d(x = df$foldingPoint, y = df$acceptanceAv, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$foldingPoint, y = df$acceptanceAv, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$foldingPoint, y = df$acceptanceAv, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))


# Folding VS MeanWeight --------------------------------------------

df = jd %>%
  # mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(foldingPoint, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$foldingPoint, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(355))
plot3d(x = df$foldingPoint, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$foldingPoint, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$foldingPoint, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))


# Acceptance VS MeanWeight --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(acceptanceAv, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$acceptanceAv, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(160))
plot3d(x = df$acceptanceAv, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$acceptanceAv, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$acceptanceAv, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))



# Experiment #13 -----------------------------------------------------------

# Loading data ------------------------------------------------------------

# Useful constant -- how many seeds are completely simulated:
completedSeeds = 43

# Loading results of the first seed
load("results13_seeds_1.RData")

# Preparing base of 'tb4' from 'results'
tb13 = results %>% drop_na()

# Adding data from the third experiment:
for (f in 2:completedSeeds) {
  load(paste0("results13_seeds_", f, ".RData"))
  results =  results %>% drop_na()
  tb13 = tb13 %>%
    add_row(results)
}

# Normalization of ESBG and factorisation of other variables:
tb13 = tb13 %>%
  mutate(
    ESBG = ESBG / 2)#,
    # across(reinforce:acceptanceSD, ~ factor(.x)))


# Loading records of individual traces ------------------------------------

# Creating "empty" row initializing combining and later erasing:
tr  = tibble(ID = -1, infoPos = -1, infoNeg = -1, attention = -1, opinion = -1,
             round = -1, seed = -1, communicationRate = -1, forgeting = -1,
             foldingPoint = -1, storing = -1, meanWeight = -1, acceptanceAv = -1)

# Loading cycle:
for (s in 1:completedSeeds) {
  for (sim in 1:100) {
    load(paste0("record13_seeds_", s, "_sim_", sim, ".RData"))
    tr = tr %>% add_row(record)
  }
}

# Dropping initializing empty row:
tr = filter(tr, ID > 0)


# Analyzing hopping traces ------------------------------------------------

## Drawing one simulation run
#  Data preparation:
trx = tr %>% ungroup %>%
  filter(seed == 566) %>%
  mutate(sID = 10000 * seed + ID) %>%
  arrange(sID) %>%
  group_by(sID)

#  Drawing itself:
trx %>%
  ggplot() +
  aes(x = round, y = opinion, group = sID, col = factor(sID)) +
  geom_line(show.legend = F) +
  scale_color_viridis_d() +
  labs(title = paste0(
    "Seed = ", trx[1, "seed"],
    ", Communication rate = ", trx[1, "communicationRate"], ", Forgetting/ memorizing = ", trx[1, "forgeting"],
    ", Folding point = ", trx[1, "foldingPoint"], ",\nAmount of stored information = ", trx[1, "storing"],
    ", Weight of reinforced opinion = ", trx[1, "meanWeight"], ", Latitude of acceptance = ", trx[1, "acceptanceAv"]
  )) +
  theme_classic()
ggsave("individualSim.png", width = 10, height = 6)

## Drawing extreme long hops:
# Parameters:
hopTreshold = 1.65
backTreshold = 0.05

# We create file which coins agents performing to big hops there and back:
key = tr %>%
  mutate(sID = 10000 * seed + ID) %>%
  arrange(sID, round) %>%
  group_by(sID) %>%
  mutate(hop = opinion - lag(opinion), back = abs(hop + lead(hop))) %>%
  ungroup %>%
  filter(abs(hop) > hopTreshold, (back / abs(hop)) < backTreshold) %>%
  select(sID) %>% unique()

# Now we join data with the key and select just keyed cases:
trx = tr %>%
  mutate(sID = 10000 * seed + ID) %>%
  arrange(sID) %>%
  right_join(key)

# Drawing extreme trajectories:
trx %>%
  ggplot() +
  aes(x = round, y = opinion, group = sID, col = factor(sID)) +
  geom_line(show.legend = F) +
  scale_color_viridis_d() +
  labs(title = paste0(
    "Extreme hops (n=", nrow(trx) / 101,
    "): We include all agents performing at least one extreme hop during sim.\n",
    "Extreme hop = change of opinion between two rounds ('hop') by ", hopTreshold,
    " and\nratio of sum of two consecutive 'hops' to original 'hop' ", backTreshold
  )) +
  theme_classic()
ggsave("extremeCases.png", width = 10, height = 6)


# Visual analysis in the same style as EXP 12 ------------------------------

#  # Analysis of smooth data -------------------------------------------------
# Plan: We summarise data according pair of independent parameters of our interest,
#       we compute SD and average for each combination of values of these two params.
#       We have 5 parameters, so we get 10 pairs.
#       Firstly we prepare joint dataset 'jd' and
#       from this we will be doing pairwise summarisation.

# Computing mean values of opinion, information and attention
jd = tb13 %>% select(1, 4:8, 12, SD:ESBG,
                     starts_with(paste0("b_f", c("o", "a", "pi", "ni", "si", "i") , "_" ))) %>%
  pivot_longer(cols = 11:76, names_prefix = "b_f", names_sep = "_",
               names_to = c("Measure", "weight"), names_transform = list(weight = as.integer)) %>%
  mutate(weight = case_when(
    Measure == "o" ~ -1.2 + weight * 0.2,
    Measure == "a" ~ -0.1 + weight * 0.1,
    Measure == "pi" ~ -0.1 + weight * 0.1,
    Measure == "ni" ~ -0.1 + weight * 0.1,
    Measure == "si" ~ -0.2 + weight * 0.2,
    Measure == "i" ~ -1.2 + weight * 0.2),
    value = value * round(weight, 2) * 0.001,
    Measure = case_match(Measure, "a" ~ "Attention", "i" ~ "Information", "pi" ~ "Positive",
                         "ni" ~ "Negative", "si" ~ "Sum","o" ~ "Opinion")) %>%
  group_by(seed, foldingPoint, meanWeight, communicationRate, forgeting, storing, acceptanceAv, SD, manhattan, ESBG, Measure) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  pivot_wider(id_cols = 1:10, names_from = "Measure") %>%
  # Now we filter conditions where 'Black Pete Scenario' happen:
  filter(foldingPoint < 0.15, forgeting > 0.45#, acceptanceAv < 1.1
         )
# NICE! 'jd' is computed!


# Communicattion VS Forgetting --------------------------------------------

df = jd %>%
  mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(communicationRate, forgeting) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# Let's plot firstly SD, since it makes no sense to plot averages with high SD
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Opinion_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$ESBG_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Attention_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Sum_sd, type = "s", size = .5, col = rainbow(100))
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$forgeting, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Communicattion VS Folding --------------------------------------------

df = jd %>%
  # mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(communicationRate, foldingPoint) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# Let's plot firstly SD, since it makes no sense to plot averages with high SD
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Opinion_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$ESBG_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Attention_sd, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Sum_sd, type = "s", size = .5, col = rainbow(100))
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$foldingPoint, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Communicattion VS Acceptance --------------------------------------------

df = jd %>%
  mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(communicationRate, acceptanceAv) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$acceptanceAv, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$acceptanceAv, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$acceptanceAv, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$acceptanceAv, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Communicattion VS MeanWeight --------------------------------------------

df = jd %>%
  mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(communicationRate, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$communicationRate, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$communicationRate, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$communicationRate, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(110))



# Forgetting VS Folding --------------------------------------------

df = jd %>%
  # mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(foldingPoint, forgeting) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$forgeting, y = df$foldingPoint, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(60))
plot3d(x = df$forgeting, y = df$foldingPoint, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(60))
plot3d(x = df$forgeting, y = df$foldingPoint, z = df$Attention_mean, type = "s", size = .5, col = rainbow(60))
plot3d(x = df$forgeting, y = df$foldingPoint, z = df$Sum_mean, type = "s", size = .5, col = rainbow(60))


# Forgetting VS Acceptance --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(forgeting, acceptanceAv) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$forgeting, y = df$acceptanceAv, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(150))
plot3d(x = df$forgeting, y = df$acceptanceAv, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$forgeting, y = df$acceptanceAv, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$forgeting, y = df$acceptanceAv, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))


# Forgetting VS MeanWeight --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(forgeting, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$forgeting, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$forgeting, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$forgeting, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$forgeting, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))


# Folding VS Acceptance --------------------------------------------

df = jd %>%
  # mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(foldingPoint, acceptanceAv) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$foldingPoint, y = df$acceptanceAv, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(350))
plot3d(x = df$foldingPoint, y = df$acceptanceAv, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$foldingPoint, y = df$acceptanceAv, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$foldingPoint, y = df$acceptanceAv, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))


# Folding VS MeanWeight --------------------------------------------

df = jd %>%
  # mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(foldingPoint, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$foldingPoint, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(355))
plot3d(x = df$foldingPoint, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$foldingPoint, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$foldingPoint, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))


# Acceptance VS MeanWeight --------------------------------------------

df = jd %>%
  mutate(across(2:6, ~round(.x, 1))) %>%
  group_by(acceptanceAv, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$acceptanceAv, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(160))
plot3d(x = df$acceptanceAv, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$acceptanceAv, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(1785))
plot3d(x = df$acceptanceAv, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(1785))



# Storing VS Communication Rate --------------------------------------------

df = jd %>%
  mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(communicationRate, storing) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$communicationRate, y = df$storing, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$storing, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$storing, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$communicationRate, y = df$storing, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Storing VS Forgetting --------------------------------------------

df = jd %>%
  mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(storing, forgeting) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$storing, y = df$forgeting, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$forgeting, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$forgeting, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$forgeting, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Storing VS Folding --------------------------------------------

df = jd %>%
  # mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(storing, foldingPoint) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$storing, y = df$foldingPoint, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$foldingPoint, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$foldingPoint, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$foldingPoint, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Storing VS Acceptance --------------------------------------------

df = jd %>%
  mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(storing, acceptanceAv) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$storing, y = df$acceptanceAv, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$acceptanceAv, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$acceptanceAv, z = df$Attention_mean, type = "s", size = .5, col = rainbow(100))
plot3d(x = df$storing, y = df$acceptanceAv, z = df$Sum_mean, type = "s", size = .5, col = rainbow(100))


# Storing VS MeanWeight --------------------------------------------

df = jd %>%
  mutate(across(2:7, ~round(.x, 1))) %>%
  group_by(storing, meanWeight) %>%
  summarise(across(SD:Sum, list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  arrange(Opinion_mean)
# OK, so sum of information makes sense...
plot3d(x = df$storing, y = df$meanWeight, z = df$Opinion_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$storing, y = df$meanWeight, z = df$ESBG_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$storing, y = df$meanWeight, z = df$Attention_mean, type = "s", size = .5, col = rainbow(110))
plot3d(x = df$storing, y = df$meanWeight, z = df$Sum_mean, type = "s", size = .5, col = rainbow(110))




# Measuring 'Black Pete' --------------------------------------------------

# Computing agents with opinion 0.5+
dp = tb13 %>% select(1, 4:8, 12, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 11:21, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight >= 9) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting, storing,
           acceptanceAv, SD, manhattan, ESBG) %>%
  summarise(extreme_positive_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, storing, meanWeight, acceptanceAv)

# Computing agents with opinion less than -0.5
dn = tb13 %>% select(1, 4:8, 12, SD:ESBG, starts_with(paste0("b_f", c("o") , "_" ))) %>%
  pivot_longer(cols = 11:21, names_prefix = "b_fo_",
               names_to = c("weight"), names_transform = list(weight = as.integer)) %>%
  filter(weight <= 3) %>%
  group_by(seed, meanWeight, foldingPoint, communicationRate, forgeting, storing,
           acceptanceAv, SD, manhattan, ESBG) %>%
  summarise(extreme_negative_opinion = sum(value)) %>% ungroup() %>%
  arrange(seed, foldingPoint, communicationRate, forgeting, storing, meanWeight, acceptanceAv)

# Joining both extremes:
do = dn %>% right_join(dp)

# Graphs
do %>%
  select(acceptanceAv, Negative = 11, Positive = 12) %>%
  mutate(acceptanceAv = round(acceptanceAv, 1) %>% factor()) %>%
  ggplot() +
  aes(x = Positive, y = Negative, col = acceptanceAv) +
  facet_wrap(vars(acceptanceAv), labeller = "label_value") +
  geom_abline(slope = 1, col = "skyblue") +
  geom_abline(slope = (1 / 1.25), intercept = 00, col = "orange") +
  geom_abline(slope = (1.25), intercept = 00, col = "orange") +
  geom_abline(slope = -1, intercept = 1000, col = "grey50") +
  geom_point(alpha = 0.3, show.legend = F) +
  scale_x_continuous(breaks = (0:5) * 200) +
  scale_y_continuous(breaks = (0:5) * 200) +
  labs(title = "Distribution of counts of extreme negative and positive opinions") +
  theme_light()
ggsave("distributions_exp13b.png", width = 7, height = 6)


# Operationalizing 'Black Pete Result':
bp = do %>% rename(Negative = 11, Positive = 12) %>%
  mutate(blackPete = Positive >= 300 & (Positive > (Negative * 1.25)))
sum(bp$blackPete) / nrow(bp)

# Which parameters predict 'Black Pete Result'?
bpx = bp %>% mutate(across(2:7, ~ (round(.x / 2, 1) * 2)  %>% factor()))
# model = glm(blackPete~foldingPoint+communicationRate+forgeting+acceptanceAv+storing+meanWeight, family="binomial", data=bpx)
# options(scipen=999)
# summary(model)
# bp[,2:7] %>% summary()

# Graph represinting these parameters' influence:
sbp = bpx %>% group_by(foldingPoint,
                       #communicationRate,
                       forgeting,
                       storing,
                       # meanWeight,
                       acceptanceAv) %>%
  mutate(N = n()) %>%
  group_by(foldingPoint,
           # communicationRate,
           forgeting,
           acceptanceAv,
           storing,
           # meanWeight,
           N) %>%
  summarise(blackPete = sum(blackPete)) %>% ungroup() %>%
  mutate(p = round(100 * blackPete / N, 1),
         across(c(#communicationRate,
                  # storing,
                  acceptanceAv,
                  foldingPoint), ~ fct_rev(.x)))
sbp %>%
  filter(p > 0) %>%
  ggplot() +
  aes(y = p, x = storing, fill = foldingPoint) +
  facet_grid(rows = vars(#communicationRate,
                         acceptanceAv
                         ),
             cols = vars(forgeting#,
                         # meanWeight
                         ),
             labeller = 'label_both') +
  geom_col(alpha = 0.7, position = position_dodge(preserve = "single")) +
  labs(y = "probability of 'Black Pete Result'") +
  scale_fill_viridis_d(option = "D") +
  theme_light()
ggsave("blackPete_exp13_params.png", width = 7, height = 6)



# Is Black Pete result log-log distributed?
sbp %>%
  ggplot() +
  aes(x = blackPete) +
  geom_histogram(fill = "skyblue", alpha = 0.8) +
  # scale_y_log10() +
  # scale_x_log10() +
  theme_light()
# Graph says 'No...'







