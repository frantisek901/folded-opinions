#### Analysis

## Encoding: windows-1250
## Created:  2023-07-05 FranÈesko
## Edited:   2023-08-02 FranÈesko

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
completedSeeds = 36

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
completedSeeds = 23

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
completedSeeds = 19

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
completedSeeds = 2

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

## Final tables
stargazer(ya7, yb7, yc7,
  omit.stat = c("f"), type = "text", omit = 13:1605,
  add.lines = "Coefficients for interactions were supressed!")



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
  arrange(communicationRate, foldingPoint, meanWeight, sdFractionWeight, forgeting)

# 3D graphs
plot3d(x = ty7$communicationRate, y = ty7$foldingPoint, z = ty7$ESBG, type = "s", size = .5, col = rainbow(9))
plot3d(x = ty7$communicationRate, y = ty7$foldingPoint, z = ty7$SD, type = "s", size = .5, col = rainbow(9))
plot3d(x = ty7$communicationRate, y = ty7$foldingPoint, z = ty7$manhattan, type = "s", size = .5, col = rainbow(9))
plot3d(x = ty7$SD, y = ty7$manhattan, z = ty7$ESBG, type = "s", size = .5, col = rainbow(9))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$SD_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$communicationRate, y = ty4$foldingPoint, z = ty4$manhattan_sd, type = "s", size = .5, col = rainbow(4))
# plot3d(x = ty4$SD_sd, y = ty4$manhattan_sd, z = ty4$ESBG_sd, type = "s", size = .5, col = rainbow(4))



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










