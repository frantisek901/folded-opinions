## Some ashes from 'analysis.R'
##



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




# Testing of distributions ------------------------------------------------

# Creating testing data
N = 1000
mx = tibble(forgeting = rep(length.out = 32 * N, c(.2, .45, .7, .75, .8, .85, .9, .95), each = 4 * N),
            forget_sd = rep(length.out = 32 * N, c(0.025, 0.05, 0.1, 0.15), each = N, times = 8),
            value = rep(length.out = 32 * N, c(.2, .45, .7, .75, .8, .85, .9, .95), each = 4 * N)) %>%
  as.matrix(ncol = 3)
for (forgeting in c(.2, .45, .7, .75, .8, .85, .9, .95)) {
  for (forget_sd in c(0.025, 0.05, 0.1, 0.15)) {
    vx = rep(0, N)
    for (i in 1:N) {
      # Acceptance
      x = rnorm(n = 1, mean = forgeting, sd = forget_sd)
      while (x <= 0 | x >= 1) {
        x = rnorm(n = 1, mean = forgeting, sd = forget_sd)
      }
      vx[i] = x
    }
    mx[(mx[, "forgeting"] == forgeting & mx[, "forget_sd"] == forget_sd),3] = vx
  }
}
mx = as_tibble(mx) %>%
  add_row(
    tibble(forgeting = rep(length.out = 8 * N, c(.2, .45, .7, .75, .8, .85, .9, .95), each = N),
           forget_sd = rep(length.out = 8 * N, 0),
           value = rep(length.out = 8 * N, c(.2, .45, .7, .75, .8, .85, .9, .95), each = N))
  )
hist(mx$value, breaks = seq(0, 1, 0.05))


# Drawing histograms
mx %>%
  ggplot() +
  aes(x = value) +
  facet_grid(vars(forget_sd), vars(forgeting), labeller = "label_both") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  theme_classic()

mx %>%
  ggplot() +
  aes(x = value) +
  facet_grid(cols = vars(forget_sd), labeller = "label_both") +
  geom_histogram(fill = "steelblue", alpha = 0.4) +
  theme_classic()

# Computing means
mx %>% group_by(forget_sd) %>% summarise(value = mean(value), forgeting = mean(forgeting))
tb3 %>% group_by(forget_sd, forgeting) %>% summarise(ESBG = mean(ESBG), SD = mean(SD), Manhattan = mean(manhattan)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = forgeting, y = forget_sd, size = ESBG * 100, label = round(ESBG, 2)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_text(size = 5) +
  scale_size_identity() +
  theme_classic()



