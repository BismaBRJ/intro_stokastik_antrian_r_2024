# === Simulasi dan Analisis DTMC dengan markovchain ===

#install.packages("markovchain")
library("markovchain")

?matrix

mc1_P <- matrix(
  c(0.5,  0.25, 0.25, 0,
    0.25, 0.25, 0,    0.5,
    0.25, 0,    0.25, 0.5,
    0,    0.4,  0.4,  0.2),
  nrow=4,
  byrow=TRUE
)
mc1_P

mc1_states <- c("0", "1", "2", "3")

mc1_obj <- new("markovchain",
               states = mc1_states,
               transitionMatrix = mc1_P)
mc1_obj

plot(mc1_obj)

?rmarkovchain
mc1_t <- 10
set.seed(123)
mc1_sim <- rmarkovchain(n = mc1_t, object = mc1_obj,
                        t0 = "0")
mc1_sim

mc1_sim_factors <- factor(mc1_sim, levels = mc1_states)
mc1_sim_factors
mc1_sim_int <- as.integer(mc1_sim_factors)
mc1_sim_int
mc1_sim_times <- 1 : mc1_t
plot(x = mc1_sim_times,
     y = mc1_sim_int,
     xlab = "Waktu (diskrit)",
     ylab = "State",
     main = "Simulasi DTMC",
     xaxt = "n", yaxt = "n")
axis(1, at = mc1_sim_times)
axis(2, at = 1 : length(mc1_states), labels = mc1_states)

### === Analisis DTMC ===

dim(mc1_obj)

# nama state, sama aja
names(mc1_obj)
states(mc1_obj)

# summary/ringkasan tentang DTMC nya
summary(mc1_obj)

# steady-state distribution (probabilitas jangka panjang)
steadyStates(mc1_obj)