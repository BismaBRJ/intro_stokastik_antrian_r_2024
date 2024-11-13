# Pengantar Model Stokastik dan Teori Antrian

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

## === Simulasi CTMC dengan markovchain ===

mc2_Q <- matrix(
  c(-0.5, 0.3, 0.2,
    0.4, -0.7, 0.3,
    0.1, 0.4, -0.5),
  nrow = 3,
  byrow = TRUE)
mc2_Q

mc2_states <- c("A", "B", "C")

mc2_obj <- new("ctmc",
               states = mc2_states,
               generator = mc2_Q)
mc2_obj

plot(mc2_obj)

?rctmc
mc2_n <- 10
set.seed(123)
mc2_sim <- rctmc(n = mc2_n, ctmc = mc2_obj,
                 initDist = c(1, 0, 0))
print(mc2_sim)
mc2_sim[[1]]
mc2_sim[[2]]
data.frame(new.state = mc2_sim[[1]],
           time = mc2_sim[[2]])

library(expm)
mc2_P <- expm(mc2_Q * mc2_n)
mc2_P

mc2_sim_factors <- factor(mc2_sim[[1]], levels = mc2_states)
mc2_sim_factors
mc2_sim_int <- as.integer(mc2_sim_factors)
mc2_sim_int
plot(x = mc2_sim[[2]],
     y = mc2_sim_int,
     xlab = "Waktu (kontinu)",
     ylab = "State",
     main = "Simulasi CTMC",
     yaxt = "n")
axis(2, at = 1 : length(mc2_states), labels = mc2_states)

### === Analisis CTMC ===

dim(mc2_obj)

states(mc2_obj)

# steady-state distribution (probabilitas jangka panjang)
steadyStates(mc2_obj)

## === Simulasi Proses Poisson ===

pp_lambda <- 2

pp_n <- 10
set.seed(2024)
# bilangan random sebanyak pp_n dari distribusi Exp(pp_lambda):
pp_interarrival <- rexp(pp_n, rate = pp_lambda)
pp_interarrival
# jumlahan kumulatif
pp_arrival <- cumsum(pp_interarrival)

pp_arrival

?plot
plot(x = pp_arrival,
     y = 1 : length(pp_arrival),
     type = "s",
     xlab = "Waktu",
     ylab = "Kedatangan",
     main = "Simulasi Proses Poisson dengan lambda = 2")

## === Simulasi Birth-Death Process dengan markovchain ===

bdp_lambda <- 0.5
bdp_mu <- 0.3

# Birth-Death Process adalah CTMC dengan matriks Q tridiagonal
bdp_Q <- matrix(
  c(-bdp_lambda, bdp_lambda,          0,
    bdp_mu,     -(bdp_mu+bdp_lambda), bdp_lambda,
    0,           bdp_mu,              -bdp_mu),
  byrow = TRUE,
  nrow = 3
)
bdp_Q

bdp_states <- 0 : 2
as.character(bdp_states)

bdp_obj <- new("ctmc",
               states = as.character(bdp_states),
               generator = bdp_Q)
bdp_obj

plot(bdp_obj)

bdp_n <- 10
set.seed(123)
bdp_sim <- rctmc(n = bdp_n, ctmc = bdp_obj,
                 initDist = c(1, 0, 0))
print(bdp_sim)
bdp_sim[[1]]
as.numeric(bdp_sim[[1]])
bdp_sim[[2]]
length(bdp_sim[[1]])

library(expm)
bdp_P <- expm(bdp_Q * bdp_n)
bdp_P

plot(x = bdp_sim[[2]],
     y = as.numeric(bdp_sim[[1]]),
     type = "s",
     xlab = "Waktu (kontinu)",
     ylab = "State",
     main = "Simulasi Birth-Death Process")

### === Analisis Birth-Death Process ===

dim(bdp_obj)

states(bdp_obj)
as.numeric(states(bdp_obj))

# steady-state distribution (probabilitas jangka panjang)
steadyStates(bdp_obj)

## === Simulasi M/M/c dengan queuecomputer ===

#install.packages("queuecomputer")
library("queuecomputer")

mmc_lambda <- 5
mmc_mu <- 2
mmc_n <- 10 # total banyaknya pelanggan sampai akhir

mmc_c <- 3 # nilai c (banyaknya server/pelayan) untuk M/M/c
# kalau c=1, jadilah M/M/1

set.seed(123)
mmc_interarrival <- rexp(mmc_n, rate = mmc_lambda)
mmc_arrival <- cumsum(mmc_interarrival)
mmc_service <- rexp(mmc_n, rate = mmc_mu)

?queue_step
mmc_sim <- queue_step(arrivals = mmc_arrival,
                      service = mmc_service,
                      servers = mmc_c)
print(mmc_sim)
summary(mmc_sim)

plot(mmc_sim)[[1]]
plot(mmc_sim)[[2]]
plot(mmc_sim)[[3]]
plot(mmc_sim)[[4]]
plot(mmc_sim)[[5]]

# data lengkap hasil simulasi
mmc_sim$departures_df

mmc_sim$server
mmc_sim$server == 1

mmc_sim$systemlength_df
plot(mmc_sim$systemlength_df,
     type = "s",
     xlab = "Waktu (kontinu)",
     ylab = "Panjang sistem",
     main = "Simulasi M/M/c (plot sistem)")

mmc_sim_times <-
  sort(c(c(0),
         mmc_sim$departures_df$arrivals,
         mmc_sim$departures_df$departures))
mmc_sim_times
mmc_sim_queuelengths_df <- data.frame(times = mmc_sim_times)
for (s in as.character(1 : mmc_c) ) {
  arr_times <- subset(mmc_sim$departures_df, server == s)$arrivals
  dep_times <- subset(mmc_sim$departures_df, server == s)$departures
  mmc_sim_queuelengths_df[[s]] <- rep(0, length(mmc_sim_times))
  for (i in 1 : length(mmc_sim_times)) {
    cur_time <- mmc_sim_queuelengths_df[["times"]][i]
    mmc_sim_queuelengths_df[[s]][i] <-
      sum(arr_times <= cur_time) - sum(dep_times <= cur_time)
  }
}
mmc_sim_queuelengths_df

?matplot
matplot(x = mmc_sim_times,
        y = mmc_sim_queuelengths_df[as.character(1 : mmc_c)],
        type = "s",
        col = 2 : (mmc_c + 1),
        xlab = "Waktu (kontinu)",
        ylab = "Panjang antrian",
        main = "Simulasi M/M/c (plot per antrian)")
legend("topright",
       legend = 1 : mmc_c,
       col = 2 : (mmc_c + 1),
       pch = 1)

## === Analisis M/M/c dengan queueing ===

#install.packages("queueing")
library("queueing")

mmc_lambda <- 30
mmc_mu <- 12
mmc_c <- 3

?NewInput.MMC
mmc_input <- NewInput.MMC(
  lambda = mmc_lambda,
  mu = mmc_mu,
  c = mmc_c
)

?CheckInput
CheckInput(mmc_input)

?QueueingModel
mmc_model <- QueueingModel(mmc_input)
summary(mmc_model)
