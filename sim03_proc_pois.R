pp_lambda <- 4

pp_n <- 10
set.seed(2024)
# bilangan random sebanyak pp_n dari distribusi Exp(pp_lambda):
pp_interarrival <- rexp(pp_n, rate = pp_lambda)
pp_interarrival
# jumlahan kumulatif
pp_arrival <- cumsum(pp_interarrival)

pp_arrival

?plot
plot(x = c(0, pp_arrival),
     y = 0 : length(pp_arrival),
     type = "s",
     xlab = "Waktu",
     ylab = "Kedatangan",
     main = "Simulasi Proses Poisson dengan lambda = 4")
