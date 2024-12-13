dpois(2, lambda = 3)

ppois(2, lambda = 3)

dpois(0, lambda = 3) +
  dpois(1, lambda = 3) +
  dpois(2, lambda = 3)

par_lambda <- 3
x_pois <- 0 : 10
p_pois <- dpois(x_pois, lambda = par_lambda)
plot(x_pois, p_pois,
     main = "Distribusi Poisson dengan lambda = 3",
     xlab = "Banyaknya kemunculan dalam satu satuan waktu",
     ylab = "Probabilitas")