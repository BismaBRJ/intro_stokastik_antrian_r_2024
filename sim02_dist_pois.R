par_lambda <- 3
x_pois <- 0 : 10
p_pois <- dpois(x_pois, lambda = par_lambda)
plot(x_pois, p_pois,
     main = "Distribusi Poisson dengan lambda = 3",
     xlab = "Banyaknya kemunculan dalam satu satuan waktu",
     ylab = "Probabilitas")

dpois(2, lambda = par_lambda)

ppois(2, lambda = par_lambda)

dpois(0, lambda = par_lambda) +
  dpois(1, lambda = par_lambda) +
  dpois(2, lambda = par_lambda)
