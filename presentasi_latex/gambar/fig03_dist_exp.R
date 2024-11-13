x_exp <- seq(0, 10, by = 0.1)
p_exp <- dexp(x_exp, rate = 3)
plot(x_exp, p_exp)
?dexp
