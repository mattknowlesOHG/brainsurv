library(rstan)
library(shinystan)
library(dplyr)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)

message("FITTING MODEL FOR OS")

Y <- DF %>% 
  filter(outcome == "OS")
Y <- Y$time

modelOS <- stan_model('model.stan')
fitOS <- sampling(modelOS, list(N = length(Y), Y = Y), iter = 1000, chains = 4)

message("DONE OS")
message("FITTING MODEL FOR PFS")

Z <- DF %>% 
  filter(outcome == "PFS")
Z <- Z$time

modelPFS <- stan_model('model.stan')
fitPFS <- sampling(modelPFS, list(N = length(Z), Y = Z), iter = 1000, chains = 4)

message("DONE PFS")

paramsOS <- extract(fitOS)
paramsPFS <- extract(fitPFS)

plot(density(paramsPFS$mu), xlab = "mu", ylab = "denisty", main = "Density plot of mu (PFS)")

summary(paramsPFS$sigma)
