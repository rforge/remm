set.seed(1234)

## simulated data
mu <- cbind(
    x = c(0, 0.2,1,0.9),
    y = c(0, 0.7,1,0.2)
)

sd_rho <- cbind(
    x = c(0.2, 0.15, 0.05, 0.02),
    y = c(0.1, 0.04, 0.03, 0.05),
    rho = c(0, 0.7, 0.3, -0.4)
)

Sigma <- lapply(1:nrow(sd_rho), FUN = function(i) rbind(
        c(sd_rho[i,"x"]^2, sd_rho[i,"rho"]*sd_rho[i,"x"]*sd_rho[i,"y"]),
        c(sd_rho[i,"rho"]*sd_rho[i,"x"]*sd_rho[i,"y"], sd_rho[i,"y"]^2)))


sequence <- c(1,1,1,2,3,2,4)
n <- 20

EMMsim_sequence_train <- rep(sequence, n)
EMMsim_sequence_test <- rep(sequence, 2)

library("MASS")
EMMsim_train <- t(sapply(EMMsim_sequence_train, FUN = function(i) 
        mvrnorm(1, mu=mu[i,], Sigma=Sigma[[i]])))

EMMsim_test <- t(sapply(rep(EMMsim_sequence_test), FUN = function(i) 
        mvrnorm(1, mu=mu[i,], Sigma=Sigma[[i]])))


save(EMMsim_train, EMMsim_sequence_train, EMMsim_test, 
    EMMsim_sequence_test, file="EMMsim.rda")
