BayesNorm <- function(prior, data, type = 1, N_size){
  if (type == 1){
    a <- mean(data) * (length(data)/var(data))
    b <- prior[1] * (1/prior[2])
    c <- length(data)/var(data) + (1/prior[2])
    posterior.mean <- (a + b)/c
    posterior.var <- 1/c
  } else {
    if (type == 2){
      if(missing(N_size)){
        print("missing sample size")
        return
      } else {
        n <- N_size
        a <- data[1] * (n/data[2])
        b <- prior[1] *(1/prior[2])
        c <- (n/data[2]) + (1/prior[2])
        posterior.mean <- (a + b)/c
        posterior.var <- 1/c
      }
    }
  }
  (Posterior <- c(posterior.mean,posterior.var))
}


