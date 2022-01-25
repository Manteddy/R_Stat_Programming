#-----------------------------------------
# Write a function that takes inputs a; b; n; y; alpha
# and produces the following outputs;
#
# 1. Parameters of the posterior distribution
# 2. Posterior mean and variances
# 3. Weights of prior and the of data in the posterior mean
# 4. Credible interval

BinBeta_fun <- function(a, b, n, y, alpha){
  #Inputs:
  # a:    parameter of bets prior
  # b:    parameter of beta prior
  # n:    number of trials
  # y:    number of successes
  # alpha:    (1-alpha) * 100 credible interval
  
  # 1. Parameters of the posterior distribution
  a1 <- a + y
  b1 <- b + n - y
  
  # 2. Posterior mean and variances
  m_pos <- a1 / (a1 + b1)
  var_pos <- a1 * b1 / ((a1 + b1)^2 * (a1 + b1 + 1))
  
  # 3. Weights of prior and the of data in the posterior mean
  w_prior <- (a + b) / (a + b + n)
  w_data <- n / (a + b + n)
  
  # 4. Credible interval
  qlow <- alpha / 2
  qup <- 1- alpha / 2
  
  Low <- qbeta(qlow, a1, b1)
  Up <- qbeta(qup, a1, b1)
  
  return()
}