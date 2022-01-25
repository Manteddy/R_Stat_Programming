
#-----------------------------------------
# Question 1
#-----------------------------------------

##########
# Q 1:1
##########
 a <- 0.5 
 d <- 4.2 
 x <- seq(1,10000,1)
 
 #The initial values
 x_values <- rep(0,10000) # 10 000 to cover all the needed values
 x_values[1] <- 1
 x_values[2] <- 2
 
 #Find the sequence values
 for (i in 3:10000){
   x_values[i] <- (a + d * (i-1) - 
      (x_values[i-2] / x_values[i-1]) ^ (x_values[i-1] * 1.1)) /
     x_values[i-1]
 }
 x_values
 round(digits = 2, x_values[500])
 round(digits = 2, x_values[5000])

 ##########
 # Q 1:2
 ##########
 plot(x, x_values, # x and y coordinates
      type = "l",
      col = "maroon",
      ylab = "x_n") 
 
 
 ##########
 # Q 1:3
 ##########
 
sum (x_values[1:1000]) # sum of the first 1000 sequence elements
median (x_values[1:1000]) # median of the first 1000 sequence elements
sd (x_values[1:1000]) # standart deviation the first 1000 sequence elements
 
 
##########
# Q 1:4
##########

ny <- seq(100, 10000, 100) #numbers of y sequence
nz <- seq(99, 9999, 100) #numbers of z sequence

y <- rep(0, length(ny)) 
z <- rep(0, length(nz))  

#Building an "y" sequence
j <- 1
for (i in ny){
  y[j] <- x_values[i]
  j <- j + 1
}
y

#Building a "z" sequence
j <- 1
for (i in nz){
  z[j] <- x_values[i]
  j <- j + 1
}
z

#Building a Y matrix
Y <- matrix(y, nrow = 10, byrow = TRUE)
Y

#Building a Z matrix
Z <- matrix(z, nrow = 10, byrow = TRUE)
Z

D <- t(Y) - Z # deriving D matrix
D
D[4,4]


#########
# Q 1:5  dpois()
##########

mu <- y[1:25]/10
prior_mu <- rep(1/25, 25)

#likelihood 
likelihood <- dpois(2, mu)
likelihood

weights_mu <- prior_mu * likelihood
posterior_mu <- weights_mu / sum(weights_mu)

#Building a table
data.frame(mu,prior_mu, likelihood, posterior_mu)

#Finding the mu values that maximizes the posterior probability
i <- which.max(posterior_mu)
max_posterior_mu <- mu[i]
max_posterior_mu








#-----------------------------------------
# Question 2
#-----------------------------------------

##########
# Q 2:1  
##########

set.seed(4321)
gamma_values <- function(r, v, n){
  gs <- rgamma (n, r, v)
  gamma_sample <- sample(gs, n, replace = TRUE)
  mgs <- mean(gamma_sample)
  vgs <- var(gamma_sample)
  Pgs <- 1 - pgamma(2,r,v)
  P5 <- qgamma(0.05, r ,v)
  P95 <- qgamma(0.95, r, v)
  return(list(mgs, vgs, Pgs, P5, P95))
}

gamma_values(40, 20, 10000)

##########
# Q 2:2  
##########

#Plotting a histogram
hist (rgamma(10000, 40, 20), main = "Density", freq = FALSE, xlab = "Value")                                                                                                                                     

# Plotting normal approximation
curve(dnorm(x, as.numeric(gamma_values(40, 20, 10000)[1]), 
                          sqrt(as.numeric(gamma_values(40, 20 ,10000)[2]))),
      add = TRUE, col = "green", ) 

# Plotting density curve
curve(dgamma(x, 40, 20), add = TRUE, col = "blue")

# Plotting percentiles
abline(v = qgamma(0.05, 40 ,20), col = "purple") # 5th perecntile
abline(v = qgamma(0.95, 40 ,20), 
       col = "red") # 95th percentile

# Showing the legend
legend(2.6, 1.2, 
       legend = c("Normal approximation", "Density curve", "5th percentile", "95th percentile"), 
       col = c("green", "blue", "purple", "red"), lty=1:4)







#-----------------------------------------
# Question 3
#-----------------------------------------

##########
# Q 3:1  
##########


BinBeta_fun <- function(a, b, n, y){
  #Inputs:
  # a:    parameter of beta prior
  # b:    parameter of beta prior
  # n:    number of trials
  # y:    number of successes
  
  # 0. Parameters of the posterior distribution
  a1 <- a + y
  b1 <- b + n - y
  
  # 1. Posterior mean and variances
  m_pos <- a1 / (a1 + b1)
  var_pos <- a1 * b1 / ((a1 + b1)^2 * (a1 + b1 + 1))

  print (m_pos)
  print (var_pos)
  
  # 2. Bayesian 90% credible interval
  qlow <- 0.1 / 2
  qup <- 1- 0.1 / 2
  
  Low <- qbeta(qlow, a1, b1)
  Up <- qbeta(qup, a1, b1)
  
  cat(Low, "< pi <",Up, "\r\n")
  
  Bayesian_Cr_Int_Width <- Up - Low
  
  # 3. Compare with credible interval from normal approximation
  qlow <- 0.1 / 2
  qup <- 1 - 0.1 / 2
  
  Low_n <- qnorm(qlow, m_pos, sqrt(var_pos))
  Up_n <- qnorm(qup, m_pos, sqrt(var_pos))
  
  Bayesian_Cr_Int_Width_n <- Up_n - Low_n
  
 
  if (Bayesian_Cr_Int_Width_n > Bayesian_Cr_Int_Width){
    cat("Credible interval from normal approximation
        is bigger than from beta distribution")
  } else if (Bayesian_Cr_Int_Width_n == Bayesian_Cr_Int_Width){
    cat("Credible interval from normal approximation
        is equal to from beta distribution")
  } else {
    cat("Credible interval from normal approximation
        is smaller than from beta distribution")
  }
  
  # Plotting posterior and prior densities
  curve(dbeta(x, a1, b1), #posterior density
        add = FALSE, col = "blue", xlab = "Pi", ylab = "Density", lwd = 2)
  curve(dbeta(x, a, b),   #prior density
        add = TRUE, col = "maroon", xlab = "Pi", ylab = "Density", lwd = 2)
  abline (v = Low, col = "orange", lwd = 1, lty = "dashed")
  abline (v = Up, col = "orange", lwd = 1, lty = "dashed")
  abline (v = Low_n, col = "green", lwd = 1, lty = "dashed")
  abline (v = Up_n, col = "green", lwd = 1, lty = "dashed")
  
  legend(0.0, 3.3, 
         legend = c("Prior density", "Posterior density", 
                   "Beta credible interval", "Normal credible interval"),
         col = c("maroon", "blue", "orange", "green"), 
         lwd = c(2, 2, 1, 1),
         lty = c("solid", "solid", "dashed", "dashed"),
         cex = 0.8)


}




##########
# Q 3:2  
##########

find_prior <- function(mbel, stdevbel){
  # mbel:   mean from our belief
  # stdevbel    standard deviation from our belief
  
  # mbel = a / (a + b)
  # var = stdevbel^2 = a * b / ((a + b)^2 * (a + b + 1))
  
  # Solving the equation on paper get the formula:
  a <- ((1 - mbel) * mbel^2) / stdevbel^2 - mbel
  b <- (a - mbel * a) / mbel
  
  cat("The prior beta is", "beta(", a, ", ", b, ")\n")

  
  # Prior probability that P(pi > 0.5)
  P1 <- 1 - pbeta(0.5, a, b)
  
  # Prior probability that P(pi < 0.8)
  P2 <- pbeta(0.8, a, b)
  
  cat("Prior probability that P(pi > 0.5) equals", P1, "\n")
  cat("Prior probability that P(pi < 0.8) equals", P2, "\n\n" )
  
  # Prior density plot
  curve (dbeta(x, a, b), ylim = c(0, 15), 
         col = 6, xlab = "x", ylab = "Density",
         lty = "dotted", add = FALSE)
  
}

find_prior(0.5, 0.1)

# Set the survey results for the posterior distribution
survres <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
               0, 0, 0, 1, 0, 1, 0, 0, 0, 0) # 0 for NO and 1 for YES
  
# Create a function to find the posterior distribution
# This function will take the parameters of your prior beta distribution
# You can type in a and b of the findprior function
  
y_pos <- 0
find_posterior <- function(a_pr, b_pr){
    # a_pr: parameter of prior distribution
    # b_pr: parameter of prior distribution 
    
    for (i in 1:length(survres)){
      y_pos <- y_pos + survres[i]
      a1 <- a_pr + y_pos
      b1 <- b_pr + i - y_pos
      cat("The posterior beta after ", i, 
          "`th trial", "is", "beta(", a1, ", ", b1, ")\n")
    }
      
}
  

# The posterior probability that 
# P(π|evidence > 0.5) and P(π|evidence < 0.8)
# We will write a function similar to findposterior

y_pos <- 0
post_evid_prob <- function(a_pr, b_pr){
  # a_pr: parameter of prior distribution
  # b_pr: parameter of prior distribution 
  
  for (i in 1:length(survres)){
    y_pos <- y_pos + survres[i]
    a1 <- a_pr + y_pos
    b1 <- b_pr + i - y_pos
  }
  # The posterior probability that P(π|evidence > 0.5)
  P3 <- 1 - pbeta(0.5, a1, b1)
  cat("The posterior probability that P(π|evidence > 0.5) equals", P3, "\n")
  
  # The posterior probability that P(π|evidence < 0.8)
  P4 <- pbeta(0.8, a1, b1)
  cat("The posterior probability that P(π|evidence < 0.8) equals", P4, "\n")
}

# find_prior(0.5, 0.1) - checking the findprior function
# find_posterior(12,12) - checking the findposterior function
# post_evid_prob(12,12) - checking the post_evid_prob function
 
plot_posterior <- function(a_pr, b_pr){
  # a_pr: parameter of prior distribution
  # b_pr: parameter of prior distribution 
  curve(dbeta(x, a_pr, b_pr), add = FALSE, ylim = c(0, 5.7), ylab = "Beta function")
  
  for (i in c(1, 5, 10, 15, 20)){
    y_pos <- y_pos + survres[i]
    a1 <- a_pr + y_pos
    b1 <- b_pr + i - y_pos
    curve(dbeta(x, a1, b1), add = TRUE, col = 3*i, lty = i, lwd)
  }
  
  legend (0.65, 5.6, cex = 0.8, 
          legend = c("beta(12, 12) (prior)", "beta(12, 13)", "beta(13, 16)", 
                  "beta(13, 21)", "beta(14, 25)", "beta(15, 29)"), 
          col = c("black", 3, 15, 30, 45, 60), lty = c(1, 1, 5, 10 ,15, 20))
}

 plot_posterior(12,12) # checking the function for 12,12 values



##########
# Q 3:2  
##########

# Our prior: a = 12, b = 12, evidence = survres
# Our posterior (from find_posterior) beta(15, 29)
a1 <- 15
b1 <- 29

# Bayesian 90% credible interval
qlow <- 0.1 / 2
qup <- 1- 0.1 / 2

Low <- qbeta(qlow, a1, b1) # lower quantile
Up <- qbeta(qup, a1, b1) # upper quantile



# Test the (a) hypothesis
mu_0 <- 30 / 100 # the critical mean value from the initial conditions
# we use the one-tailed test, because it should be 
#fewer than or equal to 30
if (mu_0 > Low){ 
  cat("\nWe fail to reject the hypothesis")
} else {
  cat("\nWe reject the hypothesis")
} 



# Test the (a) hypothesis
mu_0 <- 7 / 100 # the critical mean value from the initial conditions
# we use the two-tailed test, because it should be about 7
if (mu_0 > Low & mu_0 < Up){ 
  cat("\nWe fail to reject the hypothesis")
} else {
  cat("\nWe reject the hypothesis")
} 


hypothesis_check <- function(a1_h, b1_h, n, y, alpha, tailnum){
  # a1_h:     parameter of the posterior distribution
  # b1_h:     parameter of the posterior distribution
  # n:      number of people
  # y:      number of people who had covid
  # alpha:  significance level 
  # tailnum:    has values (0, 1, 2) for (fewer than, about, higher than)
  
  qlow_h <- alpha / 2
  qup_h <- 1 - alpha / 2
  
  Low_h <- qbeta(qlow_h, a1_h, b1_h) #lower quantile
  Up_h <- qbeta(qup_h, a1_h, b1_h) # upper quantile
  
  mu_0 <- y / n
   if (tailnum == 0){
     
     
     if (mu_0 > Low_h){ # if number of people who had covid is fewer than mu_0
       cat("\nWe fail to reject the hypothesis")
     } else {
       cat("\nWe reject the hypothesis")
     }  
     
     
   } else if (tailnum == 1){ 
     
     
     if (mu_0 > Low_h & mu_0 < Up_h){ # if number of people who had covid is about mu_0
       cat("\nWe fail to reject the hypothesis")
     } else {
       cat("\nWe reject the hypothesis")
     } 
     
     
   } else if(tailnum == 2) {
     
     
     if (mu_0 < Up_h){ # if number of people who had covid is higher than mu_0
       cat("\nWe fail to reject the hypothesis")
     } else {
       cat("\nWe reject the hypothesis")
     } 
     
     
   }
  
}

#hypothesis_check(15, 29, 100, 30, 0.1, 0) #check for (a)
#hypothesis_check(15, 29, 100, 7, 0.1, 1) #check for (b)

hypothesis_check(15, 29, 100, 30, 0.15, 0)
hypothesis_check(15, 29, 100, 30, 0.10, 0)
hypothesis_check(15, 29, 100, 30, 0.05, 0)
hypothesis_check(15, 29, 100, 30, 0.01, 0)






