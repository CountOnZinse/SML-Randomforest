
# Test-script #

install.packages("pacman")
pacman::p_load(tidyverse, randomForest)


p <- 40
n <- 100000

min_cor <- -0.1
max_cor <- 0.5

# generate x values
x <- paste0(rep("x", p), 0:(p-1))

x_val <- matrix(data = 0, 
                nrow = n,
                ncol = p)

x_val[, 1] <- 1

# different distributions for x 
dist_disc <- sample(c("norm", "ordinal", "beta",  "uni"),
                    p,
                    replace = T,
                    prob = c(0.6, 0.2, 0.15, 0.05))

# generating x by drawing out of different distributions
for(i in 2:p){
  
  if(dist_disc[i] == "norm"){
    
    mu <- rnorm(1, 0, 2)
    sig <- sample(seq(0.5, 10, 0.1), 1)
    
    x_val[, i] <- rnorm(n, mu, sig)
    
  }else if(dist_disc[i] == "ordinal"){
    
    x_val[, i] <- sample(1:10, n, replace = T)
    
  } else if(dist_disc[i] == "beta"){
    
    alpha <- sample(seq(0.5, 5, 0.1), 1) 
    beta <- sample(seq(0.5, 5, 0.1), 1) 
    
    x_val[, i] <- rbeta(n, alpha, beta)
    
  }else{
    
    min_u <- sample(seq(0, 0.5, 0.01), 1)
    max_u <- sample(seq(0.51, 1, 0.01), 1)
    
    x_val[, i] <- runif(n, min_u, max_u)
    
  }
  
}

# generating correlations in-between the id variables

cor_var <- sample(seq(min_cor, max_cor, 0.05), p, replace = T)

idx_var <- vector("numeric", p)

for(i in 2:p){
  
  idx_var[i] <- sample(c(1:p)[-i], 1)
  
}

for(i in 2:p){
  
  idx <- sample(1:n, abs(n*cor_var[i]))
  
  if(cor_var[i] > 0){
    
    x_val[idx, i] <- 1.351*x_val[idx, idx_var[i]]
    
  }else{
    
    x_val[idx, i] <- -1.351*x_val[idx, idx_var[i]]
    
  }
  
}

beta <- sample(seq(-1, 3, 0.1), p, replace = T)

epsi <- rnorm(n)

y <- x_val %*% as.vector(beta) + epsi

y <- ifelse(y >= min(y) & y < -2, "Tanne",
            ifelse(y >= -2 & y < 0, "Kiefer",
                   ifelse(y >= 0 & y < 1, "Rotbuche",
                          ifelse(y >= 1 & y < 2, "Eiche", "Esche"))))

beta <- sample(seq(-1, 2, 0.1), p, replace = T)

epsi <- rnorm(n)

y_sick <- x_val %*% as.vector(beta) + epsi

y_sick <- ifelse(y_sick >= min(y_sick) & y_sick < 0, "sick", "non-sick")

out <- cbind.data.frame(y, 
                        y_sick,
                        x_val[, -1])

out

table(y)

gen_dataset <- function(p, n, min_cor, max_cor){
  
  # generate x values
  x <- paste0(rep("x", p), 0:(p-1))
  
  x_val <- matrix(data = 0, 
                  nrow = n,
                  ncol = p)
  
  x_val[, 1] <- 1
  
  # different distributions for x 
  dist_disc <- sample(c("norm", "ordinal", "beta",  "uni"),
                      p,
                      replace = T,
                      prob = c(0.6, 0.2, 0.15, 0.05))
  
  # generating x by drawing out of different distributions
  for(i in 2:p){
    
    if(dist_disc[i] == "norm"){
      
      mu <- rnorm(1, 0, 2)
      sig <- sample(seq(0.5, 10, 0.1), 1)
      
      x_val[, i] <- rnorm(n, mu, sig)
      
    }else if(dist_disc[i] == "ordinal"){
      
      x_val[, i] <- sample(1:10, n, replace = T)
      
    } else if(dist_disc[i] == "beta"){
      
      alpha <- sample(seq(0.5, 5, 0.1), 1) 
      beta <- sample(seq(0.5, 5, 0.1), 1) 
      
      x_val[, i] <- rbeta(n, alpha, beta)
      
    }else{
      
      min_u <- sample(seq(0, 0.5, 0.01), 1)
      max_u <- sample(seq(0.51, 1, 0.01), 1)
      
      x_val[, i] <- runif(n, min_u, max_u)
      
    }
    
  }
  
  # generating correlations in-between the id variables
  
  cor_var <- sample(seq(min_cor, max_cor, 0.05), p, replace = T)
  
  idx_var <- vector("numeric", p)
  
  for(i in 2:p){
    
    idx_var[i] <- sample(c(1:p)[-i], 1)
    
  }
  
  for(i in 2:p){
    
    idx <- sample(1:n, abs(n*cor_var[i]))
    
    if(cor_var[i] > 0){
      
      x_val[idx, i] <- 1.351*x_val[idx, idx_var[i]]
      
    }else{
      
      x_val[idx, i] <- -1.351*x_val[idx, idx_var[i]]
      
    }
    
  }
  
  beta <- sample(seq(-1, 3, 0.1), p, replace = T)
  
  epsi <- rnorm(n)
  
  y <- x_val %*% as.vector(beta) + epsi
  
  y <- ifelse(y >= min(y) & y < -2, "Tanne",
              ifelse(y >= -2 & y < 0, "Kiefer",
                     ifelse(y >= 0 & y < 1, "Rotbuche",
                            ifelse(y >= 1 & y < 2, "Eiche", "Esche"))))
  
  beta <- sample(seq(-1, 2, 0.1), p, replace = T)
  
  epsi <- rnorm(n)
  
  y_sick <- x_val %*% as.vector(beta) + epsi
  
  y_sick <- ifelse(y_sick >= min(y_sick) & y_sick < 0, "sick", "non-sick")
  
  out <- cbind.data.frame(y, 
                          y_sick,
                          x_val[, -1])
  
  out
  
}

# create dataset

pop <- gen_dataset(p = 40, n = 100000,
                   min_cor = -0.3,
                   max_cor = 0.5)

cor(pop[, c(-1, -2)])

str(pop)

table(pop$y)
table(pop$y_sick)

# Sampling the data

sample_data <- pop[sample(1:nrow(pop), 10000), ]

table(sample_data$y)
table(sample_data$y_sick)

# Random Forest

randomForest()


