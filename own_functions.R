# own functions #


# ---- generate data ----

gen_dataset <- function(p, n, min_cor, max_cor, reg = F){
  
  # generate x values
  x <- paste0(rep("x", p), 0:(p-1))
  
  x_val <- matrix(data = 0, 
                  nrow = n,
                  ncol = p)
  
  x_val[, 1] <- 1
  
  # different distributions for x 
  dist_disc <- sample(c("norm", "beta",  "uni"),
                      p,
                      replace = T,
                      prob = c(0.6, 0.25, 0.15))
  
  # generating x by drawing out of different distributions
  for(i in 2:p){
    
    if(dist_disc[i] == "norm"){
      
      mu <- rnorm(1, 0, 2)
      sig <- sample(seq(0.5, 10, 0.1), 1)
      
      x_val[, i] <- rnorm(n, mu, sig)
      
    }else if(dist_disc[i] == "beta"){
      
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
    
    # for positive correlation
    if(cor_var[i] > 0){
      
      x_val[idx, i] <- 1.351*x_val[idx, idx_var[i]]
      
      # for negative correlation   
    }else{
      
      x_val[idx, i] <- -1.351*x_val[idx, idx_var[i]]
      
    }
    
  }
  
  # beta weights for the regression model
  beta <- sample(seq(-1, 2, 0.1), p, replace = T)
  
  # epsilon
  epsi <- rnorm(n)
  
  # calculate y
  y <- x_val %*% as.vector(beta) + epsi
  
  # generating multinomial y
  y <- ifelse(y >= min(y) & y < -5, "Tanne",
              ifelse(y >= -5 & y < -1, "Esche",
                     ifelse(y >= -1 & y < 5, "Rotbuche",
                            ifelse(y >= 5 & y < 20, "Eiche", 
                                   ifelse(y >= 20 & y < 23, "Douglasie",
                                          ifelse(y >= 23 & y < 37, "Buche", "Kiefer"))))))
  
  # beta weights 
  beta <- sample(seq(-1, 2, 0.1), p, replace = T)
  
  # epsilon
  epsi <- rnorm(n)
  
  # calculate y
  y_sick <- x_val %*% as.vector(beta) + epsi
  
  # generating binomial y
  y_sick <- ifelse(y_sick >= min(y_sick) & y_sick < 0, "sick", "non-sick")
  
  out <- cbind.data.frame(as.factor(y), 
                          as.factor(y_sick),
                          x_val[, -1])
  
  # rename 
  colnames(out) <- c("y", "y_sick", paste0("x", 2:p)) 
  
  out
  
}


# ---- Cross-Validation Algo ----

