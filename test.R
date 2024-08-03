
# Test-script #

# load the basic packages
if (!require("pacman")) {
  install.packages("pacman")
} 
if (!require("hardhat")) {
  install.packages("hardhat")
}

# load the usefull packages
pacman::p_load(tidyverse, hardhat, purrr, MLmetrics, foreach, doParallel, tibble,
               paletteer)

# load the packages with randomforest
pacman::p_load(randomForest, randomForestSRC, ranger,
               Rborist)


# start of testing 
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

# end of testing and building function

######################### Generating data function #############################

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

####################### Doing the actual work ##################################

############################# Classification ###################################

# ---- binomial Analysis ---- 

set.seed(1234)

pop <- gen_dataset(p = 44, n = 100000,
                   min_cor = -0.3,
                   max_cor = 0.5)

cor(pop[, c(-1, -2)])

str(pop)

table(pop$y_sick)
table(pop$y)

# Sampling the data

sample_data <- pop[sample(1:nrow(pop), 20000), ]

table(sample_data$y_sick)

idx <- sample(1:nrow(sample_data), 0.8*20000)

train <- sample_data[idx, ]
test <- sample_data[-idx, ]

table(train$y_sick)
table(test$y_sick)

# Random Forest

rf_model <- randomForest(y_sick ~ .,
                         data = train[, -1],
                         type = "classification",
                         ntree = 200)

plot(getTree(rf_model, 3))

y_pred <- predict(rf_model, newdata = test[, c(-1, -2)])

table(y_pred)

# Compute the accuracy
acc <- cbind.data.frame(test$y_sick, y_pred)
table(acc)

sum(diag(table(acc)))/sum(table(acc))

# Compute F1 Score
F1_Score(y_true = acc$`test$y_sick`,
         y_pred = acc$y_pred)

# Compute Fbeta Score
FBeta_Score(y_true = acc$`test$y_sick`,
            y_pred = acc$y_pred,
            beta = 0.5)







# Random Forest SRC

rfsrc_model <- randomForestSRC::rfsrc(y_sick ~ .,
                                   data = train[, c(-1)],
                                   type = "classification",
                                   ntree = 200)


# ---- multinomial Analysis ----

set.seed(1234)

# create dataset

pop <- gen_dataset(p = 40, n = 100000,
                   min_cor = -0.3,
                   max_cor = 0.5)

cor(pop[, c(-1, -2)])

str(pop)

table(pop$y)

# Sampling the data

sample_data <- pop[sample(1:nrow(pop), 20000), ]

table(sample_data$y)
table(sample_data$y_sick)

idx <- sample(1:nrow(sample_data), 0.8*20000)

train <- sample_data[idx, ]
test <- sample_data[-idx, ]

table(train$y)
table(test$y)

# Random Forest

rf_model <- randomForest(y ~ .,
                         data = train[, -2],
                         type = "classification",
                         ntree = 200)

y_pred <- predict(rf_model, newdata = test[, c(-1, -2)])

table(y_pred)

# Compute the accuracy
acc <- cbind.data.frame(test$y, y_pred)
table(acc)

sum(diag(table(acc)))/sum(table(acc))

# compute the F1 Score
F1_Score(y_true = acc$`test$y`,
         y_pred = acc$y_pred)

# Compute Fbeta Score
FBeta_Score(y_true = acc$`test$y`,
            y_pred = acc$y_pred,
            beta = 1)


# ---- classification with different packages ----

obj_rf <- randomForest(y ~ .,
                       data = train[, -2],
                       type = "classification",
                       ntree = 200,
                       mtry = 4)

out <- predict(obj_rf, data = test[, c(-1, -2)])

obj_rngr <- ranger(y ~ .,
                   data = train[, -2],
                   num.trees = 200,
                   mtry = 4) 

out <- predict(obj_rngr, data = test[, c(-1, -2)])$predictions

out

# caret is out 

#obj_crt <- caret::train(method = "rf",
#                        y ~ .,
#                        data = train[, -2],
#                        ntree = 200,
#                        tuneGrid = NULL)
#
#predict(obj_crt, newdata = test[, c(-1, -2)])

obj_bor <- Rborist(y = train[, 1],
                   train[, c(-1, -2)],
                   nTree = 200,
                   maxLeaf = 4)

out <- predict(obj_bor, newdata = test[, c(-1, -2)])$yPred

out

# ---- Cross-Validation ----

cv_rf <- function(train_data, test_data, y, mtry, ntree,
                  replace = NULL, formula = NULL, f_beta = 0.5){
  
  # checking for string in formula
  if(is.character(formula)==T){
    
  }else{
    print("Formula needs to be written as character")
    break
  }
  
  # matrix for the acc, f_one and f_beta
  gof_out <- matrix(0, nrow = 3, ncol = 4)
  
  # Function randomForest
  time_rf <- Sys.time()
  
  obj_rf <- randomForest(eval(parse(text = formula)),
                         data = train_data,
                         type = "classification",
                         ntree = ntree,
                         mtry = mtry)
  
  # time
  gof_out[1, 4] <- Sys.time() - time_rf
  
  y_pred <- predict(obj_rf, newdata = test_data)
  
  table(y_pred)
  
  # Compute the accuracy
  acc <- cbind.data.frame(y, y_pred)
  table(acc)
  
  gof_out[1, 1] <- sum(diag(table(acc)))/sum(table(acc))
  
  # compute the F1 Score
  gof_out[1, 2] <- F1_Score(y_true = acc$y,
                            y_pred = acc$y_pred)
  
  # Compute Fbeta Score
  gof_out[1, 3] <- FBeta_Score(y_true = acc$y,
                               y_pred = acc$y_pred,
                               beta = f_beta)
  
  # Function ranger
  time_rngr <- Sys.time()
  
  obj_rngr <- ranger(eval(parse(text = formula)),
                     data = train_data,
                     num.trees = ntree,
                     mtry = mtry)
  
  # time
  gof_out[2, 4] <- Sys.time() - time_rngr
  
  y_pred <- predict(obj_rngr, data = test_data)$predictions
  
  table(y_pred)
  
  # Compute the accuracy
  acc <- cbind.data.frame(y, y_pred)
  table(acc)
  
  gof_out[2, 1] <- sum(diag(table(acc)))/sum(table(acc))
  
  # compute the F1 Score
  gof_out[2, 2] <- F1_Score(y_true = acc$y,
                            y_pred = acc$y_pred)
  
  # Compute Fbeta Score
  gof_out[2, 3] <- FBeta_Score(y_true = acc$y,
                               y_pred = acc$y_pred,
                               beta = f_beta)
  
  # Function caret
  time_bor <- Sys.time()
  
  obj_bor <- Rborist(y = train_data[, 1],
                     train_data[, c(-1)],
                     nTree = ntree,
                     mtry = mtry)
  
  # time
  gof_out[3, 4] <- Sys.time() - time_bor
  
  y_pred <- predict(obj_bor, newdata = test_data)$yPred
  
  table(y_pred)
  
  # Compute the accuracy
  acc <- cbind.data.frame(y, y_pred)
  table(acc)
  
  gof_out[3, 1] <- sum(diag(table(acc)))/sum(table(acc))
  
  # compute the F1 Score
  gof_out[3, 2] <- F1_Score(y_true = acc$y,
                            y_pred = acc$y_pred)
  
  # Compute Fbeta Score
  gof_out[3, 3] <- FBeta_Score(y_true = acc$y,
                               y_pred = acc$y_pred,
                               beta = f_beta)
  
  
  gof_out
  
}

# this function provides the cores for later usage in the foreach loop so 
# we do it parallel

# adjust the cores here - it depends on how many cores you have 
# you should use at least 2 cores
registerDoParallel(detectCores()-2)

n_tree <- seq(100, 500, 100)
mtry <- 2:8

# grid with all the hyperparameters
grid_hp <- expand.grid(n_tree, mtry)

out_fe <- foreach(i = 1:nrow(grid_hp),
                  .multicombine = T, # combine the results efficiently
                  .combine = "list", # way of binding
                  .packages = c("randomForest", "ranger", "caret", "MLmetrics", "Rborist")) %dopar% { 
                    # load the packages for the function, otherwise error
                    cv_rf(train_data = train[, -2], y = test$y, test_data = test[, c(-1, -2)],
                          ntree = grid_hp[i, 1], mtry = grid_hp[i, 2], 
                          replace = NULL, formula = "y ~ .")
                  }

registerDoParallel(1) # reset the cores

out_fe





