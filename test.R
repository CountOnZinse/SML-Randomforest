rm(list = ls())
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
               paletteer, caret)

# load the packages with randomforest
pacman::p_load(randomForest, randomForestSRC, ranger,
               party)


# load data from V1
load("data/V1GoF_Workspace.RData")
source("own_functions.R")

####################### Doing the actual work ##################################

############################# Classification ###################################


set.seed(1234)

n <- 100000

# create dataset

pop <- gen_dataset(p = 54, n = n,
                   min_cor = -0.3,
                   max_cor = 0.5)

cor(pop[, c(-1, -2)])

str(pop)

table(pop$y)

# Sampling the simulated data

sample_data <- pop[sample(1:nrow(pop), n), ]

table(sample_data$y)
table(sample_data$y_sick)

idx <- sample(1:nrow(sample_data), 0.8*n)

train <- sample_data[idx, ]
test <- sample_data[-idx, ]

table(train$y)
table(test$y)

#### load empirical data ####
cov <- as.data.frame(read.csv("covtype.csv", stringsAsFactors = FALSE))

str(cov)
summary(cov)

# recode dependent variable (Cover_Type) as factor and assign labels
cov$Cover_Type <- factor(cov$Cover_Type,
                         levels = 1:7,
                         labels = c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                                    "Cottonwood/Willow", "Aspen", "Douglas-fir", 
                                    "Krummholz"))
cov$Cover_Type <- as.factor(cov$Cover_Type)
table(cov$Cover_Type)

# check for NA's: none
sapply(cov ,function(x)any(is.na(x)))

# sampling the empirical data 
set.seed(1234)

sample_cov <- cov[sample(1:nrow(cov), n), ]

table(sample_cov$Cover_Type)

idx_cov <- sample(1:nrow(sample_cov), 0.8*n)

train_cov <- sample_cov[idx_cov, ]
test_cov <- sample_cov[-idx_cov, ]

table(train_cov$Cover_Type)
table(test_cov$Cover_Type)

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
                     mtry = mtry,
                     num.threads = 1)
  
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
  
  obj_crf <- cforest(formula = eval(parse(text = formula)),
                     data = train_data, 
                     controls = cforest_unbiased(ntree = ntree, 
                                                 mtry = mtry))
  
  # time
  gof_out[3, 4] <- Sys.time() - time_bor
  
  y_pred <- predict(obj_crf, newdata = test_data)
  
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

n_tree <- seq(300, 700, 100)
mtry <- 4:10

# grid with all the hyperparameters
grid_hp <- expand.grid(n_tree, mtry)


#### simulated data ####
out_fe <- foreach(i = 1:nrow(grid_hp),
                  .multicombine = T, # combine the results efficiently
                  .combine = "list", # way of binding
                  .packages = c("randomForest", "ranger", "MLmetrics", "party")) %dopar% { 
                    # load the packages for the function, otherwise error
                    cv_rf(train_data = train[, -2], y = test$y, test_data = test[, c(-1, -2)],
                          ntree = grid_hp[i, 1], mtry = grid_hp[i, 2], 
                          replace = NULL, formula = "y ~ .")
                  }

registerDoParallel(1) # reset the cores

out_fe

# ---- transform the data ----

gof_data <- list()

# Accuracy
gof_data[[1]] <- lapply(out_fe, function(x) x[, 1]) %>% 
  unlist() %>% 
  cbind() %>% 
  data.frame(value = .,
             Package = rep(c("RandomForest", "Ranger", "Party"), 
                           length(out_fe)),
             Iteration = rep(1:length(out_fe), each = 3))

names(gof_data[[1]])[1] <- "value" 


# F1-Score
gof_data[[2]] <- lapply(out_fe, function(x) x[, 2]) %>% 
  unlist() %>% 
  cbind() %>% 
  data.frame(value = .,
             Package = rep(c("RandomForest", "Ranger", "Party"), 
                           length(out_fe)),
             Iteration = rep(1:length(out_fe), each = 3))

names(gof_data[[2]])[1] <- "value" 


# FBeta-Score
gof_data[[3]] <- lapply(out_fe, function(x) x[, 3]) %>% 
  unlist() %>% 
  cbind() %>% 
  data.frame(value = ., Package = rep(c("RandomForest", "Ranger", "Party"), 
                                      length(out_fe)),
             Iteration = rep(1:length(out_fe), each = 3))

names(gof_data[[3]])[1] <- "value" 

# Time
gof_data[[4]] <- lapply(out_fe, function(x) x[, 4]) %>% 
  unlist() %>% 
  cbind() %>% 
  data.frame(value = ., Package = rep(c("RandomForest", "Ranger", "Party"), 
                                      length(out_fe)),
             Iteration = rep(1:length(out_fe), each = 3))

names(gof_data[[4]])[1] <- "value" 

# ---- plot it ----

gof_plot <- list()

gof_name <- c("Accuracy", "F1 Score", "F-beta Score", "Time")

for (i in 1:length(gof_data)){
  
  gof_plot[[i]] <- ggplot(data = gof_data[[i]],
                          aes(x = Iteration,
                              y = value,
                              group = Package,
                              col = Package)) +
    geom_point() +
    scale_color_manual(values = c("RandomForest" = "green",
                                  "Ranger" = "red", 
                                  "Party" = "blue")) +
    labs(title = gof_name[i],
         subtitle = "Changing the parameters ntree and mtry at each step",
         x = "Step",
         y = "Value") 
  
    ggsave(paste0("pics/", "GoF_", gof_name[i], ".png"),
         gof_plot[[i]],
         device = "png")
  
}

gof_plot[3]


#### same as in test.R but for empirical data ####
registerDoParallel(detectCores()-2)

n_tree <- seq(300, 700, 100)
mtry <- 4:10

# grid with all the hyperparameters
grid_hp <- expand.grid(n_tree, mtry)

out_fe_cov <- foreach(i = 1:nrow(grid_hp),
                      .multicombine = T, # combine the results efficiently
                      .combine = "list", # way of binding
                      .packages = c("randomForest", "ranger", "MLmetrics", "party")) %dopar% { 
                        # load the packages for the function, otherwise error
                        cv_rf(train_data = train_cov, y = test_cov$Cover_Type, test_data = test_cov[,  -55],
                              ntree = grid_hp[i, 1], mtry = grid_hp[i, 2], 
                              replace = NULL, formula = "Cover_Type ~ .")
                      }

registerDoParallel(1) # reset the cores

out_fe_cov


# ---- transform the data ----

gof_data_cov <- list()

# Accuracy
gof_data_cov[[1]] <- lapply(out_fe_cov, function(x) x[, 1]) %>% 
  unlist() %>% 
  cbind() %>% 
  data.frame(value = .,
             Package = rep(c("RandomForest", "Ranger", "Party"), 
                           length(out_fe_cov)),
             Iteration = rep(1:length(out_fe_cov), each = 3))

names(gof_data_cov[[1]])[1] <- "value" 


# F1-Score
gof_data_cov[[2]] <- lapply(out_fe_cov, function(x) x[, 2]) %>% 
  unlist() %>% 
  cbind() %>% 
  data.frame(value = .,
             Package = rep(c("RandomForest", "Ranger", "Party"), 
                           length(out_fe_cov)),
             Iteration = rep(1:length(out_fe_cov), each = 3))

names(gof_data_cov[[2]])[1] <- "value" 


# FBeta-Score
gof_data_cov[[3]] <- lapply(out_fe_cov, function(x) x[, 3]) %>% 
  unlist() %>% 
  cbind() %>% 
  data.frame(value = ., Package = rep(c("RandomForest", "Ranger", "Party"), 
                                      length(out_fe_cov)),
             Iteration = rep(1:length(out_fe_cov), each = 3))

names(gof_data_cov[[3]])[1] <- "value" 

# Time
gof_data_cov[[4]] <- lapply(out_fe_cov, function(x) x[, 4]) %>% 
  unlist() %>% 
  cbind() %>% 
  data.frame(value = ., Package = rep(c("RandomForest", "Ranger", "Party"), 
                                      length(out_fe_cov)),
             Iteration = rep(1:length(out_fe_cov), each = 3))

names(gof_data_cov[[4]])[1] <- "value" 

# ---- plot it ----

gof_plot_cov <- list()

gof_name <- c("Accuracy", "F1 Score", "F-beta Score", "Time")

for (i in 1:length(gof_data_cov)){
  
  gof_plot_cov[[i]] <- ggplot(data = gof_data_cov[[i]],
                              aes(x = Iteration,
                                  y = value,
                                  group = Package,
                                  col = Package)) +
    geom_point() +
    scale_color_manual(values = c("RandomForest" = "green",
                                  "Ranger" = "red", 
                                  "Party" = "blue")) +
    labs(title = gof_name[i],
         subtitle = "Changing the parameters ntree and mtry at each step",
         x = "Step",
         y = "Value") 
  
    ggsave(paste0("pics/", "GoF_", gof_name_cov[i], ".png"),
         gof_plot_cov[[i]],
         device = "png")
  
}

gof_plot_cov[3]


#### some descriptives ####

# empirical data
str(cov)
summary(cov)

# check dependent variable
table(cov$Cover_Type)
# classes are imbalanced, but reasonable number of obs. for each category

# simulated data
str(pop)
summary(pop)

# check dependent variable
table(pop$y)


# check distribution of dependent variables in simulated and empirical data
par(mfrow = c(1, 2)) # Set up plotting area for two plots

barplot(table(sample_data$y),
        main = "Simulated data: dependent variable",
        xlab = "Tree types",
        ylab = "",
        col = "blue",
        ylim = c(0, 50000),
        las = 2,              # Make x-axis labels perpendicular
        cex.names = 0.8,      # Adjust size of x-axis labels
        mgp = c(3, 1, 0))  


barplot(table(sample_cov$Cover_Type),
        main = "Empirical data: dependent variable",
        xlab = "Cover Type",
        ylab = "",
        col = "yellow",
        ylim = c(0, 50000),
        las = 2,
        cex.names = 0.8,
        mgp = c(3, 1, 0))   

par(mfrow = c(1, 1)) # Set back plotting area