# distribution of dependent variable in sample, train and test sets
# simulated data
par(mfrow = c(1,3))
barplot(table(sample_data$y))
barplot(table(train$y))
barplot(table(test$y))
par(mfrow = c(1,1))

#empirical data
par(mfrow = c(1,3))
barplot(table(sample_cov$Cover_Type))
barplot(table(train_cov$Cover_Type))
barplot(table(test_cov$Cover_Type))
par(mfrow = c(1,1))



#### correlation ####
install.packages("corrplot")

library(corrplot)
str(cov)

#### only for correlation plot ####
pop$y <- as.numeric(pop$y)
pop$y_sick <- as.numeric(pop$y_sick)

cov$Cover_Type <- as.numeric(cov$Cover_Type)

# install.packages("corrplot")
library(corrplot)

# labels for colors
label_colors_cov <- rep("red", ncol(cov))
label_colors_cov[55] <- "blue"

label_colors_pop <- rep("red", ncol(pop))
label_colors_pop[which(names(pop) == "y")] <- "blue"

par(mfrow = c(1,2))

# empirical correlationmatrix
corrplot(cor(cov), method = "color", tl.cex = 0.3, tl.col = label_colors_cov, title = "empirical correlationmatrix")

# simulated correlationmatrix
corrplot(cor(pop), method = "color", tl.cex = 0.3, tl.col = label_colors_pop, title = "simulated correlationmatrix")

par(mfrow = c(1,1))



# do rf ####
############# Training RandomForest model on SIMULATED data #################
set.seed(1234)
rf_sim <- randomForest(y ~ ., data = train)

# Predict on test set
pred_sim <- predict(rf_sim, newdata = test)

# Confusion matrix and Accuracy
confusion_sim <- confusionMatrix(pred_sim, test$y)
accuracy_sim <- confusion_sim$overall['Accuracy']

# F1 and Fbeta score
f1_sim <- F1_Score(pred_sim, test$y, positive = NULL)  
fbeta_sim <- FBeta_Score(pred_sim, test$y, positive = NULL, beta = 0.5) 

# Display results
print(confusion_sim)
cat("F1 Score:", f1_sim, "\n")
cat("Fbeta Score:", fbeta_sim, "\n")

########### rf model on simulated data without "y$sick" ######################
rf_sim <- randomForest(y ~ ., data = train[,-2])

# Predict on test set
pred_sim <- predict(rf_sim, newdata = test[,-2])

# Confusion matrix and Accuracy
confusion_sim <- confusionMatrix(pred_sim, test$y)
accuracy_sim <- confusion_sim$overall['Accuracy']

# F1 and Fbeta score
f1_sim <- F1_Score(pred_sim, test$y, positive = NULL)
fbeta_sim <- FBeta_Score(pred_sim, test$y, positive = NULL, beta = 0.5)

# Display results
print(confusion_sim)
cat("F1 Score:", f1_sim, "\n")
cat("Fbeta Score:", fbeta_sim, "\n")
###########

########## Training RandomForest model on EMPIRICAL data ##############
set.seed(1234)
rf_cov <- randomForest(Cover_Type ~ ., data = train_cov)

# Predict on test set
pred_cov <- predict(rf_cov, newdata = test_cov)

# Confusion matrix and Accuracy
confusion_cov <- confusionMatrix(pred_cov, test_cov$Cover_Type)
accuracy_cov <- confusion_cov$overall['Accuracy']

# F1 and Fbeta score
f1_cov <- F1_Score(pred_cov, test_cov$Cover_Type, positive = NULL)
fbeta_cov <- FBeta_Score(pred_cov, test_cov$Cover_Type, positive = NULL, beta = 0.5)

# Display RESULTS #####
print(confusion_cov)
cat("F1 Score:", f1_cov, "\n")
cat("Fbeta Score:", fbeta_cov, "\n")

