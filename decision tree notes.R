######### TO PREDICT DT type = 'prob'. type = "class"

#install.packages("rpart")
#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
bankdata <- read.csv("bank-sample.csv", header=TRUE)
head(bankdata)
head(bankdata[,2:8])
head(bankdata[,c(9,16,17)])

#### Some SUMMARIES ###
table(bankdata$job)
table(bankdata$marital)
table(bankdata$education)
table(bankdata$default)
table(bankdata$housing)
table(bankdata$loan)
table(bankdata$contact)
table(bankdata$poutcome)
#  entropy is a measure of unpredictability
#---------------------Decision tree building-----------------------
fit <- rpart(subscribed ~job + marital + education+default + 
               housing + loan + contact+poutcome,
             method="class",
             data=bankdata,
             control=rpart.control(minsplit=1),
             parms=list(split='information')
)

# there are few other arguments instead of minsplit in rpart.control: cp, maxdepth;
# smaller values of cp correspond to decision trees of larger sizes, 
# and hence more complex decision surfaces.
# argument "maxdepth" also could be used.

# method = "anova", "poisson", "class" or "exp"
# If response is a survival object, then method = "exp" is assumed, 
# if response has 2 columns then method = "poisson" is assumed, 
# if response is a factor then method = "class" is assumed, 
# otherwise method = "anova" is assumed
# split = 'information' or 'gini'
rpart.plot(fit, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE)
#-----------------STEP 1: finding the best depth for best accuracy (no n fold)--------
library(rpart)

# Read in the bank data
bank_data <- read.csv("bank-sample.csv")

# Convert categorical variables to factors and ensure consistent levels
bank_data$subscribed <- factor(bank_data$subscribed, levels = c("no", "yes"))
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$education <- as.factor(bank_data$education)
bank_data$default <- as.factor(bank_data$default)
bank_data$housing <- as.factor(bank_data$housing)
bank_data$loan <- as.factor(bank_data$loan)
bank_data$contact <- as.factor(bank_data$contact)
bank_data$poutcome <- as.factor(bank_data$poutcome)

# Define the features used for training
features <- c("job", "marital", "education", "default", "housing", 
              "loan", "contact", "poutcome")

# Split the data into 80% training and 20% testing
set.seed(123)
train_indices <- sample(1:nrow(bank_data), size = floor(0.8 * nrow(bank_data)), replace = FALSE)
train_data <- bank_data[train_indices, ]
test_data <- bank_data[-train_indices, ]

# Initialize a list to store accuracy for each depth
depth_accuracies <- numeric(5)  # One slot for each depth (1-5)

# Loop through different max depths
for (depth in 1:5) {
  
  # Train the model using the selected input features and current max depth
  model <- rpart(subscribed ~ job + marital + education + default + housing + 
                   loan + contact + poutcome, 
                 data = train_data, 
                 method = "class",
                 control = rpart.control(maxdepth = depth))
  
  # Predict on the test set using only the feature columns
  predictions <- predict(model, test_data[, features], type = "class")
  
  # Ensure the levels of predictions are the same as the actual data
  predictions <- factor(predictions, levels = levels(test_data$subscribed))
  
  # Full Confusion Matrix (Actual values first, Predicted values second)
  conf_matrix <- table(Actual = test_data$subscribed, Predicted = predictions)
  
  # Extract TP, TN, FP, and FN from the confusion matrix using numeric indices
  TP <- conf_matrix[2, 2]  # True Positives (Predicted yes, Actual yes)
  FN <- conf_matrix[2, 1]  # False Negatives (Predicted no, Actual yes)
  FP <- conf_matrix[1, 2]  # False Positives (Predicted yes, Actual no)
  TN <- conf_matrix[1, 1]  # True Negatives (Predicted no, Actual no)
  
  # Print the results for each depth
  cat("Depth:", depth, "\n")
  cat("True Positives (TP):", TP, "\n")
  cat("False Negatives (FN):", FN, "\n")
  cat("False Positives (FP):", FP, "\n")
  cat("True Negatives (TN):", TN, "\n\n")
  
  # Calculate Accuracy for this depth
  fold_accuracy <- (TP + TN) / (TP + TN + FP + FN)
  
  # Store the accuracy for this depth
  depth_accuracies[depth] <- fold_accuracy
}

# Print the accuracies for each depth
print(depth_accuracies)

# Identify the best max depth (highest accuracy)
best_depth_accuracy <- which.max(depth_accuracies)
cat("Best max depth (highest accuracy): ", best_depth_accuracy, "\n")
cat("Highest accuracy: ", max(depth_accuracies), "\n")
#---------------- STEP 1: N cross validation to find the best depth for best accuracy-----------
library(rpart)

# Read in the bank data
bank_data <- read.csv("bank-sample.csv")

# Convert categorical variables to factors and ensure consistent levels
bank_data$subscribed <- factor(bank_data$subscribed, levels = c("no", "yes"))
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$education <- as.factor(bank_data$education)
bank_data$default <- as.factor(bank_data$default)
bank_data$housing <- as.factor(bank_data$housing)
bank_data$loan <- as.factor(bank_data$loan)
bank_data$contact <- as.factor(bank_data$contact)
bank_data$poutcome <- as.factor(bank_data$poutcome)

# Define the features used for training (excluding other columns like "age", "balance", etc.)
features <- c("job", "marital", "education", "default", "housing", 
              "loan", "contact", "poutcome")

# Set the number of folds for cross-validation
n_folds <- 5

# Randomly shuffle the data
set.seed(123)
shuffled_data <- bank_data[sample(nrow(bank_data)), ]

# Create fold assignments using sample() to assign each row to a fold
folds <- sample(rep(1:n_folds, length.out = nrow(shuffled_data)))

# Initialize a list to store average accuracy for each depth
depth_accuracies <- numeric(5)  # One slot for each depth (1-5)

# Loop through different max depths
for (depth in 1:5) {
  
  # Initialize an accuracy list for each fold (since n_folds = 5)
  accuracy_list <- numeric(n_folds)
  
  # Cross-validation loop
  for(i in 1:n_folds) {
    # Create training and testing sets for each fold
    test_indices <- which(folds == i)
    test_data <- shuffled_data[test_indices, ]
    train_data <- shuffled_data[-test_indices, ]
    
    # Train the model using the selected input features and current max depth
    model <- rpart(subscribed ~ job + marital + education + default + housing + 
                     loan + contact + poutcome, 
                   data = train_data, 
                   method = "class",
                   control = rpart.control(maxdepth = depth))
    
    # Predict on the test set using only the feature columns
    predictions <- predict(model, test_data[, features], type = "class")
    
    # Ensure the levels of predictions are the same as the actual data
    predictions <- factor(predictions, levels = levels(test_data$subscribed))
    
    # Full Confusion Matrix (Actual values first, Predicted values second)
    conf_matrix <- table(Actual = test_data$subscribed, Predicted = predictions)
    
    # Extract TP, TN, FP, and FN from the confusion matrix using numeric indices
    TP <- conf_matrix[2, 2]  # True Positives (Predicted yes, Actual yes)
    FN <- conf_matrix[2, 1]  # False Negatives (Predicted no, Actual yes)
    FP <- conf_matrix[1, 2]  # False Positives (Predicted yes, Actual no)
    TN <- conf_matrix[1, 1]  # True Negatives (Predicted no, Actual no)
    
    # Print the results for each fold
    cat("Fold:", i, "\n")
    cat("True Positives (TP):", TP, "\n")
    cat("False Negatives (FN):", FN, "\n")
    cat("False Positives (FP):", FP, "\n")
    cat("True Negatives (TN):", TN, "\n\n")
    
    # Calculate Accuracy for this fold
    fold_accuracy <- (TP + TN) / (TP + TN + FP + FN)
    
    # Store the accuracy for this fold in the pre-allocated list
    accuracy_list[i] <- fold_accuracy
  }
  
  # Calculate and store the average accuracy for this max depth
  depth_accuracies[depth] <- mean(accuracy_list)
}

# Print the accuracies for each max depth
print(depth_accuracies)

# Identify the best max depth (highest accuracy)
best_depth_accuracy <- which.max(depth_accuracies)
cat("Best max depth (highest accuracy): ", best_depth_accuracy, "\n")
cat("Highest accuracy: ", max(depth_accuracies), "\n")

#-----------------N cross validation to find the best depth for best tyoe 2 error-----------------------
library(rpart)

# Read in the bank data
bank_data <- read.csv("bank-sample.csv")

# Convert categorical variables to factors and ensure consistent levels
bank_data$subscribed <- factor(bank_data$subscribed, levels = c("no", "yes"))
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$education <- as.factor(bank_data$education)
bank_data$default <- as.factor(bank_data$default)
bank_data$housing <- as.factor(bank_data$housing)
bank_data$loan <- as.factor(bank_data$loan)
bank_data$contact <- as.factor(bank_data$contact)
bank_data$poutcome <- as.factor(bank_data$poutcome)

# Define the features used for training (excluding other columns like "age", "balance", etc.)
features <- c("job", "marital", "education", "default", "housing", 
              "loan", "contact", "poutcome")

# Set the number of folds for cross-validation
n_folds <- 5

# Randomly shuffle the data
set.seed(123)
shuffled_data <- bank_data[sample(nrow(bank_data)), ]

# Create fold assignments using sample() to assign each row to a fold
folds <- sample(rep(1:n_folds, length.out = nrow(shuffled_data)))

# Initialize a list to store average Type II error for each depth
depth_type2_errors <- numeric(5)  # One slot for each depth (1-5)

# Loop through different max depths
for (depth in 1:5) {
  
  # Initialize a Type II error list for each fold (since n_folds = 5)
  type2_list <- numeric(n_folds)
  
  # Cross-validation loop
  for(i in 1:n_folds) {
    # Create training and testing sets for each fold
    test_indices <- which(folds == i)
    test_data <- shuffled_data[test_indices, ]
    train_data <- shuffled_data[-test_indices, ]
    
    # Train the model using the selected input features and current max depth
    model <- rpart(subscribed ~ job + marital + education + default + housing + 
                     loan + contact + poutcome, 
                   data = train_data, 
                   method = "class",
                   control = rpart.control(maxdepth = depth))
    
    # Predict on the test set using only the feature columns
    predictions <- predict(model, test_data[, features], type = "class")
    
    # Ensure the levels of predictions are the same as the actual data
    predictions <- factor(predictions, levels = levels(test_data$subscribed))
    
    # Full Confusion Matrix (Actual values first, Predicted values second)
    conf_matrix <- table(Actual = test_data$subscribed, Predicted = predictions)
    
    # Extract TP, TN, FP, and FN from the confusion matrix using numeric indices
    TP <- conf_matrix[2, 2]  # True Positives (Predicted yes, Actual yes)
    FN <- conf_matrix[2, 1]  # False Negatives (Predicted no, Actual yes)
    FP <- conf_matrix[1, 2]  # False Positives (Predicted yes, Actual no)
    TN <- conf_matrix[1, 1]  # True Negatives (Predicted no, Actual no)
    
    # Print the results for each fold
    cat("Fold:", i, "\n")
    cat("True Positives (TP):", TP, "\n")
    cat("False Negatives (FN):", FN, "\n")
    cat("False Positives (FP):", FP, "\n")
    cat("True Negatives (TN):", TN, "\n\n")
    
    # Calculate Type II Error (False Positive Rate) for this fold
    if ((FP + TN) > 0) {  # Avoid division by zero
      fold_type2_error <- FP / (FP + TN)
    } else {
      fold_type2_error <- 0
    }
    
    # Store the Type II error for this fold in the pre-allocated list
    type2_list[i] <- fold_type2_error
  }
  
  # Calculate and store the average Type II error for this max depth
  depth_type2_errors[depth] <- mean(type2_list)
}

# Print the Type II error for each max depth
print(depth_type2_errors)

# Identify the best max depth (lowest Type II error)
best_depth_type2 <- which.min(depth_type2_errors)
cat("Best max depth (lowest Type II error): ", best_depth_type2, "\n")
cat("Lowest Type II error: ", min(depth_type2_errors), "\n")
#----------N Fold cross validation to find best depth for lowest type 1----------
library(rpart)

# Read in the bank data
bank_data <- read.csv("bank-sample.csv")

# Convert categorical variables to factors and ensure consistent levels
bank_data$subscribed <- factor(bank_data$subscribed, levels = c("no", "yes"))
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$education <- as.factor(bank_data$education)
bank_data$default <- as.factor(bank_data$default)
bank_data$housing <- as.factor(bank_data$housing)
bank_data$loan <- as.factor(bank_data$loan)
bank_data$contact <- as.factor(bank_data$contact)
bank_data$poutcome <- as.factor(bank_data$poutcome)

# Define the features used for training (excluding other columns like "age", "balance", etc.)
features <- c("job", "marital", "education", "default", "housing", 
              "loan", "contact", "poutcome")

# Set the number of folds for cross-validation
n_folds <- 5

# Randomly shuffle the data
set.seed(123)
shuffled_data <- bank_data[sample(nrow(bank_data)), ]

# Create fold assignments using sample() to assign each row to a fold
folds <- sample(rep(1:n_folds, length.out = nrow(shuffled_data)))

# Initialize a list to store average Type I error for each depth
depth_type1_errors <- numeric(5)  # One slot for each depth (1-5)

# Loop through different max depths
for (depth in 1:5) {
  
  # Initialize a Type I error list for each fold (since n_folds = 5)
  type1_list <- numeric(n_folds)
  
  # Cross-validation loop
  for(i in 1:n_folds) {
    # Create training and testing sets for each fold
    test_indices <- which(folds == i)
    test_data <- shuffled_data[test_indices, ]
    train_data <- shuffled_data[-test_indices, ]
    
    # Train the model using the selected input features and current max depth
    model <- rpart(subscribed ~ job + marital + education + default + housing + 
                     loan + contact + poutcome, 
                   data = train_data, 
                   method = "class",
                   control = rpart.control(maxdepth = depth))
    
    # Predict on the test set using only the feature columns
    predictions <- predict(model, test_data[, features], type = "class")
    
    # Ensure the levels of predictions are the same as the actual data
    predictions <- factor(predictions, levels = levels(test_data$subscribed))
    
    # Full Confusion Matrix (Actual values first, Predicted values second)
    conf_matrix <- table(Actual = test_data$subscribed, Predicted = predictions)
    
    # Extract TP, TN, FP, and FN from the confusion matrix using numeric indices
    TP <- conf_matrix[2, 2]  # True Positives (Predicted yes, Actual yes)
    FN <- conf_matrix[2, 1]  # False Negatives (Predicted no, Actual yes)
    FP <- conf_matrix[1, 2]  # False Positives (Predicted yes, Actual no)
    TN <- conf_matrix[1, 1]  # True Negatives (Predicted no, Actual no)
    
    # Print the results for each fold
    cat("Fold:", i, "\n")
    cat("True Positives (TP):", TP, "\n")
    cat("False Negatives (FN):", FN, "\n")
    cat("False Positives (FP):", FP, "\n")
    cat("True Negatives (TN):", TN, "\n\n")
    
    # Calculate Type I Error (False Negative Rate) for this fold
    if ((TP + FN) > 0) {  # Avoid division by zero
      fold_type1_error <- FN / (TP + FN)
    } else {
      fold_type1_error <- 0
    }
    
    # Store the Type I error for this fold in the pre-allocated list
    type1_list[i] <- fold_type1_error
  }
  
  # Calculate and store the average Type I error for this max depth
  depth_type1_errors[depth] <- mean(type1_list)
}

# Print the Type I error for each max depth
print(depth_type1_errors)

# Identify the best max depth (lowest Type I error)
best_depth_type1 <- which.min(depth_type1_errors)
cat("Best max depth (lowest Type I error): ", best_depth_type1, "\n")
cat("Lowest Type I error: ", min(depth_type1_errors), "\n")

#------N fold cross validation for best depth for best precision----
library(rpart)

# Read in the bank data
bank_data <- read.csv("bank-sample.csv")

# Convert categorical variables to factors and ensure consistent levels
bank_data$subscribed <- factor(bank_data$subscribed, levels = c("no", "yes"))
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$education <- as.factor(bank_data$education)
bank_data$default <- as.factor(bank_data$default)
bank_data$housing <- as.factor(bank_data$housing)
bank_data$loan <- as.factor(bank_data$loan)
bank_data$contact <- as.factor(bank_data$contact)
bank_data$poutcome <- as.factor(bank_data$poutcome)

# Define the features used for training (excluding other columns like "age", "balance", etc.)
features <- c("job", "marital", "education", "default", "housing", 
              "loan", "contact", "poutcome")

# Set the number of folds for cross-validation
n_folds <- 5

# Randomly shuffle the data
set.seed(123)
shuffled_data <- bank_data[sample(nrow(bank_data)), ]

# Create fold assignments using sample() to assign each row to a fold
folds <- sample(rep(1:n_folds, length.out = nrow(shuffled_data)))

# Initialize a list to store average precision for each depth
depth_precisions <- numeric(5)  # One slot for each depth (1-5)

# Loop through different max depths
for (depth in 1:5) {
  
  # Initialize a precision list for each fold (since n_folds = 5)
  precision_list <- numeric(n_folds)
  
  # Cross-validation loop
  for(i in 1:n_folds) {
    # Create training and testing sets for each fold
    test_indices <- which(folds == i)
    test_data <- shuffled_data[test_indices, ]
    train_data <- shuffled_data[-test_indices, ]
    
    # Train the model using the selected input features and current max depth
    model <- rpart(subscribed ~ job + marital + education + default + housing + 
                     loan + contact + poutcome, 
                   data = train_data, 
                   method = "class",
                   control = rpart.control(maxdepth = depth))
    
    # Predict on the test set using only the feature columns
    predictions <- predict(model, test_data[, features], type = "class")
    
    # Ensure the levels of predictions are the same as the actual data
    predictions <- factor(predictions, levels = levels(test_data$subscribed))
    
    # Full Confusion Matrix (Actual values first, Predicted values second)
    conf_matrix <- table(Actual = test_data$subscribed, Predicted = predictions)
    
    # Extract TP, TN, FP, and FN from the confusion matrix using numeric indices
    TP <- conf_matrix[2, 2]  # True Positives (Predicted yes, Actual yes)
    FN <- conf_matrix[2, 1]  # False Negatives (Predicted no, Actual yes)
    FP <- conf_matrix[1, 2]  # False Positives (Predicted yes, Actual no)
    TN <- conf_matrix[1, 1]  # True Negatives (Predicted no, Actual no)
    
    # Print the results for each fold
    cat("Fold:", i, "\n")
    cat("True Positives (TP):", TP, "\n")
    cat("False Negatives (FN):", FN, "\n")
    cat("False Positives (FP):", FP, "\n")
    cat("True Negatives (TN):", TN, "\n\n")
    
    # Calculate Precision for this fold
    if ((TP + FP) > 0) {  # Avoid division by zero
      fold_precision <- TP / (TP + FP)
    } else {
      fold_precision <- 0
    }
    
    # Store the precision for this fold in the pre-allocated list
    precision_list[i] <- fold_precision
  }
  
  # Calculate and store the average precision for this max depth
  depth_precisions[depth] <- mean(precision_list)
}

# Print the precision for each max depth
print(depth_precisions)

# Identify the best max depth (highest precision)
best_depth_precision <- which.max(depth_precisions)
cat("Best max depth (highest precision): ", best_depth_precision, "\n")
cat("Highest precision: ", max(depth_precisions), "\n")





#-------STEP 2: PREDICT USING BEST DEPTH ON A FEW ROWS------
# After finding the best depth from cross-validation
best_depth_precision <- which.max(depth_precisions)  # Assuming you already calculated this

# Train the final model on the entire dataset using the best max depth
final_model <- rpart(subscribed ~ job + marital + education + default + housing + 
                       loan + contact + poutcome, 
                     data = bank_data, 
                     method = "class",
                     control = rpart.control(maxdepth = best_depth_precision))

levels(bank_data$subscribed)
bank_data$subscribed <- factor(bank_data$subscribed, levels = c("no", "yes"))
# Create new data (the features should match those used in the model)

new_data <- data.frame(
  job = factor(c("admin.", "blue-collar", "technician"), levels = levels(bank_data$job)),
  marital = factor(c("married", "single", "divorced"), levels = levels(bank_data$marital)),
  education = factor(c("tertiary", "secondary", "primary"), levels = levels(bank_data$education)),
  default = factor(c("no", "yes", "no"), levels = levels(bank_data$default)),
  housing = factor(c("yes", "no", "yes"), levels = levels(bank_data$housing)),
  loan = factor(c("no", "no", "yes"), levels = levels(bank_data$loan)),
  contact = factor(c("cellular", "telephone", "cellular"), levels = levels(bank_data$contact)),
  poutcome = factor(c("success", "failure", "unknown"), levels = levels(bank_data$poutcome))
)

# Predict on new data with 3 rows
predictions <- predict(final_model, new_data, type = "class")

# Print the predictions for each row
print(predictions)



################ STEP 2 PREDICT ON THE ENTIRE DATASET##########
# Remove the 'subscribed' column from bank_data for prediction purposes
bank_data_features <- bank_data[, !names(bank_data) %in% "subscribed"]


# Step 1: Predict on the dataset without the response variable using the final model
predictions_entire <- predict(final_model, bank_data_features, type = "class")

# Step 2: Create a confusion matrix by comparing predictions to actual values
conf_matrix <- table(Predicted = predictions_entire, Actual = bank_data$subscribed)
print(conf_matrix)

# Step 3: Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Step 4: Calculate Type I and Type II errors
# Type I error (False Positive Rate): Predicting "yes" when actual is "no"
type_1_error <- conf_matrix["yes", "no"] / sum(conf_matrix[, "no"])

# Type II error (False Negative Rate): Predicting "no" when actual is "yes"
type_2_error <- conf_matrix["no", "yes"] / sum(conf_matrix[, "yes"])

cat("Type I Error (False Positive Rate):", type_1_error, "\n")
cat("Type II Error (False Negative Rate):", type_2_error, "\n")

# Step 5: Calculate Precision
# Precision: True Positives / (True Positives + False Positives)
precision <- conf_matrix["yes", "yes"] / sum(conf_matrix["yes", ])
cat("Precision:", precision, "\n")


#----------------WHAT ARE THE IMPT FEATURES OF THE TREE-------------
iris <- read.csv("iris.csv")
head(iris)
dim(iris)
library("rpart")
library("rpart.plot")

fit <- rpart(class ~sepal.length + sepal.width + petal.length+petal.width,
             method="class",
             data=iris,
             control=rpart.control(minsplit=1),
             parms=list(split='information')
)
rpart.plot(fit, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE)
# if the measurement of petal.length is <2.5cm, the flower is Iris-setosa with 100% probability
# if the petal length is >= 2.5cm and petal width is >= to 1.8cm, 
# there is a high chance of 45/46 that is will be Iris-Virgincia
# if the petal length is >=2.5cm and petal width < 1.8cm and petal length is < 5cm,
# there is a probability of 47/48 that it is a iris-versicolor
# if the petal length is >= 2.5cm and the petal width is <1.8cm and petal length >= 5cm
# there is a probability of 4/6 that it is a iris-virgincia


pred = predict(fit, newdata = iris[, 1:4],
               type = "class")

confusion_matrix <- table(iris$class, pred)

#--------------------what are the more important features in the tree------
# it seems like the petal length and petal width are the more important features
# the sepal length and sepal width are not important in the classification

# Load necessary libraries
library(rpart)  # Decision Tree
library(ROCR)   # ROC and AUC

# 1) Train the Decision Tree model using selected features
features <- c("job", "marital", "education", "default", 
              "housing", "loan", "contact", "poutcome") # CHANGE THIS

fit_dt <- rpart(subscribed ~ .,  # CAN LEAVE THE . SINCE WE CONTROL THE FEATURES
                data = bankdata[, c(features, "subscribed")],  # include response
                method = "class",
                control = rpart.control(minsplit = 1),  
                parms = list(split = 'information'))

# 2) Predict probabilities for the positive class ("yes")
prob_dt <- predict(fit_dt, newdata = bankdata[, features], type = "prob")[,2]

# 3) Generate ROC performance objects
pred_dt <- prediction(prob_dt, bankdata$subscribed)
roc_dt <- performance(pred_dt, "tpr", "fpr")
auc_dt <- performance(pred_dt, "auc")

# 4) Extract and print the AUC value
auc_dt_value <- round(auc_dt@y.values[[1]], 4)
cat("AUC (Decision Tree):", auc_dt_value, "\n")

# 5) Plot the ROC curve with AUC in the title
plot(roc_dt, col = "red", lwd = 2,
     main = paste("ROC Curve (AUC =", auc_dt_value, ")"),
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)")

# 6) Add a diagonal reference line (random guessing line)
abline(a = 0, b = 1, lty = 2, col = "gray")

# --------------------------------------------------------------------
# Threshold Performance Curve
# -------------------------------------------------------------

# 7) Extract alpha (threshold), FPR, and TPR values from ROC object
alpha_dt <- round(as.numeric(unlist(roc_dt@alpha.values)), 4)  # Thresholds (alpha values)
fpr_dt <- round(as.numeric(unlist(roc_dt@x.values)), 4)        # False Positive Rate (FPR)
tpr_dt <- round(as.numeric(unlist(roc_dt@y.values)), 4)        # True Positive Rate (TPR)

# 8) Calculate Youden's J statistic for each threshold (Youden's J = TPR - FPR)
youden_dt <- tpr_dt - fpr_dt  # Youden's J

# 9) Identify the optimal threshold based on the maximum Youden's J
optimal_index_dt <- which.max(youden_dt)  # Index of max Youden's J
optimal_threshold_dt <- alpha_dt[optimal_index_dt]  # Optimal threshold (alpha value)
optimal_youden_dt <- youden_dt[optimal_index_dt]    # Max Youden's J value

cat("Optimal Threshold (Youden's J):", optimal_threshold_dt, "\n")

# 10) Adjust margins to fit multiple axes properly
par(mar = c(5, 5, 4, 5))  # Adjust margins

# 11) Plot TPR vs. Alpha on the left y-axis
plot(alpha_dt, tpr_dt, type = "l", col = "blue", lwd = 2, 
     xlab = "Alpha (Threshold)", ylab = "True Positive Rate (TPR)", 
     main = "TPR, FPR, and Youden's J vs Alpha", ylim = c(0, 1))

# 12) Overlay FPR vs. Alpha on the right y-axis
par(new = TRUE)  # Overlay FPR plot
plot(alpha_dt, fpr_dt, type = "l", col = "red", lwd = 2, 
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 1))
axis(side = 4)
mtext("False Positive Rate (FPR)", side = 4, line = 3, col = "red")

# 13) Overlay Youden's J vs. Alpha on the same plot
lines(alpha_dt, youden_dt, col = "green", lwd = 2, lty = 2)

# 14) Annotate the optimal threshold on the Youden's J curve
text(optimal_threshold_dt, optimal_youden_dt, 
     paste0("Optimal: ", optimal_threshold_dt), 
     col = "green", pos = 4)

# 15) Add labels near the TPR and FPR curves
text(0.2, 0.2, "FPR", col = "red", pos = 4)
text(0.7, 0.7, "TPR", col = "blue", pos = 4)

# 16) Optional: Add a legend to distinguish curves
legend("topright", legend = c("TPR", "FPR", "Youden's J"), 
       col = c("blue", "red", "green"), lty = c(1, 1, 2), lwd = 2)

youden_dt <- tpr_dt - fpr_dt
threshold_data_dt <- cbind(alpha_dt, tpr_dt, fpr_dt, youden_dt); threshold_data_dt














