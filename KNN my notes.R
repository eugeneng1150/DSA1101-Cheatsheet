market = read.csv("Smarket.csv")
dim(market)
head(market)
library("class")
set.seed(1)
#-------------------------- PREPARING DATA TO FORM MODEL AND TO TEST MODEL---------------------------
# REMEMBER TO SCALE THE DATA PLEASE SCALE PLEASE DONT FORGET
# Mostly scale quantitative variables
# If categorical and is ordered, can treat as quantitative (eg, age 1 (0-18yro),2 (19-30yro),3(30-40yro),4(41++))
# otherwise,(Nominal: Gender, race, religion) dont scale, drop (disadvantage)

# to separate the data above to two parts: 
# one part is used to train the model 80%
# another part is to test the model. 20%
# We'll select the rows that belong to the years before 2005 to train model 
# index of the rows before year 2005 is in the vector "index.train":



#filter data < 2005
index_train = which(market$Year<2005) ; index_train
train_data = market[which(market$Year<2005),]
test_data = market[which(market$Year>2004),]
# check if correct---------
tail(test_data)
tail(train_data)
dim(train_data) 
dim(test_data)

#-----------------
# form a SET OF FEATURES for the training (train.x); and for testing (test.x) :
train_x <- train_data[,c('Lag1',"Lag2","Lag3","Lag4","Lag5")] #ITEM 1
test_x <- test_data[,c('Lag1',"Lag2","Lag3","Lag4","Lag5")]   #ITEM 2

# form the RESPONSE for the training (train_y); and for testing (test_y)
train_y <- train_data[,c("Direction")] #Item 3
test_y <- test_data[, c("Direction")]

knn_pred <- knn(train=train_x, test=test_x, cl=train_y, k=1) ; knn_pred
# 1) regressor 2) response of regressor 3) train testing 
confusion_table <- table(test_y, knn_pred)
# 55 Down were predicted correctly;
# 75 Ups were predicted correctly.
# the rest (56 + 66) were predicted wrongly by the KNN classifier where k = 1.


#-----------------------ACCURACY-----------------
accuracy <- sum(diag(confusion_table))/sum(confusion_table) ;accuracy
# 0.515873
TP <- confusion_table[1,1] 
FN <- confusion_table[1,2]
FP <- confusion_table[2,1]
TN <- confusion_table[2,2]

TPR <- TP/sum(TP + FN) ; TPR #measures the proportion of actual positives that are correctly identified by the model. 
FPR <- FP/sum(FP + TN) ; FPR #measures the proportion of actual negatives that are incorrectly classified as positives by the model => Type 1
FNR <- FN/sum(TP + FN) ; FNR # measures the proportion of actual positives that are incorrectly classified as negatives by the model. => Type 2
precision <- TP/ sum(TP + FP) ;precision #measures the proportion of predicted positives that are actually correct. It's useful when false positives are costly.
# since changing year will change accuracy, let each portion be the  test set 1 time
# find mean(accuracy)


#----------------Split data to 80% train, 20% test and use KNN--------------
# Load necessary library
library(class)

# Step 1: Load the dataset
credit <- read.csv("German_credit.csv")
head(credit)

# Step 2: Separate features (X) and response (Y)
X <- scale(credit[, 2:5])  # Scaling columns 2 to 5 as features
Y <- credit$Creditability   # Response variable

# Step 3: Get the total number of rows
n <- nrow(credit)

# Step 4: Shuffle the row indices using sample() and select 80% for training
train_indices <- sample(1:n, size = floor(0.8 * n), replace = FALSE) #CHANGE IF NOT 80%

# Step 5: Split the data into training and testing sets
train_data <- X[train_indices, ]  # 80% of the data for training
test_data <- X[-train_indices, ]   # Remaining 20% for testing

# Optional: Check the dimensions using print() and paste()
print(paste("Training set dimensions: ", dim(train_data)[1], " rows and ", dim(train_data)[2], " columns"))
print(paste("Testing set dimensions: ", dim(test_data)[1], " rows and ", dim(test_data)[2], " columns"))

# Step 6: Apply k-NN
k <- 1  # You can change the value of k as needed
pred <- knn(train = train_data, test = test_data, cl = Y[train_indices], k = k)

# Step 7: Create a confusion matrix and check results
confusion_matrix <- table(actual = Y[-train_indices], prediction = pred)
print(confusion_matrix)

# Step 8: Calculate the accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))






########  RANDOM WAY TO SPLIT ORIGINAL DATA INTO TRAIN AND TEST with 8:2 ratio

#---------------N fold cross validation------------------------
 

#  A small example on dividing whole data set into n folds  #################
n_folds=3
Number_of_datapoints=12 # sample size
index=rep(1:n_folds,length.out = Number_of_datapoints)
s = sample(index); s
table(s) 
# dataset of dim(data)[1] points is devided into n folds randomly, each fold has 4 points.
# the 4 points for each of 3 folds are selected from the dataset following s. For example,
# s = 3 1 1 3 2 2 2 2 1 3 1 3
# then, the first data point belongs to 3rd fold. The next 2 points belong to 1st fold, etc.
##################################################

## 5-fold Cross-Validation for KNN with k=1, 5, 10, etc. for the data set Smarket.csv

X=market[,c("Lag1","Lag2","Lag3","Lag4","Lag5")] # columns of explanatories/features
Y=market[,c("Direction")] # response

dim(market) # 1250 data points/observations and 10 columns

dim(market)[1] # 1250

n_folds=5
?rep()

folds_j <- sample(rep(1:n_folds, length.out = dim(market)[1] ))  

# group names from 1 to 5 in a random order

table(folds_j)

#err=numeric(n_folds) # vector to store the error rate of each fold

acc=numeric(n_folds) # vector to store the accuracy of each fold in the loop below

for (j in 1:n_folds) {
  test_j <- which(folds_j == j) # get the index of the points that will be in the test set
  pred <- knn(train=X[ -test_j, ], test=X[test_j, ], cl=Y[-test_j ], k=1) # KNN with k = 1, 5, 10, etc
  
  #err[j]=mean(Y[test_j] != pred) 
  acc[j]=mean(Y[test_j] == pred) 
  # this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix), where confusion.matrix=table(Y[test_j],pred)
  
}

#err
acc

#error=mean(err); error
accur=mean(acc); accur # this is the average accuracy over 5 folds


#----------- FIND THE BEST K VALUE FOR TYPE 1 AND 2 ERROR AND PRECISION AND ACCURACY NO N FOLD (MODIFY IF NEED BE)---------------------
# Load and preprocess data
data <- read.csv("data2.csv") #CHANGE THIS

# Standardize the relevant columns (excluding 'year')
data_stand <- scale(data[, c("age", "node")]) #CHANGE THIS

# Define features (X) and response (Y)
X <- data_stand[, c("age", "node")]  # CHANGE THIS

# Ensure the response variable 'survival.status' is treated correctly
# Set 1 as negative and 2 as positive
Y <- factor(data$survival.status, levels = c(1, 2)) #CHANGE THIS INCLUDING THE LEVELS

# Initialize vectors to store Type I, Type II error rates, accuracy, and precision
ave_type1 <- numeric(100)  # Storing for 50 iterations of different k values
ave_type2 <- numeric(100)
ave_accuracy <- numeric(100)
ave_precision <- numeric(100)

# Loop over different k values for KNN (from 1 to 50)
for (i in 1:100) {
  # Train and test on the same dataset
  pred <- knn(train = X, test = X, cl = Y, k = i)
  
  # Create confusion matrix
  confusion_table <- table(Y, pred)
  
  # Extract True Positives (TP), False Negatives (FN), False Positives (FP), and True Negatives (TN)
  TP <- confusion_table[2, 2]  # Actual "2" (positive) and predicted "2"
  FN <- confusion_table[2, 1]  # Actual "2" but predicted "1"
  FP <- confusion_table[1, 2]  # Actual "1" (negative) but predicted "2"
  TN <- confusion_table[1, 1]  # Actual "1" and predicted "1"
  
  # Print TP, FN, FP, and TN for each k value
  print(paste("k:", i))
  print(paste("True Positives (TP):", TP))
  print(paste("False Negatives (FN):", FN))
  print(paste("False Positives (FP):", FP))
  print(paste("True Negatives (TN):", TN))
  
  # Calculate Type I and Type II error rates
  ave_type1[i] <- FN / (TP + FN)  # Type I Error (False Negative Rate)
  ave_type2[i] <- FP / (FP + TN)  # Type II Error (False Positive Rate)
  
  # Calculate Accuracy
  ave_accuracy[i] <- (TP + TN) / (TP + TN + FP + FN)
  
  # Calculate Precision, handle case where TP + FP = 0
  if (TP + FP == 0) {
    ave_precision[i] <- 0  # No positive predictions, precision is set to 0
  } else {
    ave_precision[i] <- TP / (TP + FP)
  }
}

# Results: Print average Type I, Type II error rates, accuracy, and precision across all k values
print(ave_type1)
print(ave_type2)
print(ave_accuracy)
print(ave_accuracy)
mean(ave_accuracy)
print(ave_precision)

#------------------------IMPORTANT!!!!!!!!!!----------------------------
#----------N FOLD TO FIND THE BEST K VALUE FOR TYPE 1 AND 2 ERROR AND PRECISION AND ACCURACY (MODIFY IF NEED BE)----------------
# Load and preprocess data
library("class")
data <- read.csv("data2.csv") ### CHANGE THIS
head(data)                    ### CHANGE THIS
# Standardize the relevant columns (excluding 'year')
data_stand <- scale(data[, c("age", "node")])     #CHANGE THIS

# Define features (X) and response (Y)
X <- data_stand[, c("age", "node")]  # #CHANGE THIS

# Ensure the response variable 'survival.status' is treated correctly
# Set 1 as negative and 2 as positive
Y <- factor(data$survival.status, levels = c(1, 2)) #CHANGE THIS INCLUDING LEVELS

# Set up cross-validation with 3 folds
n_folds <- 3        #CHANGE THIS
folds_j <- sample(rep(1:n_folds, length.out = dim(data)[1]))  # CHANGE THIS

# Initialize vectors to store average Type I, Type II error rates, accuracy, and precision
ave_type1 <- numeric(100)  # Storing for 50 iterations of different k values
ave_type2 <- numeric(100)
ave_accuracy <- numeric(100)
ave_precision <- numeric(100)

# Loop over different k values for KNN (from 1 to 50)
for (i in 1:100) {
  type1 <- numeric(n_folds)  # Vector to store Type I error for each fold
  type2 <- numeric(n_folds)  # Vector to store Type II error for each fold
  accuracy <- numeric(n_folds)  # Vector to store accuracy for each fold
  precision <- numeric(n_folds)  # Vector to store precision for each fold
  
  for (j in 1:n_folds) {
    # Split the data into training and test sets
    test_j <- which(folds_j == j)
    pred <- knn(train = X[-test_j, ], test = X[test_j, ], cl = Y[-test_j], k = i)
    
    # Create confusion matrix
    confusion_table <- table(Y[test_j], pred)
    print(confusion_table)
    # Extract True Positives (TP), False Negatives (FN), False Positives (FP), and True Negatives (TN)
    TP <- confusion_table[2, 2]  # Actual "2" (positive) and predicted "2"
    FN <- confusion_table[2, 1]  # Actual "2" but predicted "1"
    FP <- confusion_table[1, 2]  # Actual "1" (negative) but predicted "2"
    TN <- confusion_table[1, 1]  # Actual "1" and predicted "1"
    
    # Print TP, FN, FP, and TN for each fold and k
    cat("Fold:", j, "k:", i, "\n")
    cat("True Positives (TP):", TP, "\n")
    cat("False Negatives (FN):", FN, "\n")
    cat("False Positives (FP):", FP, "\n")
    cat("True Negatives (TN):", TN, "\n\n")
    
    # Calculate Type I and Type II error rates for this fold
    type1[j] <- FN / (TP + FN)  # Type I Error (False Negative Rate)
    type2[j] <- FP / (FP + TN)  # Type II Error (False Positive Rate)
    
    # Calculate Accuracy for this fold
    accuracy[j] <- (TP + TN) / (TP + TN + FP + FN)
    
    # Calculate Precision for this fold
    precision[j] <- TP / (TP + FP)
  }
  
  # Average Type I and Type II error rates, accuracy, and precision for this value of k
  ave_type1[i] <- mean(type1)
  ave_type2[i] <- mean(type2)
  ave_accuracy[i] <- mean(accuracy)
  ave_precision[i] <- mean(precision)
}

# Results: Print average Type I, Type II error rates, accuracy, and precision across all k values

print(ave_type1)
print(ave_type2)
print(ave_accuracy)
mean(ave_accuracy)
print(ave_precision)

#######PLOT IT AND FIND BEST K####
print(ave_accuracy)
max(ave_accuracy)
index <- which(ave_accuracy == max(ave_accuracy))
plot(x=1:100, ave_accuracy)
abline(v = index , col= "red")

#------------WHEN ADDING A NEW DATA POINT, NEED TO STANDERDIZE WITH
# THE WHOLE DATA THEN EXTRACT THE LAST ROW WHICH IS THE ADDED ROW-------
new = data.frame(X1 = 83, X2 = 57, X3= 2, X4 = 3)
standard = scale(rbind(data[,2:5], new) ) 
# add the new point to the x-values of the whole data, then standardizing all
standard.new = standard[90,] # x values of the new point after standardizing
standard.new
#using the classifier to predict for the new point:
test = knn(train=standardized.X, test=standard.new, cl=data[,1], k=3)
test

#----------------WHAT HAPPENS IF U NEED TO ADD IN A DATAFRAME TO PREDICT OUTCOME---------
# Load and preprocess data
data <- read.csv("data2.csv")

# Define features (age and node) for existing data (X-values) and new data points
new <- data.frame(age = c(45, 50), node = c(5, 2))  # New data points to predict

# Combine the new data points with the existing data for standardization
combined_data <- rbind(data[, c("age", "node")], new)

# Standardize the combined data (both existing and new points)
standard <- scale(combined_data)

# Extract the standardized values of the new data points (the last rows)
standard_new <- standard[(nrow(standard) - 1):nrow(standard), ]  # The last two rows

# Extract the standardized values of the original data
standardized_X <- standard[-c((nrow(standard) - 1):nrow(standard)), ]  # All rows except the last two
# might need to change the value of 1

# Use KNN to predict the class of the new data points
library(class)  # Make sure you load the class library
predictions <- knn(train = standardized_X, test = standard_new, cl = data$survival.status, k = 3)

# Print the prediction results
print("The predicted survival status for the new data points is:")
print(predictions)


