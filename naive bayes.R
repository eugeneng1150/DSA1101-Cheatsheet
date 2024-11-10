# NOTES:
# by default, R uses majority rule where threshold is 0.5
# however in certain data set such as diabetics, only 10% of population have diabetics
# need to change to match the lower population. if probability of yes = 0.3 and no = 0.7, by majority rule 
# patient should not have diabetics
# but if threshold is 0.1, then patient will be classified as diabetic
# this changing of threshold will affect goodness of model / classifer
# need to get threshold that gives the best model
# How to choose proper threshold?
### CONSIDER TPR AND FPR, hope that have High TPR low FPR
### USE ROC CURVE
### TPR on Y,, FPR on X
### the line that approach the TOP LEFT (high TPR and low FPR IS THE BEST) (CALCULUATE AREA OF CURVE TO DETERMINE)
##### NEED THE PROBABILITY OF YES TO PLOT ROC CURVE
# what Threshold to use?
## plot TPR vs S vs FPR # to plot ROC, need y hat and actual y answer

# MANUAL CALCULATION OF NAIVE BAYES
data <- read.csv("sample1.csv")
head(data)
data
library(e1071)
traindata <- as.data.frame(data[1:14,]) #first 14 rows
testdata <- as.data.frame(data[15,]) # the 15th row
testdata
# Check proportion of response
tab <- table(traindata$Enrolls) ; tab
tab <- prop.table(tab); tab

ageCounts <- table(traindata[,c("Enrolls", "Age")]); ageCounts
age_pro <- prop.table(ageCounts, margin =1) ; age_pro # margin = 1 to get row-wise probabilities
# means probability of age <= 30 with enroll is yes is 0.2222
# probability of age 31 to 40 that is enrolled is 0.4444

incomeCounts <- table(traindata[,c("Enrolls", "Income")])
income_pro <- prop.table(incomeCounts, margin = 1); income_pro
# probability of income high that is not enrolled is 0.4
# probability of income low that is not enrolled is 0.2
# probability of income medium that is not enrolled is 0.4
# probability of income high that is enrolled is 0.222
# probability of income low that is enroolled is 0.333
# probability of income medium that is enrolled is 0.444

jsCounts <- table(traindata[,c("Enrolls", "JobSatisfaction")])
js_pro <- prop.table(jsCounts, margin = 1); js_pro

desireCounts <- table(traindata[, c("Enrolls", "Desire")])
desire_pro <- prop.table(desireCounts, margin = 1); desire_pro
# probability of desire excellent that is not enrolled is 0.6
# probability of desire that is fair that is not enrolled is 0.4
# probability of desire that is excellent that is enrolled is 0.333
# probability of desire that is fair that is enrolled is 0.666

# probability of a specific user that Enrolled
prob_yes <- tab["Yes"] * age_pro["Yes", testdata[,c("Age")]] * 
  income_pro["Yes", testdata[, c("Income")]] * 
  js_pro["Yes", testdata[,c("JobSatisfaction")]] * 
  desire_pro["Yes", testdata[,c("Desire")]]  
prob_yes

prob_no <- tab["No"] * age_pro["No", testdata[,c("Age")]] * 
  income_pro["No", testdata[, c("Income")]] * 
  js_pro["No", testdata[,c("JobSatisfaction")]] * 
  desire_pro["No", testdata[,c("Desire")]]  
prob_no

prob_yes / prob_no # take the ratio between prob of yes and no
# predicted result for test is "Yes"

prob_yes / (prob_yes + prob_no) # gives probability of Y = 1, Enrolled



##################### Using e1071 to predict ########################
# predict(NB, "raw") --> use to build ROC curve

data <- read.table("sample1.csv",header=TRUE,sep=",")
head(data)
traindata <- as.data.frame(data[1:14,])
testdata <- as.data.frame(data[15,])
testdata
library(e1071)
model <- naiveBayes(Enrolls ~ Age + Income + JobSatisfaction + Desire, traindata)
results = predict(model, testdata, "raw"); results # to get raw probabilities
results[2] / results[1] # ratio of yes and no
results = predict(model, testdata, "class")
results
# Convert the 'Enrolls' column to character to make replacement easier
data$Enrolls <- as.character(data$Enrolls)

# Replace empty strings or whitespace-only strings with "Yes"
data$Enrolls[trimws(data$Enrolls) == ""] <- "Yes"
data

# build ROC curve
# Load required libraries
library(e1071)  # Naive Bayes model
library(ROCR)   # ROC curve and AUC calculation

# 1) Train the Naive Bayes model
model_nb <- naiveBayes(Enrolls ~ Age + Income + JobSatisfaction + Desire, data = data)

# 2) Predict raw probabilities on the entire dataset
prob_nb <- predict(model_nb, data, type = "raw")[, 2]  # Extract probabilities for "Yes" class

# 3) Generate ROC performance objects for Naive Bayes
pred_nb <- prediction(prob_nb, data$Enrolls)
roc_nb <- performance(pred_nb, "tpr", "fpr")
auc_nb <- performance(pred_nb, "auc")

# 4) Extract and print the AUC value
auc_nb_value <- round(auc_nb@y.values[[1]], 4)
cat("AUC (Naive Bayes):", auc_nb_value, "\n")

# 5) Plot the ROC curve with AUC in the title
plot(roc_nb, col = "blue", lwd = 2,
     main = paste("ROC Curve (AUC =", auc_nb_value, ")"),
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)")

# 6) Add a diagonal reference line (random guessing line)
abline(a = 0, b = 1, lty = 2, col = "gray")

# --------------------------------------------------------------------
# Threshold Performance Curve for Naive Bayes
# --------------------------------------------------------------------

# 7) Extract alpha (threshold), FPR, and TPR values from ROC object
alpha_nb <- round(as.numeric(unlist(roc_nb@alpha.values)), 4)  # Thresholds (alpha values)
fpr_nb <- round(as.numeric(unlist(roc_nb@x.values)), 4)        # False Positive Rate (FPR)
tpr_nb <- round(as.numeric(unlist(roc_nb@y.values)), 4)        # True Positive Rate (TPR)

# 8) Calculate Youden's J statistic for each threshold (Youden's J = TPR - FPR)
youden_nb <- tpr_nb - fpr_nb  # Youden's J

# 9) Identify the optimal threshold based on the maximum Youden's J
optimal_index_nb <- which.max(youden_nb)  # Index of max Youden's J
optimal_threshold_nb <- alpha_nb[optimal_index_nb]  # Optimal threshold (alpha value)
optimal_youden_nb <- youden_nb[optimal_index_nb]    # Max Youden's J value

cat("Optimal Threshold (Youden's J):", optimal_threshold_nb, "\n")

# 10) Adjust margins to fit multiple axes properly
par(mar = c(5, 5, 4, 5))  # Adjust margins

# 11) Plot TPR vs. Alpha on the left y-axis
plot(alpha_nb, tpr_nb, type = "l", col = "blue", lwd = 2, 
     xlab = "Alpha (Threshold)", ylab = "True Positive Rate (TPR)", 
     main = "TPR, FPR, and Youden's J vs Alpha (Naive Bayes)", ylim = c(0, 1))

# 12) Overlay FPR vs. Alpha on the right y-axis
par(new = TRUE)  # Overlay FPR plot
plot(alpha_nb, fpr_nb, type = "l", col = "red", lwd = 2, 
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 1))
axis(side = 4)
mtext("False Positive Rate (FPR)", side = 4, line = 3, col = "red")

# 13) Overlay Youden's J vs. Alpha on the same plot
lines(alpha_nb, youden_nb, col = "green", lwd = 2, lty = 2)

# 14) Annotate the optimal threshold on the Youden's J curve
text(x=0.5, y = 0.6, 
     paste0("Optimal: ", optimal_threshold_nb), 
     col = "green", pos = 4)
abline(v = optimal_threshold_nb, col = "purple", lty = 2)

# 15) Add labels near the TPR and FPR curves
text(0.2, 0.4, "FPR", col = "red", pos = 4)
text(0.7, 0.7, "TPR", col = "blue", pos = 4)

# 16) Optional: Add a legend to distinguish curves
legend("bottomleft", legend = c("TPR", "FPR", "Youden's J"), 
       col = c("blue", "red", "green"), lty = c(1, 1, 2), lwd = 2)

# 17) Create a data frame to manually inspect the thresholds and performance metrics
threshold_data_nb <- data.frame(
  Threshold = alpha_nb,
  TPR = tpr_nb,
  FPR = fpr_nb,
  YoudenJ = youden_nb
)
print(threshold_data_nb)


