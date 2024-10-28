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
library(e1071)
model <- naiveBayes(Enrolls ~ Age + Income + JobSatisfaction + Desire, traindata)
results = predict(model, testdata, "raw"); results # to get raw probabilities
results[2] / results[1] # ratio of yes and no
results = predict(model, testdata, "class")
results
data$Enrolls[data$Enrolls == ""] <- "Yes"

# build ROC curve
# Load necessary libraries
library(e1071)  # Naive Bayes model
library(ROCR)   # ROC curve and AUC calculation

# 1) Train the Naive Bayes model
model <- naiveBayes(Enrolls ~ Age + Income + JobSatisfaction + Desire, data = data)

# 2) Predict raw probabilities on the entire dataset
probabilities <- predict(model, data, type = "raw")

# 3) Extract probabilities for the positive class (e.g., "Yes")
positive_probs <- probabilities[, 2]  # Assuming "Yes" is the second column

# 4) Create a prediction object for the ROC curve
roc <- prediction(positive_probs, data$Enrolls)

# 5) Generate the performance object for the ROC curve
perf <- performance(roc, "tpr", "fpr")

# 6) Calculate AUC value
auc_value <- performance(roc, "auc")@y.values[[1]]

# 7) Plot the ROC curve with AUC value in the title
plot(perf, col = "blue", lwd = 2, 
     main = paste0("ROC Curve (AUC = ", round(auc_value, 4), ")"),
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal reference line

# -----------------------------------------
# Part 2: Plot Threshold vs. TPR/FPR/G-Mean Graph
# -----------------------------------------
# 1) Generate performance objects for TPR, FPR, and thresholds
tpr_perf <- performance(roc, "tpr", "cutoff")
fpr_perf <- performance(roc, "fpr", "cutoff")

# 2) Extract and round thresholds (alpha), TPR, and FPR values to 4 decimal places
alpha <- round(tpr_perf@x.values[[1]], 4)  # Thresholds (cutoffs)
tpr <- round(tpr_perf@y.values[[1]], 4)    # True positive rates (TPR)
fpr <- round(fpr_perf@y.values[[1]], 4)    # False positive rates (FPR)

# 3) Calculate G-Mean for each threshold
gmean <- sqrt(tpr * (1 - fpr))  # G-Mean formula

# 4) Identify the optimal threshold based on the maximum G-Mean
optimal_index <- which.max(gmean)  # Index of max G-Mean
optimal_threshold <- alpha[optimal_index]  # Optimal threshold rounded to 4dp
optimal_gmean <- round(gmean[optimal_index], 4)  # Max G-Mean value

cat("Optimal Threshold (G-Mean):", optimal_threshold, "\n")

# 5) Set consistent y-axis limits for TPR and FPR
y_limit <- range(0, 1)  # Ensure all metrics fit within [0, 1]

# 6) Adjust margins to fit multiple axes properly
par(mar = c(5, 5, 2, 5))  # Adjust the margins (change if needed)

# 7) Plot TPR vs. Threshold on the left y-axis
plot(alpha, tpr, type = "l", col = "blue", lwd = 2, 
     xlab = "Threshold", ylab = "True Positive Rate (TPR)", 
     xlim = c(0, 1), ylim = y_limit, 
     main = "TPR, FPR, and G-Mean vs Threshold")

# 8) Overlay FPR vs. Threshold with a secondary axis
par(new = TRUE)  # Overlay FPR plot
plot(alpha, fpr, type = "l", col = "red", lwd = 2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = y_limit)

# 9) Add a secondary axis for FPR on the right side
axis(side = 4)
mtext("False Positive Rate (FPR)", side = 4, line = 3, col = "black")

# 10) Overlay G-Mean vs. Threshold with another right-side axis
par(new = TRUE)  # Overlay G-Mean plot
plot(alpha, gmean, type = "l", col = "green", lwd = 2, lty = 2, 
     axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = range(gmean))

# 11) Add a third axis for G-Mean on the far right
axis(side = 4, at = pretty(gmean), col.axis = "green", line = 6, las = 1)
mtext("G-Mean", side = 4, line = 7, col = "green")

# 12) Annotate the optimal threshold on the G-Mean curve
text(0.2, 0.6, 
     paste0("Best Threshold: ", optimal_threshold), 
     col = "green", pos = 4)

# 13) Add labels near the TPR and FPR curves
text(0.2, 0.2, "FPR", col = "red", pos = 4)
text(0.7, 0.7, "TPR", col = "blue", pos = 4)

# 14) Optional: Add a legend to distinguish the curves
legend("topright", legend = c("TPR", "FPR", "G-Mean"), 
       col = c("blue", "red", "green"), lty = c(1, 1, 2), lwd = 2)
