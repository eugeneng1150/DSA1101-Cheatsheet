# LOGSTIRC REGRESSION ONLY CAN USE 2 OUTCOMES EITHER YES OR NO
# FITTED MODEL:
## log(p hat / (1-phat)) = 3.5 + 0.25 A + 0.382C
## but we always give answer as odd and not log odd eg:
### At the same age, customer with church contacts by 1 will have odds off churning increase
### by e^0.382 times (REMEMBER IT IS TIMES AND NOT PLUS)

# Template for answering Coefficient of Logistic Regression graph for NUMERIC FEATURES
# if the other regressors are fixeed, when X increases by one unit,
# the Log-Odds of Y increases/decreases by _____ units
# Thus the odds of Y will be e^___ times larger / smaller

# Template for answering Coefficient of Logistic Regression graph for CATEGORICAL FEATURES
# X is the reference, Y is the indicator
# Meaning, given the same condition on ______, when comparing to X, the LOG-odds of ____
# for Y is higher/lower than that of X by _____
# Thus, odds of ____ for Y will be more/ less thasn that of X by e^_____ times

library(ROCR)
data = read.csv("churn.csv")
head(data)

data$Churned = as.factor(data$Churned)
data$Married = as.factor(data$Married)
data= data[,-1] #Remove ID column
str(data)
attach(data)

table(Churned)
prop.table(table(Churned))


# LOGISTIC MODEL
M1<- glm( Churned ~., data = data,family = binomial)
summary(M1)
# if we don't specify "family = binomial", then a LINEAR model is formed, not logistic model


M2<- glm( Churned ~ Age + Married + Churned_contacts,
          data = data,family = binomial(link ="logit"))
summary(M2)

M3<- glm( Churned ~Age + Churned_contacts,
          data = data,family = binomial(link ="logit"))
summary(M3)

predict(M3, newdata = data.frame(Age = 50, Churned_contacts = 5), type ="response")
# type = 'response' gives actual probability of positive
# type = link gives log of the odd, need to transform
# type = "term" will give individual term contribution to log of the odd
# for glm(), when family = binomial, the default predictions are of log-odds (probabilities on logit scale) 

########  PLOTTING ROC CURVE FOR LOGSTIC REGRESSION #######
# Step 1: Generate predicted probabilities for logistic regression model (positive class)
# Use type = "response" to get probabilities instead of class labels
prob_LR = predict(M3, type = "response") 

# Step 2: Create prediction object with predicted probabilities and true class labels
# 'Churned' should be the actual target variable with true labels
pred_LR = prediction(prob_LR, Churned)

# Step 3: Calculate performance metrics: ROC and AUC
# ROC for True Positive Rate (tpr) vs. False Positive Rate (fpr)
roc_LR = performance(pred_LR, "tpr", "fpr")
# AUC (Area Under the Curve)
auc_LR = performance(pred_LR, measure = "auc")

# Display AUC value
auc_LR_value = auc_LR@y.values[[1]]
print(paste("AUC:", round(auc_LR_value, 4)))

# Step 4: Plot the ROC curve with AUC displayed in the title
plot(roc_LR, col = "red", main = paste("ROC Curve (AUC:", round(auc_LR_value, 4), ")"))


#threshold performance curve:
# 1) extract the alpha(threshold), FPR , and TPR values from roc
alpha <- round (as.numeric(unlist(roc@alpha.values)) ,4) # threshold values to calculate TPR and FPR
# unlist() is to convert the list format of roc@alpha.values into num vector
length(alpha) 
fpr <- round(as.numeric(unlist(roc@x.values)) ,4) # x.values == fpr
tpr <- round(as.numeric(unlist(roc@y.values)) ,4) # y.values == tpr

#2)  adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5)) #CHANGE ARG IF NEEDED

#3) Plot tpr, fpr, alpha
plot(alpha ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
# tells R to plot on top of the existing plot
plot(alpha ,fpr , xlab ="", ylab ="", axes =F, xlim =c(0 ,1) , type ="l", col = "red" )
axis(side =4) # creates right y axis
# creates a secondary y-axis on the right side
mtext(side =4, line =3, "False positive rate")
# Adds the label "False Positive rate" to the newly created axis
text(0.18 ,0.18 , "FPR", col = "red") #### CHANGE THE ARGUEMENTS 
text(0.58 ,0.58 , "TPR", col = "blue") ### CHANGE THE ARGUEMENTS


cbind(alpha, tpr,fpr)

# Threshold Performance Curve for Logistic Regression

# Step 1: Extract alpha (threshold), FPR, and TPR values from the ROC object
alpha_LR <- round(as.numeric(unlist(roc_LR@alpha.values)), 4)  # Threshold values
fpr_LR <- round(as.numeric(unlist(roc_LR@x.values)), 4)        # False Positive Rate (FPR)
tpr_LR <- round(as.numeric(unlist(roc_LR@y.values)), 4)        # True Positive Rate (TPR)

# Step 2: Calculate Youden's J statistic for each threshold (Youden's J = TPR - FPR)
youden_LR <- tpr_LR - fpr_LR

# Step 3: Adjust margins for plotting multiple graphs
par(mar = c(5, 5, 2, 5))  # Adjust margins (change if needed)

# Step 4: Plot TPR vs Threshold
plot(alpha_LR, tpr_LR, xlab = "Threshold", ylab = "True Positive Rate (TPR)", 
     xlim = c(0, 1), type = "l", col = "blue", lwd = 2)

# Step 5: Add FPR curve on top of TPR curve
par(new = TRUE)  # Overlay the plot
plot(alpha_LR, fpr_LR, xlab = "", ylab = "", axes = FALSE, 
     xlim = c(0, 1), type = "l", col = "red", lwd = 2)

# Step 6: Add a secondary axis for FPR on the right side
axis(side = 4)  # Create right y-axis for FPR
mtext(side = 4, line = 3, "False Positive Rate (FPR)", col = "red")

# Step 7: Plot Youden's J statistic curve
par(new = TRUE)  # Overlay on top of previous plots
plot(alpha_LR, youden_LR, xlab = "", ylab = "", axes = FALSE, 
     xlim = c(0, 1), type = "l", col = "green", lwd = 2, lty = 2)
axis(side = 4, at = pretty(youden_LR), col.axis = "green", line = 6)  # Add Youden's J axis

# Step 8: Annotate the optimal threshold based on Youden's J
optimal_threshold_LR <- alpha_LR[which.max(youden_LR)]  # Find optimal threshold
cat("Optimal Threshold (Youden's J):", optimal_threshold_LR, "\n")

# Add text annotations to the plot
text(0.18, 0.18, "FPR", col = "red", pos = 4)  # Adjust as needed
text(0.58, 0.58, "TPR", col = "blue", pos = 4)  # Adjust as needed
text(optimal_threshold_LR, max(youden_LR), paste0("Optimal Threshold: ", optimal_threshold_LR),
     col = "green", pos = 4)

# Step 9: Add legend to distinguish curves
legend(x = 0.61, y =0.5, legend = c("TPR", "FPR", "Youden's J"), 
       col = c("blue", "red", "green"), lty = 1:2, lwd = 2)

threshold_data_LR <- cbind(alpha_LR, tpr_LR, fpr_LR); threshold_data_LR
