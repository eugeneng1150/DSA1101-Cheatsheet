# LOGSTIRC REGRESSION ONLY CAN USE 2 OUTCOMES EITHER YES OR NO
# FITTED MODEL:
## log(p hat / (1-phat)) = 3.5 + 0.25 A + 0.382C
## but we always give answer as odd and not log odd eg:
### At the same age, customer with church contacts by 1 will have odds off churning increase
### by e^0.382 times (REMEMBER IT IS TIMES AND NOT PLUS)
library(ROCR)
data = read.csv("churn.csv")
head(data)

data$Churned = as.factor(data$Churned)
data$Married = as.factor(data$Married)
data= data[,-1] #Remove ID column

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


# HOW TO PLOT ROC CURVE FOR LOGISTIC MODEL
# 1) use predict() to generate predicted probabilities from your logistic model
### type = "response"
# these are probabilities of the positive case
prob  = predict(M3, type = "response") #response because get probability of positive case
# only for LR
# 2) prediction() to create pred object, pass predicted prob and true class labels
pred = prediction(prob, Churned)

# 3) performance() to compute performance metrics like ROC or AUC
roc = performance(pred, "tpr", "fpr")
auc = performance(pred , measure ="auc")
# higher AUC better
auc@y.values[[1]]
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))
# ROC AND AUC based on changing threshold values


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

# 1) Extract alpha (threshold), FPR, and TPR values from roc object
alpha <- round(as.numeric(unlist(roc@alpha.values)), 4)  # Thresholds
fpr <- round(as.numeric(unlist(roc@x.values)), 4)        # FPR values
tpr <- round(as.numeric(unlist(roc@y.values)), 4)        # TPR values

# 2) Calculate G-Mean for each threshold
gmean <- sqrt(tpr * (1 - fpr))  # G-Mean = sqrt(TPR * TNR)

# 3) Adjust margins for plotting multiple graphs
par(mar = c(5, 5, 2, 5))  # Adjust the margins (change if needed)

# 4) Plot TPR vs Threshold
plot(alpha, tpr, xlab = "Threshold", ylab = "True Positive Rate (TPR)", 
     xlim = c(0, 1), type = "l", col = "blue", lwd = 2)

# 5) Add FPR curve on top of TPR curve
par(new = TRUE)  # Overlay the plot
plot(alpha, fpr, xlab = "", ylab = "", axes = FALSE, 
     xlim = c(0, 1), type = "l", col = "red", lwd = 2)

# 6) Add a secondary axis for FPR on the right side
axis(side = 4)  # Create right y-axis
mtext(side = 4, line = 3, "False Positive Rate (FPR)", col = "red")

# 7) Plot the G-Mean curve
par(new = TRUE)  # Overlay on top of previous plots
plot(alpha, gmean, xlab = "", ylab = "", axes = FALSE, 
     xlim = c(0, 1), type = "l", col = "green", lwd = 2, lty = 2)
axis(side = 4, at = pretty(gmean), col.axis = "green", line = 6)  # Add G-Mean axis

# 8) Annotate the optimal threshold based on G-Mean
optimal_threshold <- alpha[which.max(gmean)]  # Find the optimal threshold
cat("Optimal Threshold (G-Mean):", optimal_threshold, "\n")

# Add text annotations to the plot
text(0.18, 0.18, "FPR", col = "red", pos = 4)  # Adjust as needed
text(0.58, 0.58, "TPR", col = "blue", pos = 4)  # Adjust as needed
text(optimal_threshold, max(gmean), paste0("Optimal Threshold: ", optimal_threshold),
     col = "green", pos = 4)

# 9) Add legend to distinguish curves ## OPTIONAL
legend("topright", legend = c("TPR", "FPR", "G-Mean"), 
       col = c("blue", "red", "green"), lty = 1:2, lwd = 2)


