getwd()
setwd("C:/Users/eugen/OneDrive/Documents/R code/Data")
sales <- read.csv("yearly_sales.csv")
head(sales)
total <- round(sales$sales_total, digit=2)
length(total)
#################### HOW TO GET MULTIPLE GRAPH IN DIAGRAM###########
# fev_female <- data$FEV[which(data$Sex == 0)]
# fev_male <- data$FEV[which(data$Sex == 1)]
# opar <- par(mfrow=c(1,2))
# hist_female <- hist(fev_female, main = "FEV Female", col="red")
# hist_male <- hist(fev_male, main = "FEV MALE", col="blue")
# par(opar) 
#-------------------------- Commands ----------------------------------------
summary(total)
range(total)
var(total)
sd(total)
IQR(total) #spread of middle 50%
cor(total, order) # if number close to 1 means strong association,
# for cor X and Y needs to be numeric
#----------------How to answer correlation qn-----------
# Correlationn is quite high at ________
# it is clear from the plot that there is a strong positive linear assoication between FEV and height
# the range of FEV and height for males appears larger than for females
# the variability of FEV at lower heights seems slightly less than the variability of FEV at greter height

#------------------------ Histogram------------------------------------
hist(x=total,freq=TRUE, main="Histogram of Total Sales",
     xlab="Total", ylab="Frequency", col="blue")
# 1) range
# 2) Bimodal/Unimodal at which number is the mound at
# 3) symmetric/ Skewed (left / right)
# 4) Data has gaps/ is Clustered at which range
# 5) Suspected outliers from which point
#---------------------Histogram with density line-----------------------
hist(total, freq=FALSE, main=paste("Histogram of total sale"),
     xlab="total", ylab = "probability", col="blue", ylim=c(0,0.0045))
lines(density(total), col="red")


# HISTOGRAM WITH NORMAL DENSITY 
hist(total, freq=FALSE, main = paste("Histogram of Total Sales"),
     xlab = "total sales", ylab="Probability", 
     col = "grey", ylim = c(0, 0.002))
n = length(total); n
x <- seq(0, max(total), length.out=n)
y <- dnorm(x, mean(total), sd(total))
lines(x, y, col = "red") # this is the normal density curve
# normal density line helps to check if a hist is normal or not

#--------------------------- Boxplot -----------------------------------------
boxplot(total, xlab= "total ____",col="blue", main="boxplot of __")
# How many outliers boxplot$out
# median
# distribution / normal or skewed
# any visable pattern
################ how to get outliers in boxplot? ###########
# boxplot_fev <-boxplot(fev, main="Boxplot of FEV", pch=20)
# outliers <- boxplot_fev$out ; length(outliers)
# index <- which(fev %in% outliers) ; index #ORDER MATTERS use longer vec as first arg
# data[index,]
# once ^ data[index,] called, comment on it eg. most outliers are male / tall /non-smokers

#------------how to extract data of specific outliers like "old"------------
boxplot <- boxplot(data$age, xlab= "age",col="blue")
length(boxplot$out)
# there are 278 outliers
boxplot$out

head(data)
data$age
outliers_index <- which(data$age %in% boxplot$out)
outliers_data <- data[outliers_index,]
dim(outliers_data)

large_outliers <- outliers_data[which(outliers_data$year == "old"),]
dim(large_outliers)
large_outliers_weight_mean = mean(large_outliers$weight)
large_outliers_weight_mean
length(large_outliers_weight_mean)
#----------------------------QQ plot in R---------------------------------
qqnorm(total, main="QQ Plot", pch=20)
qqline(total, col="red")
# needs to identify where the tails are 
# RIGHT TAIL, sample larger than norm == longer // sample smaller than normal == shorter
# LEFT TAIL, sample larger than norm == shorter // sample smaller than normal == longer

################## eg of answer#######################
# Left tail sample quantiles are larger than expected, hence left tail shorter than normal
# Right tail sample quantiles are larger than expected, right tail longer than normal
# sample IS OR IS NOT normally distributed

# left tail samples quantiles are ___ than expected, hence left tail _____ than normal
# Right tail sample quantiles are ___ than expected, hence right tail ____ than normal

#-------------------------- Scatter plot------------------------------
order = sales$num_of_orders
scatter <- plot(order, total, main="Scatter plot",pch=20, col="blue"); scatter
# Is there any relationship? is it strong?
# If there is, is it positive or negative
# Relationship is linear or non linear
# special observation
# Is the variability of the response stable when X changes
cor(fev, height) 
# Correlationn is quite high at eg 0.868
# it is clear from the plot that there is a strong positive linear assoication between FEV and height
# the range of FEV and height for males appears larger than for females (special obv)
# the variability of FEV at lower heights seems slightly less than the variability of FEV at greter height


############ HOW TO PLOT 2 DIFFERENT CATEGORY ON SAME SCATTER PLOT############
# plot(height,fev, type="n") #type = NULL # gets empty graph then add points on it
# height_test <- height[which(data$Sex==0)] ; height_test # this returns a vector v[5]
# if this is a df, then need to [,]
# points(y = fev_female, x= height[which(data$Sex==0)], col="red",pch=20)
# points(fev_male ~ height[which(data$Sex==1)], col="blue", pch=20)

#legend("topleft", legend = c("light", "dark"), 
       #col= c( "blue", "red"), pch = c(20,20,20))
plot(new$FPR, new$TPR, xlim = c(0,1), ylim = c(0,1), type = "n")
points(y = new$TPR[which(new$delta == 0.3)], x = new$FPR[which(new$delta == 0.3)],col="red", pch=20)
points(y = new$TPR[which(new$delta == 0.6)], x = new$FPR[which(new$delta == 0.6)],col="blue", pch=20)
points(y = new$TPR[which(new$delta == 0.8)], x = new$FPR[which(new$delta == 0.8)],col="green", pch=20)
legend("topright", legend = c("delta = 0.3", "delta = 0.6", "delta = 0.8"), 
       col= c("red", "blue", "green"), pch = c(20,20,20))

#------ HOW TO PLOT A BEST FIT LINE ON A LINEAR MODEL---------------
plot(x=floor_area_sqm, y=log(resale_price), pch=20)
model <- lm(log(resale_price)~ floor_area_sqm)
abline(model,col="red")

#------------------------- Boxplot of multiple groups------------------
gender <- sales$gender
boxplot(total ~ gender, col="blue")


#----------------------Barplot and Pie Chart ------------------------
count = table(gender)
count
barplot(count)
pie(count)
order_size <- ifelse(order<=5, "small", "large")
order_size

contingency_table <- table(gender, order_size)
prop.table(contingency_table) # sum of all number adds to 1
prop.table(contingency_table, "gender") #contingency table of joint proportion
#--------------------------ODD RATIO----------------------------------
# CONTINGENCY TABLE

# The odds ratio is directly calculated from the values in a 2x2 contingency table.
# It quantifies how strongly being in one category (e.g., a smoker) is associated with a particular outcome (e.g., having lung disease).
# If OR = 6, the odds of having lung disease are 6 times higher for smokers compared to non-smokers.

# OR WILL NOT CHANGE EVEN IF THE ORIENTATION OF THE TABLE IS REVERSED (e.g., table(x, y) & table(y, x) yield the same OR)
# Explanation:
# The product of the diagonals (A * D) and (B * C) is the same no matter how the table is arranged.
# This symmetry is what makes the odds ratio invariant to the order of the variables in the table.

# Create a contingency table between two categorical variables (e.g., smoking status and lung cancer)
or_table = table(categorical_1, categorical_2)
print("Contingency Table:")
print(or_table)

########## OR TABLE ##########
#              CATEGORICAL VAR 1
#                (+)     (-)
# Cat var 2 (+)   A       C
# Cat var 2 (-)   B       D
#
# Odds Ratio = A * D / (B * C) = (A / B) / (C / D)

# Calculate the odds ratio
OR = (or_table[1,1] * or_table[2,2]) / (or_table[1,2] * or_table[2,1])
print(paste("Odds Ratio:", OR))

# Interpretation of the odds ratio:
# OR > 1: Positive association (Outcome more likely in the first group).
# OR = 1: No association between variables (odds of the outcome are equal in both groups).
# OR < 1: Negative association (Outcome less likely in the first group).
## The OR of ___ means that ___(exposed group)___ are ___ more likely to develope _____
# compared to ______

#-------------------------------Conclusion----------------------------------------------
# historgram <- 1 quant summary 
# hist(X, freq=TRUE/FALSE, xlab=,ylab=, main="",col="")
# histogram usually overlaid density line, show overall trend. 
# freq = "FALSE" when plotting density line
# density line shows the probability density, while histogram shows freq

# boxplot <- multiple variables
# boxplot(X, xlab="", col="")
# median, lower, upper quantiles, IQR, outliers

# QQplot <- check if sample follow normal dist
# qqnorm(X)
# qqline(X)


# 2 quant variables (eg. height and weight) <-- scatter plot
# plot(X,Y, main=, col=)
# check if:
# 1) linear / quadratic / sin curve
# 2) is the assoication positive or neg
# 3) is the association linear or non  linear
# 4) are there any outliers, if so, how many

# 1 Quant 1 Cat variable (eg. height and gender) <-- boxplots of multiple groups
# boxplot(X ~ Y )
# check if:
# 1) check for their majority of their points
# 2) Check for their outliers
# 3) check if the 2 boxes have significant overlap and size # if significant overlap, no relation
# 4) check if median are similar

# 2 cat variables (cancer and smoking) <-- contingency table and odd ratio
# use a bar plot or pie chart to represent cat variable
# how to produce a freq table? :
# count = table(variable) # produces a summary of how many in each category #freq table
# category with highest freq == modal category
# barplot(count) or pie(count) 
# ^^ display a categorical variable

# contingency tables of frequency  
# table = table(X,Y) X will be in the Row and Y be in the Column// response variable tends to be Y

# contingency table of joint proportion
# prop.table(frequency_table # sum all the variables to get 1
# however this is not very useful, need to distill the information more
# prop.table(table, "gender") # "gender" is the category // each row sums to 1, treat each row as 100%
# odds_off_success = prob of event / prob of not event
# odds_ratio = odds_of_success_case1 / odds_of_success_case2, OR = ad/bc . If table  contains a "0", +0.5 to all values. NO NA ALLOWED

# Assuming you have a data frame with 'gender' and 'order.size' columns

# Sample data
set.seed(123)
gender <- factor(sample(c("M", "F"), 100, replace = TRUE))  # Generate 100 random genders
order.size <- factor(sample(c("large", "small"), 100, replace = TRUE))  # Generate 100 random order sizes

# Create a contingency table
table <- table(gender, order.size) ### CAN CALULATE OR HERE
table
# Generate proportions by row (gender) using prop.table
tab <- prop.table(tab, margin=1)  # 1 indicates row-wise proportions
# CAN CALCULATE OR HERE TOO
# Print the contingency table with proportions by gender
print(tab)
# females are 1.03 times more likely to place a large order than a male 















