
# fitted / predicted value (by model) ,, responed value (raw data)
# LINEAR MODEL: yhat = B0hat (estimate intercept) + B1hat(estimate slope)X
#--------------ASSUMPTIONS FOR A RESPONSE IN A LINEAR MODEL---------
# 1) quantative
# 2) symmetric (use hist to check) (transformation to make response be symmetric)
# how to transform??
# instead of y~x use log(y) ~ x (actually mean ln(e))
# need to transform back,, if used log then need use exp()
# 3) variability of the response should be stable when independent variable change (scatter plot) if upper and lower bound lines // means variability low
# ^^ and/or make varaibility more stable when x changes

#-------------EVAULATING THE GOODNESS OF FIT--------------------------
# 1) R**2  (use to justify goodness of fit)
# 2) F test (use p value to justify significance)
## good fit & simple use both to justify as long respond is same. 
# for comparing models ->> use adjusted R**2, higher adjustable R**2 is better

#-------------Check for as.factor error using coefficiet------------------------
# If a categorical variable has K categories then a model will need (k-1) coefficient
# for a cate variable, we need indicator to represent it in a model
# weight ~ height + gender
# gender needs to be represented by an indicator eg F = 0, M = 1
# W = 5 + 10 * 1.8 + 15* I(gender = M) # 15 is the unit,  coefficient. eg for the same height of 2 gender, M is 15units taller than F
#---------------------------------------------------------------------------------
?lm()
getwd()
setwd("C:/Users/eugen/OneDrive/Documents/R code/Data")
resale <- read.csv("C:/Users/eugen/OneDrive/Documents/R code/Data/hdbresale_reg.csv")
attach(resale)
head(resale)
block <- as.factor(block) # make it a categorical ## BUT THIS DOES NOT CHANGE THE DF
head(resale[,2:7]) #id not impt hence
x <- c(-1, 3 ,5)
y <- c(-1, 3.5, 3)
lm(y~x) # first variable is response, 2nd is independent
# ~ means "is dependent" "y is dependent on x"
# "hat" is given for estimate
# B0hat is the intercept hence 0.1250
# B1hat is the slope hence 0.7321
# fitted model:
# y hat = 0.125 + 0.7321x
?predict()
args(predict.lm)
M <- lm(y ~ x)
summary(M)
summary(M)$r.squared
df <- data.frame(x=2) # if want more variable use a vector
predict(M, newdata=df) # accepts a ONLY A dataframe
head(resale[,2:7])
head(resale)
price <- resale$resale_price
area <- resale$floor_area_sqm

model <- lm(price ~ area) ; model
summary(model)
summary(model)$r.squared # use this to justify goodness of fit

#-------------Predicting a data frame-------------------------
M1 <- lm(resale_price ~ floor_area_sqm, data = resale) ; summary(M1)
M1$fitted.values # all the predicted response for the data points used to fit the model
resale$floor_area_sqm
new = data.frame(floor_area_sqm = c(74,74,73)) ; new
predict(M1, newdata = new) #predict base on what model and what df 
#NAME OF INDEPENDENT VARIABLE MUST BE THE SAME AS DF IN PREDICT ^^
# respond variable infront, # independent behind # what arguement in df
another_model <- lm(resale_price ~ floor_area_sqm)

new <- data.frame(width = 27,spine = "3") # note categorical so "3"


#--------------------MULTIPLE LINEAR REGRESSION-------------------------------
M2 <- lm(resale_price ~ floor_area_sqm + flat_type, data=resale)
head(resale)
resale$flat_type <- as.factor(resale$flat_type)
class(resale$flat_type)
levels(as.factor(resale$flat_type))
summary(M2) # note that when run, only 4 coefficient, data got 5 category. 2 rm flat is taken as reference

#------------------------Coefficient table---------------------
# 1) intercept
# 2) floor_area_sqm is quant hence + 1 coefficent
# 3) since flat_type has 5 cate, (5-1) coefficent, total is 6
summary(M2)
summary(M2)$adj.r.squared
summary(M1)
summary(M1)$adj.r.squared
# how does indicator work: for eg race => 2 indicator
# I1 = {1 if chinese, 0 otherwise} => I2 = {1 if Malay, 0 otherwise}
# as such, chinese: I1=1, I2=0; Malay: I1=0, I2=1; Indian:I1=0, I2=0
# here, default indicator or category without any indicator is chosen as indian
## In case below: W^ = 5 + 10H, 15G, '5' Indian which is the reference

#---------------How to answer coefficient questions and interpret--------------
# - take note of y and x is there are any transformation like log
# - exp() function useful to convert log into actual data
# - check if multiple independent X variable
# 1) The coefficient is _______
# 2) meaning when comparing 2 ___ of the same type 
# 3) an increase of 1 ____ will increase the predicted  ___(y variable)__ by 
# ___(coefficient)_____
# 4) equivalently, the price will increase by _____ = _____ (if there is transformation)
# 
#---------------How to answer R^2 questions and interpret-------------
# - assuming model have R^2 of 0.712
# 1) That means model can explain 71.2% of the variability of the response in the sample


#-------------------lm functions and key template------------------
# model$fitted.values
# To predict using model
# pred.data <- data.frame()
# predict(M1, newdata = pred.data)
# qn "interpret the coefficient of height"
# FEV = -5.39103 + 5.12889*height + 0.12313*I(sex=1)
# qn ^ if we want to interpret coefficient of quant variable eg height
# If the other regressors are fixed, when HEIGHT increases by 1 unit
# the predicted FEV increases by 5.12889 units
# qn ^ interpret coefficient of categorical variable eg SEX
# If the other regressors are fixed, the predicted FEV of a male
# is 0.12313 units larger than that of a female








