library(plotly)
library(ggplot2)
library(ggthemes)
library(caret)
library(glmnet)

df <- read.csv("C:/Users/Christopher/Documents/LASSO Regression/oil.gas.data.csv",
               header = TRUE, na.strings = "NA", check.names = FALSE)
colnames(df) = c("Date","Brent.US","Jet", "D.Gulf", "G.Gulf", "Propane.Tex", "Gas", "Brent.Europe",
                 "Heating.NY", "D.NY", "G.LA", "D.LA", "G.NY")
df = df[,-1]  #removes the date column
df <- na.omit(df)#Omits NA
# <- data.frame(scale(df))

training <- df[1:1200, ] #Create training set of first 900 days in data
training <- data.frame(scale(training))
testing <- df[1201:1300,] #Create testing set of last 300 days in data
testing <- data.frame(scale(testing))

#Create prediction results data.frame
pred.train <- data.frame(matrix(ncol = 1, nrow = length(training[,1])))
pred.train[,1] <- training$G.Gulf #set the first column of prediction to the actual values.
colnames(pred.train) = "Actual"

pred.test <- data.frame(matrix(ncol = 1, nrow = length(testing[,1])))
pred.test[,1] <- testing$G.Gulf
colnames(pred.test) = "Actual"

coefs <- data.frame(matrix(ncol = 4, nrow = length(training)))
colnames(coefs) = c("ols","ridge","lasso", "adaptive")
#Normal Regression
ols_lm <- lm(G.Gulf ~., data = training)
#Coefficients for ols_lm
coefs[,1] <- ols_lm$coefficients



x <- as.matrix(training[,-4]) 
y <- as.double(as.matrix(training$G.Gulf)) # Only class

#RIDGE
ridge_lm <- cv.glmnet(x, y, alpha=0, type.measure='mse', nfolds = 100)
coefs[,2] <- as.matrix(coef(ridge_lm, s=ridge_lm$lambda.1se))

#LASSO
lasso_lm <- cv.glmnet(x, y, alpha=1, type.measure='mse', nfolds = 100)
coefs[,3] <- as.matrix(coef(lasso_lm, s=lasso_lm$lambda.1se))

#ADAPTIVE LASSO
coefs[1,] = 0
w = 1/(abs(coefs$ridge)^.5) #This is gamma, I have it set to 1. can/should be tuned.
w = w[2:length(w),]
adaptive_lm <- cv.glmnet(x,y,alpha = 1, penalty.factor = w, nfolds = 100)
coefs[,4] <- as.matrix(coef(adaptive_lm, s = adaptive_lm$lambda.1se))
coefs[1,] = 0

#PREDICTING: CONVENTIONAL GASOLINE

###TRAINING###
newdata=as.matrix(training[,-4]) #removes G.Gulf
colnames(newdata) = c("Brent.US","Jet", "D.Gulf", "Propane.Tex", "Gas", "Brent.Europe",
                      "Heating.NY", "D.NY", "G.LA", "D.LA", "G.NY")
pred.train$ols <- predict(ols_lm)
pred.train$ridge <- predict(ridge_lm, newx = newdata, s = "lambda.1se")
pred.train$lasso <- predict(lasso_lm, newx = newdata, s = "lambda.1se")
pred.train$adaptive <- predict(adaptive_lm, newx = newdata, s = "lambda.1se")


###TESTING###
newdata=as.matrix(testing[,-4]) #removes G.Gulf
colnames(newdata) = c("Brent.US","Jet", "D.Gulf", "Propane.Tex", "Gas", "Brent.Europe",
                      "Heating.NY", "D.NY", "G.LA", "D.LA", "G.NY")
pred.test$ols <- predict(ols_lm, data.frame(newdata))
pred.test$ridge <- predict(ridge_lm, newx = newdata, s = "lambda.1se")
pred.test$lasso <- predict(lasso_lm, newx = newdata, s = "lambda.1se") 
pred.test$adaptive <- predict(adaptive_lm, newx = newdata, s = "lambda.1se")

#-----#-----#-----REGRESSION PERFORMANCE-----#-----#-----#

#TRAINING
RSS.train = matrix(nrow = length(coefs), ncol = 1)
MSE.train = matrix(nrow = length(coefs), ncol = 1)
for(i in 1:4){
  RSS.train[i] = sum((pred.train$Actual - pred.train[,i+1])^2)
  MSE.train[i]= RSS.train[i]/length(training$Brent.US)
}
#TESTING
RSS.test = matrix(nrow = length(coefs), ncol = 1)
MSE.test = matrix(nrow = length(coefs), ncol = 1)
for(i in 1:4){
  RSS.test[i] = sum((pred.test$Actual - pred.test[,i+1])^2)
  MSE.test[i]= RSS.test[i]/length(testing$Brent.US)
}


# require(broom)
# tidy(ridge_lm)



##Make the above stats into a nicer table  
# rp1 <- data.frame(RMSE = c(RMSE.train.E, RMSE.train.I), RSS = c(RSS.train.E, RSS.train.I),
#                   ESS = c(ESS.train.E, ESS.train.I),
#                   "r-squared" = c(ESS.train.E/(ESS.train.E+RSS.train.E), ESS.train.I/(ESS.train.I+RSS.train.I)))
# rownames(rp1) <- c("Conv. Gas Price", "Heating Oil Price")
# 
# 
# 
# rp2 <- data.frame(RMSE = c(RMSE.test.E, RMSE.test.I), RSS = c(RSS.test.E, RSS.test.I),
#                   ESS = c(ESS.test.E, ESS.test.I),
#                   "r-squared" = c(ESS.test.E/(ESS.test.E+RSS.test.E), ESS.test.I/(ESS.test.I+RSS.test.I)))
# rownames(rp2) <- c("Conv. Gas Price", "Heating Oil Price")






#Create my non-linear regression model, named poly2_lm. raw = TRUE uses raw and not 
#orthogonal polynomials. I didn't investigate the difference, only that Raw
#gave terribly bad predictions
# poly2_lm <- lm(data = training, G.Gulf ~ polym(Brent.US,Jet, D.Gulf, Propane.Tex, Gas, Brent.Europe,
#                                                Heating.NY, D.NY, G.LA, D.LA, G.NY,degree=2, raw=TRUE))
#use predict to find predicted values of 'fit'
# training$Predict <- predict(poly2_lm)
# testing$Predict <- predict(poly2_lm, newdata = testing)

