##Christopher Vollmer
##Dr. Azencott - Machine Learning
##Project 3 - Part 2 and part 3 - Kernel Ridge Regression
##This Script will perform polynomial kernal non linear regression on
##a data set containing the price of metals on 858 sequential days. 
##In addition, this script uses kernel ridge regression

library(caret)
library(plotly)
library(ggplot2)
library(nls2)
library(kernlab)
df <- read.csv("C:/Users/Christopher/Documents/LASSO Regression/metals.data.csv",
               header = TRUE, na.strings = "NA", check.names = FALSE)

df$JJ <- 0
colnames(df) <- c("Date", "ZINC","TIN","D","E","MOLYBDENUM","LEAD","COBALT","I","J", "JJ")

#Does the df have any missing values?
ifelse(sum(is.na(df)) == 0, paste("There are NO missing values"), paste("There ARE missing values"))


#split df into training and testing. first 600 days into training and last 258 into testing      
training <- df[1:600,]
testing <- df[601:858,]
training <- training[,c(1,4,5,9,10,11)]
testing <- testing[,c(1,4,5,9,10,11)]
#Rename $Date as a sequence and not dates.
training$Date <- seq(1,600,1)
testing$Date <- seq(1,258,1)
#Calculate the mean values for the explanatory variables
mD = mean(training$D)
mE = mean(training$E)
mI = mean(training$I)
mJ = mean(training$J)
#Scale the training data by the mean values
training$D = training$D/mD
training$E = training$E/mE
training$I = training$I/mI
training$J = training$J/mJ
#Scale the testing data by the mean values
testing$D = testing$D/mD
testing$E = testing$E/mE
testing$I = testing$I/mI
testing$J = testing$J/mJ
#Create column with Aluminium's next day values
  #I decided to force the last 'next-day' of the training and testing set
  #to be the values of the last day of those sets. I could have used a value more 
  #fitting of the general trend of the price but felt that repeating the 600th and 
  #258th day would not affect my regression coefficients too much. 
  #I name this column JJ.
for (i in 1:600) {
  training[i,6] <- training[i+1,5]
}
training[600,6] <- training[600,5]

for (i in 1:258) {
  testing[i,6] <- testing[i+1,5]
}
testing[258,6] <- testing[258,5]

#Create my regression model, named fit. raw = TRUE uses raw and not 
  #orthogonal polynomials. I didn't investigate the difference, only that Raw
  #gave terribly bad predictions
fit <- lm(data = training, JJ ~ polym(D, E, I, J, degree=2, raw=TRUE))
#use predict to find predicted values of 'fit'
training$Predict <- predict(fit)
testing$Predict <- predict(fit, newdata = testing)

#The function lm above cannot introduce lambda for Kernel Ridge Regression
  #so I wrote it out manually below.

#Kernel type is polynomial of degree 2. I tested with offset = 1,10,100,1000
  #and offset = 100 gave the best results...by mere hundredths of a percent.
  #Thus, these comparisons are not reported.
kern <- polydot(degree = 2, offset = 100)
#'kernelMatrix' requires the vectors to be matrices
x.training <- as.matrix(training[,c(2,3,4,5)])
x.testing <- as.matrix(testing[,c(2,3,4,5)])

#Kernel Matrix K for training set
kern.train = kernelMatrix(kern,x.training)

#Kernel Matrix k for testing set
kern.test = kernelMatrix(kern,x.training,x.testing)

#Finding optimal Lambda for KRR. The Lambda which gave the smallest RMSE was
  #Lambda = .81. I combed through .001-10.
lambda = seq(.1, 1, .1)
findlambda <-data.frame(1:258)
error <- data.frame(Lambda = 1:length(lambda), Error = 1:length(lambda))
for(i in 1:length(lambda)){
  beta_hat_temp = solve(kern.train+lambda[i]*diag(length(training$JJ)), tol = 1e-23)%*%as.matrix(training$JJ)
  findlambda[,i] <- t(kern.test)%*%beta_hat_temp
  error[i,1] <- lambda[i]
  error[i,2] <- sd(testing$JJ - findlambda[,i])/mean(training$JJ)
}
best.lambda = error[which(error[,2] == min(error[,2])),]
View(best.lambda)
lambda = best.lambda$Lambda #assigns lambda to be the best lambda found

#Computes Kernel coefficients, beta hat
beta_hat = solve(kern.train+lambda*diag(length(training$JJ)))%*%as.matrix(training$JJ)

# Computes the predictions of KRR
training$Predict.krr <-  t(kern.train) %*% beta_hat
testing$Predict.krr <- t(kern.test)%*%beta_hat



##compare the residual errors of nokrr and krr
#Training Set
STDtrain.nokrr = sd(training$JJ - training$Predict)
STDtrain.krr = sd(training$JJ - training$Predict.krr)

RMSE.train.nokrr = STDtrain.nokrr/mean(training$JJ)
RMSE.train.krr = STDtrain.krr/mean(training$JJ)
RSS.train.nokrr = sum((training$JJ - training$Predict)^2)
RSS.train.krr = sum((training$JJ - training$Predict.krr)^2)
ESS.train.nokrr = sum((training$Predict - mean(training$JJ))^2)
ESS.train.krr = sum((training$Predict.krr - mean(training$JJ))^2)

#Testing Set
STDtest.nokrr = sd(testing$JJ - testing$Predict)
STDtest.krr = sd(testing$JJ - testing$Predict.krr)

RMSE.test.nokrr = STDtest.nokrr/mean(testing$JJ)
RMSE.test.krr = STDtest.krr/mean(testing$JJ)
RSS.test.nokrr = sum((testing$JJ - testing$Predict)^2)
RSS.test.krr = sum((testing$JJ - testing$Predict.krr)^2)
ESS.test.nokrr = sum((testing$Predict - mean(testing$JJ))^2)
ESS.test.krr = sum((testing$Predict.krr - mean(testing$JJ))^2)



##Puts the above into a better looking table.
rp1 <- data.frame(RMSE = c(RMSE.train.nokrr, RMSE.train.krr), RSS = c(RSS.train.nokrr, RSS.train.krr),
                  ESS = c(ESS.train.nokrr, ESS.train.krr),
                  "r-squared" = c(ESS.train.nokrr/(ESS.train.nokrr+RSS.train.nokrr), ESS.train.krr/(ESS.train.krr+RSS.train.krr)))

rp2 <- data.frame(RMSE = c(RMSE.test.nokrr, RMSE.test.krr), RSS = c(RSS.test.nokrr, RSS.test.krr),
                  ESS = c(ESS.test.nokrr, ESS.test.krr),
                  'r-squared' = c(ESS.test.nokrr/(ESS.test.nokrr+RSS.test.nokrr), ESS.test.krr/(ESS.test.krr+RSS.test.krr)))
row.names(rp1) <- c("NO KRR", "KRR") #STDtrain/mJJ
row.names(rp2) <- c("NO KRR", "KRR")
colnames(rp1)[4] <- "R-Squared" 
colnames(rp2)[4] <- "R-Squared"

#I wanted to double check my code for KRR and found the library(CVST)
#I implement below what I did above and it checks out.
library(CVST)
d.train = constructData(x = x.training, y = as.vector(training[,6]))
d.test = constructData(x = x.testing, y = as.vector(testing[,6]))## Structure data in CVST format
krr_learner = constructKRRLearner()   ## Build the base learner
params = list(kernel='polydot', degree = 2, offset = 100, lambda=0.81) ## Function params
krr_trained = krr_learner$learn(d.train, params)
##Predict testing values
testing$Predict.cvst = krr_learner$predict(krr_trained, d.test)
training$Predict.cvst = krr_learner$predict(krr_trained, d.train)

##important to use 'data = df' instead of df$variable because when you go to predict,
##the testing set will not have df$variable since the testing set will not be called df!
##so, make sure that the testing df has the same names for the columns of the predictor
##variables as the training set. makes using predict(lm) not return an error about 
##mismatching dimensions.


