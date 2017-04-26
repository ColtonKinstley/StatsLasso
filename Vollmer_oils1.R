# Christopher Vollmer
# Project 3 Part 1 - Linear Regression
# Dr. Azencott- Spring 2016- Machine Learning

df <- read.csv("C:/Users/Christopher/Documents/Project_3_copied 08082016/oil.gas.data.csv",
                 header = TRUE, na.strings = "NA", check.names = FALSE)
colnames(df) = c("Date","B","Kerosene", "Diesel Fuel GC", "E", "F", "G", "H",
                 "I", "Diesel Fuel NY", "Blendstock", "Diesel Fuel LA", "Gas NY")
df$Date <- seq(1,nrow(df),1)


training <- df[1:1000, c('B', 'E', 'F', 'G', 'H', 'I')] #Create training set of first 1000 days in data
training <- na.omit(training) #Omits NA
training$Date <- seq(1,nrow(training),1)


testing <- df[1001:1411,c('B', 'E', 'F', 'G', 'H', 'I')] #Create testing set of last 411 days in data
testing <- na.omit(testing) #Omits NA
testing$Date <- seq(nrow(training)+1, nrow(training)+nrow(testing),1)

library(plotly)
library(ggplot2)
library(ggthemes)

###Graphing the Price of Conventional Gas Price Versus time

##Training Set
ggplot(data = training, aes(x = Date)) +
  geom_line(aes(y=E), size = 1) + ylab("Conventional Gas Price") + xlab("Day") +
  ggtitle("Conventional Gas Price for training set")
##Testing Set
ggplot(data = testing, aes(x = Date)) +
  geom_line(aes(y=E), size = 1) + ylab("Conventional Gas Price") + xlab("Day") +
  ggtitle("Conventional Gas Price for testing set")
##Entire Set
tempdf <- data.frame(Date = c(training$Date,testing$Date), E = c(training$E, testing$E))
ggplot(data = tempdf, aes(x = Date)) +
  geom_line(aes(y=E), size = 1) + ylab("Conventional Gas Price") + xlab("Day") +
  geom_vline(xintercept = 952, size = 1, linetype = 2) +
  geom_text(x = 952, y = 2, label = "Test data --->", hjust = 1.1, size = 5) +
  ggtitle("Conventional Gas Price for entire set")

###Graphing the Price of Heating Oil Price Versus time

##Training Set
ggplot(data = training, aes(x = Date)) +
  geom_line(aes(y=I), size = 1) + ylab("Heating Oil Price") + xlab("Day") +
  ggtitle("Heating Oil Price for training set")
##Testing Set
ggplot(data = testing, aes(x = Date)) +
  geom_line(aes(y=I), size = 1) + ylab("Heating Oil Price") + xlab("Day") +
  ggtitle("Heating Oil Price for testing set")
##Entire Set
tempdf <- data.frame(Date = c(training$Date,testing$Date), I = c(training$I, testing$I))
ggplot(data = tempdf, aes(x = Date)) +
  geom_line(aes(y=I), size = 1) + ylab("Heating Oil Price") + xlab("Day") +
  geom_vline(xintercept = 952, size = 1, linetype = 2) +
  geom_text(x = 952, y = 2, label = "Test data --->", hjust = 1.1, size = 5) +
  ggtitle("Heating Oil Price for entire set")

#---------------------------------------------------------------------------------------------------------#
#----------------------------------CONVENTIONAL GAS PRICES vs EXPLANATORY VARS----------------------------#
#---------------------------------------------------------------------------------------------------------#

#Plotting Crude Oil Price: West Texas versus Conventional Gas Price.
pp <- ggplot(data = training, aes(x = Date,y = `E`))
pp + geom_point() + stat_smooth(method = "lm") + xlab("Crude Oil Prices: West Texas") +
  ggtitle(expression(atop("Conventional Gas Price vs. Crude Oil: West Texas",
                          "Linear Regression")))#expression(atop("label1", "label2"))--good way to subtitle

#Plotting Propane Price: Mont Belvieu, Texas versus Conventional Gas Price.
pp <- ggplot(data = training, aes(x = `F`,y = `E`))
pp + geom_point() + stat_smooth(method = "lm") + xlab("Propane Prices: Mont Belvieu, Texas") +
  ggtitle(expression(atop("Conventional Gas Price vs. Propane Prices: Mont Belvieu, Texas",
                          "Linear Regression")))

#Plotting Henry Hub Natural Gas Spot Price versus Conventional Gas Price.
pp <- ggplot(data = training, aes(x = `G`,y = `E`))
pp + geom_point() + stat_smooth(method = "lm") + xlab("Henry Hub Natural Gas Spot Price") +
  ggtitle(expression(atop("Conventional Gas Price vs. Henry Hub Natural Gas Spot Price",
                          "Linear Regression")))

#Plotting Crude Oil Price: Brent Europe versus Conventional Gas Price.
pp <- ggplot(data = training, aes(x = `H`,y = `E`))
pp + geom_point() + stat_smooth(method = "lm") + xlab("Crude Oil Prices: Brent Europe") +
  ggtitle(expression(atop("Conventional Gas Price vs. Crude Oil Prices: Brent Europe",
                          "Linear Regression")))

#---------------------------------------------------------------------------------------------------------#
#-----------------------------------HEATING OIL PRICE vs EXPLANATORY VARS---------------------------------#
#---------------------------------------------------------------------------------------------------------#


#Plotting Crude Oil Price: West Texas versus Heating Oil Price.
pp <- ggplot(data = training, aes(x = `B`,y = `I`))
pp + geom_point() + stat_smooth(method = "lm") + xlab("Crude Oil Prices: West Texas") +
  ggtitle(expression(atop("Heating Oil Price vs. Crude Oil: West Texas",
                          "Linear Regression")))

#Plotting Propane Price: Mont Belvieu, Texas versus Heating Oil Price.
pp <- ggplot(data = training, aes(x = `F`,y = `I`))
pp + geom_point() + stat_smooth(method = "lm") + xlab("Propane Prices: Mont Belvieu, Texas") +
  ggtitle(expression(atop("Heating Oil Price vs. Propane Prices: Mont Belvieu, Texas",
                          "Linear Regression")))

#Plotting Henry Hub Natural Gas Spot Price versus Heating Oil Price.
pp <- ggplot(data = training, aes(x = `G`,y = `I`))
pp + geom_point() + stat_smooth(method = "lm") + xlab("Henry Hub Natural Gas Spot Price") +
  ggtitle(expression(atop("Heating Oil Price vs. Henry Hub Natural Gas Spot Price",
                          "Linear Regression")))

#Plotting Crude Oil Price: Brent Europe versus Heating Oil Price.
pp <- ggplot(data = training, aes(x = `H`,y = `I`))
pp + geom_point() + stat_smooth(method = "lm") + xlab("Crude Oil Prices: Brent Europe") +
  ggtitle(expression(atop("Heating Oil Price vs. Crude Oil Prices: Brent Europe",
                          "Linear Regression")))


#Perform MULTIVARIATE LINEAR REGRESSION: Labeled En
#Response Variable: Es: U.S. Gulf Coast, Regular
#Explanatory Variables: Crude Oil Prices: West Texas
#"-------------------": Propane Prices: Mont Belvieu, Texas
#"-------------------": Henry Hub Natural Gas Spot Price
#"-------------------": Crude Oil Prices: Brent - Europe

En_lm <- lm(E ~ B + F + G + H, data = training)
training$predYn_En<-  predict(En_lm)
 
#----------------------------------------#

#Perform MULTIVARIATE LINEAR REGRESSION: Labeled In
#Response Variable: I
#Explanatory Variables: Crude Oil Prices: West Texas
#"-------------------": Propane Prices: Mont Belvieu, Texas
#"-------------------": Henry Hub Natural Gas Spot Price
#"-------------------": Crude Oil Prices: Brent - Europe

In_lm <- lm(I ~ B + F + G + H, data = training)
training$predYn_In<-  predict(In_lm)
#----------------------------------------#

#Coefficients for En and In#
  tempdf <-data.frame(Coefficients = En_lm$coefficients)
  View(tempdf)
  tempdf <-data.frame(Coefficients = In_lm$coefficients)
  View(tempdf)


#PREDICTING: CONVENTIONAL GASOLINE
  newdata=data.frame(testing$B, testing$F, testing$G, testing$H)
  colnames(newdata) = c("B","F","G","H")
  testing$predYn_En <- predict(En_lm, newdata)
#PREDICTING: HEATING OIL
  testing$predYn_In <- predict(In_lm, newdata)

  
#REGRESSION PERFORMANCE
  
  #TRAINING
  STDtrain.E = sd(training$E - training$predYn_En)
  STDtrain.I = sd(training$I - training$predYn_In)
  RMSE.train.E = STDtrain.E/mean(training$E)
  RMSE.train.I = STDtrain.I/mean(training$I)
  
  RSS.train.E = sum((training$E - training$predYn_En)^2)
  RSS.train.I = sum((training$I - training$predYn_In)^2)
  
  ESS.train.E = sum((training$predYn_En - mean(training$E))^2)
  ESS.train.I = sum((training$predYn_In - mean(training$I))^2)
  
  #TESTING
  STDtest.E = sd(testing$E - testing$predYn_En)
  STDtest.I = sd(testing$I - testing$predYn_In)
  RMSE.test.E = STDtest.E/mean(training$E)
  RMSE.test.I = STDtest.I/mean(training$I)
  
  RSS.test.E = sum((testing$E - testing$predYn_En)^2)
  RSS.test.I = sum((testing$I - testing$predYn_In)^2)
  
  ESS.test.E = sum((testing$predYn_En - mean(testing$E))^2)
  ESS.test.I = sum((testing$predYn_In - mean(testing$I))^2)
  
  
##Make the above stats into a nicer table  
rp1 <- data.frame(RMSE = c(RMSE.train.E, RMSE.train.I), RSS = c(RSS.train.E, RSS.train.I),
                    ESS = c(ESS.train.E, ESS.train.I),
                    "r-squared" = c(ESS.train.E/(ESS.train.E+RSS.train.E), ESS.train.I/(ESS.train.I+RSS.train.I)))
rownames(rp1) <- c("Conv. Gas Price", "Heating Oil Price")
  
  
  
rp2 <- data.frame(RMSE = c(RMSE.test.E, RMSE.test.I), RSS = c(RSS.test.E, RSS.test.I),
                    ESS = c(ESS.test.E, ESS.test.I),
                    "r-squared" = c(ESS.test.E/(ESS.test.E+RSS.test.E), ESS.test.I/(ESS.test.I+RSS.test.I)))
rownames(rp2) <- c("Conv. Gas Price", "Heating Oil Price")
  