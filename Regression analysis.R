
#LR - finds the best linear relationship between an independent variable and a dependent variable
# Minimum mean error
#Y has normal distribution

library(ggplot2)

#Read file
toyota.df <- read.csv("toyotacorolla.csv", stringsAsFactors = TRUE)
head(toyota.df)

### split the data into training and validation
set.seed(1)  
train.index <- sample(1:dim(toyota.df)[1], dim(toyota.df)[1]*0.6)  #dimention - number of rows, number of columns
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[-train.index, ]

#Prediction of the relationhip between car's price and weight
###Examine the relationship between Price ~ Weight
# viz
p <- ggplot(train.df, aes(y = Price, x = Weight)) + 
  geom_point() 
p

# simple regression 
reg <- lm(Price ~ Weight, data = train.df)
summary(reg)

# viz 
p + geom_smooth(method = lm)

### predict
# fit on training
pred.train <- predict(reg)

# predict and evaluate
library(forecast)
pred.train <- predict(reg)
accuracy(pred.train, train.df$Price)

pred.valid <- predict(reg, newdata = valid.df)
accuracy(pred.valid, valid.df$Price)

## Multiple LR

## convert to factor
class(toyota.df$Fuel_Type)
ggplot(train.df, aes(x = Doors, y = Price)) + 
  geom_bar(stat = "summary", fun = "mean")
#The relationship isn't linear

### split the data
set.seed(1)  
train.index <- sample(1:dim(toyota.df)[1], dim(toyota.df)[1]*0.6)  
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[-train.index, ]


## multiple regression 
reg <- lm(Price ~ ., data = train.df[, -1]) 
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAkCAYAAAD7PHgWAAACSklEQVR42u3YzW/SYBwH8Gm8eNz/4Wl/gBf/AA/ejWf15GFXbupwyCilhYHAxnCsg9TVUloKLVDJiEYTY4xxvmxLdTJkvGQST+bn8zRxEScHWro1S7/JN3Cinzzp88bUlBs3btwMhde0acfiJEWjFG0T8Kek1i85DigqNcB5/3Eb8HepXL3sKGChVDWAnW4fPnzaAbFcA1GuXXUEzuPxnBeK6hEQ9/OODhgtyMr1E0GoqnqB57XpUS2UKkNA3F19z0ByBeXmxCBcXr63Icg9VPi7XKEEeJRGtahox4C4+temgdwQSrMTAbK81AML+ReIu9dsoXeyCmxe8lgG5riCAez2+v99mNk299toCapBlhPvWwIyT/hfo0bCalvtDsgVDRg27zMNzOQ4sAuI2z7oQrlaB/QcwhQwvc7aCsQ96PSg8qwBK+tsiGGYi2MBU5mc7cA/rTdeQCqTvTMWMPmYOREgHsVqvQHJ1bVbYwHjqYztwDbCFdGMTqyskXgXGgsYXUrbCvyOZvJTUYbocjpgapJEEinbgPutNuQ4ASLx5XnTywwVS9oCxLvJapYF9Pt3LS3U5GJ84kD9yzdYQpMvFE1Y3+oIOkYR4RiYaa9/eAy3vatDDL3XQTo2e6pnQj+1CIPBzyHcFjpdU4+SsEBHbp/6ofUhGR4Cvn23BUE0qr4QfcMRp+p5goLDHwMD9+r1G/CHwuAL0tcccyfxBkIGbvP5S3iAsHMB8oqjLk1eP0F5F0iY8wfB6ydnHHk3xtsWWqHOuX9huDkr+Q3pRAm21Sk4jQAAAABJRU5ErkJggg==summary(reg)

## interactions
ggplot(train.df, aes(y = Price, x = Weight, color = Fuel_Type)) + 
  geom_point() + 
  xlim(c(1000, 1400))
#Cars with diesel has the higher weight and price

reg2 <- lm(Price ~ .^2, data = train.df[, -1]) 
summary(reg2)

### stepwise
#start with empty model, at each step:
# Add predictors by some criteria (forward selection)
# Remove predictors by some criteria (backward elimination)
reg <- lm(Price ~ ., data = train.df[, -1]) 
summary(reg)

step.reg <- step(reg, direction = "backward")
summary(step.reg)  # left with the best interactions

##Log-log
#Hetroscedasticity - unequal variance as function of y variable
#as one increases, the other's variance increases

#Read file - Boston property assessment data
df <- read.csv("West Roxbury.csv", stringsAsFactors = TRUE)
head(df)

### split the data
set.seed(1)  
train.index <- sample(1:dim(df)[1], dim(df)[1]*0.6)  
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

### plot
library(ggplot2)
ggplot(train.df, aes(y = TOTAL.VALUE, x = LOT.SQFT)) + 
  geom_point() + geom_smooth(method = lm)

### check heteroscedasticity
reg <- lm(TOTAL.VALUE ~ LOT.SQFT, data = train.df)
library(car)
ncvTest(reg)


### plot log-log
#Reduced hetroscedasticity
ggplot(train.df, aes(y = TOTAL.VALUE, x = LOT.SQFT)) + 
  scale_x_log10() + scale_y_log10() + 
  geom_point() + geom_smooth(method = lm)


### compare models
library(forecast)

# OLS - Ordinary least squares
reg <- lm(TOTAL.VALUE ~ LOT.SQFT, data = train.df)
summary(reg)
pred <- predict(reg, newdata = valid.df)
accuracy(pred, valid.df$TOTAL.VALUE)

# log-log model
reg <- lm(log(TOTAL.VALUE) ~ log(LOT.SQFT), data = train.df)
summary(reg)
pred <- predict(reg, newdata = valid.df)
accuracy(exp(pred), valid.df$TOTAL.VALUE) #better RMSE, MAPE

#Another Exercises

#Partition and Remove from training cars with weight above 1200
set.seed(1)
train.index<- sample(1:dim(toyota.df)[1], dim(toyota.df)[1]*0.6)
train.df<- toyota.df[train.index,]
valid.df<-toyota.df[-train.index,]
train.df <- subset(train.df, train.df$Weight <= 1200)

#LR training
library(ggplot2)
p<- ggplot(train.df, aes(x= Weight , y= Price)) + geom_point()
p
reg<-lm(Price~Weight, data=train.df)
summary(reg)

#LR Validation
library(forecast)
pred.valid<-predict(reg, newdata=valid.df)
accuracy(pred.valid, valid.df$Price)

#Price prediction for cars with weight of 1400
pred.weight<-predict(reg, newdata = data.frame(Weight=1400))
pred.weight #The predicted price is 25,850


##

library(ggplot2)
library(forecast) 

toyota.df <- read.csv("ToyotaCorolla.csv", stringsAsFactors = TRUE)
head(toyota.df)

#Convert into factor
toyota.df$Doors <- as.factor(toyota.df$Doors)


### split the data
set.seed(1)  
train.index <- sample(1:dim(toyota.df)[1], dim(toyota.df)[1]*0.6)  
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[-train.index, ]


## multiple regression 
reg <- lm(Price ~ ., data = train.df[, -1]) 
summary(reg)

pred.train <- predict(reg)
accuracy(pred.train, train.df$Price)
pred.valid <- predict(reg, newdata = valid.df)
accuracy(pred.valid, valid.df$Price)

## interactions
ggplot(train.df, aes(y = Price, x = Weight, color = Fuel_Type)) + 
  geom_point() + 
  xlim(c(1000, 1400))

reg2 <- lm(Price ~ .^2, data = train.df[, -1]) 
summary(reg2)

pred.train <- predict(reg2)
accuracy(pred.train, train.df$Price)
pred.valid <- predict(reg2, newdata = valid.df)
accuracy(pred.valid, valid.df$Price)

### stepwise
step.reg <- step(reg2, direction = "backward")
summary(step.reg)  

pred.train <- predict(step.reg)
accuracy(pred.train, train.df$Price)
pred.valid <- predict(step.reg, newdata = valid.df)
accuracy(pred.valid, valid.df$Price)


## Quarterly_Tax
ggplot(train.df, aes(x = as.factor(Quarterly_Tax), y = Price)) + 
  geom_bar(stat = "summary", fun = "mean")
#Heavy-tailed distribution

toyota.df$Quarterly_Tax <-  as.factor(toyota.df$Quarterly_Tax)





## Catalogs.csv
#Marketing efficiency of a high-tech company through catalogs and customer data



#Read file ans partitioning
catalog.df <- read.csv("Catalogs.csv", stringsAsFactors = TRUE)
head(catalog.df) 
set.seed(1)
train.index <- sample(1:dim(catalog.df)[1], dim(catalog.df)[1]*0.6) 
train.df <- catalog.df[train.index, ]
valid.df <- catalog.df[-train.index, ]



#bar plot of Catalogs via AmountSpent
library(ggplot2)

ggplot(train.df, aes(x = Catalogs, y = AmountSpent)) + 
  geom_bar(stat = "summary", fun = "mean") #linear relationship 
#Catalogs - quantitive

ggplot(train.df, aes(x = Children, y = AmountSpent)) + 
  geom_bar(stat = "summary", fun = "mean") #Non-linear relationship
#Childern - nominal

ggplot(train.df, aes(x = Age, y = AmountSpent)) + 
  geom_bar(stat = "summary", fun = "mean")
#Age- nominal


#Salary ~ AmountSpent

ggplot(train.df, aes(y = AmountSpent, x = Salary)) + 
  geom_point() + geom_smooth(method = lm)  # Left tale

ggplot(train.df, aes(y = AmountSpent, x = Salary)) + 
  scale_x_log10() + scale_y_log10() + 
  geom_point() + geom_smooth(method = lm) #Strong relationship using log-log transformation
#Right tale


#Multiple regression model for prediction of AmountSpent

train.df$Children<- as.factor(train.df$Children)
valid.df$Children<- as.factor(valid.df$Children)
class(train.df$Children)
class(valid.df$Children)
train.df$Age<- as.factor(train.df$Age)
valid.df$Age<- as.factor(valid.df$Age)
class(train.df$Age)
class(valid.df$Age)

library(forecast)

reg <- lm(log(AmountSpent) ~ log(Salary) + Age + Gender 
          + Married + Location + Children + Catalogs , data = train.df)
summary(reg) #R squared very high - 0.99.

pred.train <- predict(reg)
accuracy(exp(pred.train), train.df$AmountSpent)

pred.valid <- predict(reg, newdata = valid.df)
accuracy(exp(pred.valid), valid.df$AmountSpent)
# Results better on the validation, smaller RMSE indicates it.