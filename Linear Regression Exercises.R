# Climate Change
## Linear Regression for Temperature Change Prediction
climate = read.csv('climate_change.csv')
climate_linear = lm(Temp ~ . - Year - Month, data=climate)
summary(climate_linear)

## The Correlation of Variables
library(corrplot)
climate_corr = cor(climate[, 3:10]) # only consider the independent variables
corrplot.mixed(climate_corr, lower.col='black', number.cex=0.7, tl.cex=0.6)

## Exclude Collinearity and Re-Run the Linear Regression
climate_linear2 = lm(Temp ~ . - Year - Month - CO2 - CH4 - CFC_12, data=climate)
summary(climate_linear2)


# Forecasting Automobile Sales
## Train Test Split and Initial Linear Regression
WE2018 = read.csv('WranglerElantra2018.csv')
WE_train = WE2018[WE2018$Year <= 2017, ] # training set
WE_test = WE2018[WE2018$Year == 2018, ] # test set
Wrangler_lr1 = lm(
  Wrangler.Sales ~ Year + Unemployment.Rate + Wrangler.Queries + CPI.Energy + CPI.All, 
  data=WE_train)
summary(Wrangler_lr1)

## Choose a Subset of the Five Variables to Update the Model
Wrangler_lr2 = lm(
  Wrangler.Sales ~ Unemployment.Rate + Wrangler.Queries + CPI.Energy + CPI.All, 
  data=WE_train)
summary(Wrangler_lr2)

pred = predict(Wrangler_lr2, newdata = WE_test)
SSE = sum((pred - WE_test$Wrangler.Sales)^2)
train_mean = mean(WE_train$Wrangler.Sales)
SST = sum((train_mean - WE_test$Wrangler.Sales)^2)
OSR2 = 1 - SSE/SST
OSR2

## Plot Wrangler.Sales and Wrangler.Queries for Each Month over 2010-2018
library(dplyr)
library(ggplot2)
WE2018$date <- as.Date(WE2018$date,format="%m/%d/%y")
ggplot(data=WE2018, aes(x=date))+
  geom_line(aes(y=Wrangler.Sales, colour = "sales")) +
  geom_line(aes(y=Wrangler.Queries*200, colour = "queries")) +
  scale_y_continuous(sec.axis = sec_axis(~./200, name = "Wrangler Queries")) +
  theme_bw() +
  labs(x = "Month", y = "Wrangler Sales", colour="Parameter") +
  scale_colour_manual(values = c("blue", "red")) +
  theme(legend.position = c(0.1,0.85)) +
  scale_x_date(date_labels = "%Y-%m")

## Modeling Seasonality in Linear Regression with Month.Factor
Wrangler_lr3 = lm(
  Wrangler.Sales ~ Month.Factor + Unemployment.Rate + Wrangler.Queries + CPI.Energy + CPI.All, 
  data=WE_train)
summary(Wrangler_lr3)

pred = predict(Wrangler_lr3, newdata = WE_test)
SSE = sum((pred - WE_test$Wrangler.Sales)^2)
train_mean = mean(WE_train$Wrangler.Sales)
SST = sum((train_mean - WE_test$Wrangler.Sales)^2)
OSR2 = 1 - SSE/SST
OSR2

## Predict the Monthly Sales of Elantra with Linear Regression
Elantra_lr = lm(
  Elantra.Sales ~ Month.Factor + Unemployment.Rate + Wrangler.Queries + CPI.Energy + CPI.All, 
  data=WE_train)
summary(Elantra_lr)

pred = predict(Elantra_lr, newdata = WE_test)
SSE = sum((pred - WE_test$Elantra.Sales)^2)
train_mean = mean(WE_train$Elantra.Sales)
SST = sum((train_mean - WE_test$Elantra.Sales)^2)
OSR2 = 1 - SSE/SST
OSR2

## Trends of the Sales and their Correlations
ggplot(data=WE2018, aes(x=date))+
  geom_line(aes(y=Elantra.Sales, colour = "sales")) +
  geom_line(aes(y=Elantra.Queries*1000, colour = "queries")) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Elantra Queries")) +
  theme_bw() +
  labs(x = "Month", y = "Elantra Sales", colour="Parameter") +
  scale_colour_manual(values = c("blue", "red")) +
  theme(legend.position = c(0.1,0.85)) +
  scale_x_date(date_labels = "%Y-%m")

corrE = as.data.frame( # Elantra sales correlations
  cor(WE2018[,c("Elantra.Sales","Year","Unemployment.Rate","Elantra.Queries","CPI.Energy")]))
data.frame(Elantra.Sales=corrE$Elantra.Sales, row.names=row.names(corrE))

corrW = as.data.frame( # Wrangler sales correlations
  cor(WE2018[,c("Wrangler.Sales","Year","Unemployment.Rate","Elantra.Queries","CPI.Energy")]))
data.frame(Wrangler.Sales=corrW$Wrangler.Sales, row.names=row.names(corrW))

