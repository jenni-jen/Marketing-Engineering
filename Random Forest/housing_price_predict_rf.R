# Predicting Housing Prices in Ames, Iowa
# read data
ames = read.csv("ames.csv")[, 2:75]
ames$SalePrice = as.numeric(ames$SalePrice)

# Explore the Dataset
## The sales prices of each year
table(ames$YrSold)
boxplot(SalePrice ~ YrSold, data=ames, xlab="Year", ylab="Sale Price")

## Correlation between areas and prices
library(corrplot)
areas_prices = subset(ames, select=c("TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", 
                                     "GarageArea", "PoolArea", "SalePrice"))
corrplot.mixed(cor(areas_prices), lower.col="black", tl.pos="lt", tl.cex=0.7)

# Linear Regression Model
## train test split
set.seed(123)
split = sort(sample(seq_len(nrow(ames)), 0.8*nrow(ames)))
amesTrain = ames[split,]
amesTest = ames[-split,]

## fit model
lr_fit = lm(SalePrice ~ ., data=amesTrain)
summary(lr_fit)

# Random Forest Model
library(caret)
library(randomForest)
## use cross-validation to select the value of the mtry parameter
rf.cv = train(y = amesTrain$SalePrice,
              x = subset(amesTrain, select=-c(SalePrice)),
              method="rf", nodesize=25, ntree=80,
              trControl=trainControl(method="cv", number=10),  # 10-fold 
              tuneGrid=data.frame(mtry=seq(1,10,1)))           # tune mtry
rf.cv

## rf importance
rf_fit = rf.cv$finalModel
important_vars = importance(rf_fit)
head(important_vars[order(important_vars, decreasing=TRUE),], 10)

# Report Performance Metrics
## linear regression performance
### in sample
in_original = amesTrain$SalePrice
in_predicted = predict(lr_fit, newdata=amesTrain)
in_R2 = R2(in_predicted, in_original)
in_MAE = MAE(in_predicted, in_original)
in_RMSE = RMSE(in_predicted, in_original)
### out of sample
out_original = amesTest$SalePrice
out_predicted = predict(lr_fit, newdata=amesTest)
out_R2 = R2(out_predicted, out_original)
out_MAE = MAE(out_predicted, out_original)
out_RMSE = RMSE(out_predicted, out_original)
data.frame(R2=c(in_R2, out_R2), MAE=c(in_MAE, out_MAE), RMSE=c(in_RMSE, out_RMSE),
           row.names=c("in-sample", "out-of-sample")) # output

## random forest performance
### in sample
in_original = amesTrain$SalePrice
in_predicted = predict(rf_fit, newdata=amesTrain)
in_R2 = R2(in_predicted, in_original)
in_MAE = MAE(in_predicted, in_original)
in_RMSE = RMSE(in_predicted, in_original)
### out of sample
out_original = amesTest$SalePrice
out_predicted = predict(rf_fit, newdata=amesTest)
out_R2 = R2(out_predicted, out_original)
out_MAE = MAE(out_predicted, out_original)
out_RMSE = RMSE(out_predicted, out_original)
data.frame(R2=c(in_R2, out_R2), MAE=c(in_MAE, out_MAE), RMSE=c(in_RMSE, out_RMSE),
           row.names=c("in-sample", "out-of-sample")) # output


