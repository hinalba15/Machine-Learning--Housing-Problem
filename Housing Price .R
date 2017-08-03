library(dplyr)
setwd("G:\\Hinal BA\\Kaggle")
train<- read.csv("train.csv")
test <- read.csv("test.csv")
#summarized Data
colSums(is.na(train))
colSums(is.na(test))
summary(train)
# Delet column which contain either over 70% missiong value or cantain same value 
train <- train[,-c(1,6,7,10,13,59,74,75,76)]
test <- test[,-c(1,6,7,10,13,59,74,75,76)]

# Combined train and test data
combined =  bind_rows(train, test)
colSums(is.na(combined))
colnames(combined)

#  missing value treatment
imputedFrontage <- combined %>%
  group_by(LotConfig) %>%
  summarize(medianFront = median(na.omit(LotFrontage)))
combined <- combined %>%
  left_join(imputedFrontage, by = c("LotConfig")) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), medianFront, LotFrontage)) %>%
  select(-medianFront)
combined$LotFrontage<- ifelse(is.na(combined$LotFrontage),69,combined$LotFrontage)
combined$MSZoning <- ifelse(is.na(combined$MSZoning), "RL", combined$MSZoning)
combined$Exterior1st <- ifelse(is.na(combined$Exterior1st), "VinylSd", combined$Exterior1st)
combined$Exterior2nd <- ifelse(is.na(combined$Exterior2nd), "VinylSd", combined$Exterior2nd)
combined$MasVnrType <- ifelse(is.na(combined$MasVnrType), "None", combined$MasVnrType)
combined$MasVnrArea <- ifelse(is.na(combined$MasVnrArea), 0, combined$MasVnrArea)
combined$BsmtQual <- ifelse(is.na(combined$BsmtQual), "None", combined$BsmtQual)
combined$BsmtCond <- ifelse(is.na(combined$BsmtCond), "None", combined$BsmtCond)
combined$BsmtExposure <- ifelse(is.na(combined$BsmtExposure), "None", combined$BsmtExposure)
combined$BsmtFinType1 <- ifelse(is.na(combined$BsmtFinType1), "None", combined$BsmtFinType1)
combined$BsmtFinSF1 <- ifelse(is.na(combined$BsmtFinSF1), 0, combined$BsmtFinSF1)
combined$BsmtFinType2 <- ifelse(is.na(combined$BsmtFinType2), "None", combined$BsmtFinType2)
combined$BsmtFinSF2 <- ifelse(is.na(combined$BsmtFinSF2), 0, combined$BsmtFinSF2)
combined$BsmtUnfSF <- ifelse(is.na(combined$BsmtUnfSF), 0, combined$BsmtUnfSF)
combined$TotalBsmtSF <- ifelse(is.na(combined$TotalBsmtSF), 0, combined$TotalBsmtSF)
combined$BsmtFullBath <- ifelse(is.na(combined$BsmtFullBath), 0, combined$BsmtFullBath)
combined$BsmtHalfBath <- ifelse(is.na(combined$BsmtHalfBath), 0, combined$BsmtHalfBath)
combined$Electrical <- ifelse(is.na(combined$Electrical), "FuseA", combined$Electrical)
combined$KitchenQual <- ifelse(is.na(combined$KitchenQual), "TA", combined$KitchenQual)
combined$Functional <- ifelse(is.na(combined$Functional), "Typ", combined$Functional)
combined$GarageType <- ifelse(is.na(combined$GarageType), "None", combined$GarageType)
combined$GarageYrBlt <- ifelse(is.na(combined$GarageYrBlt), 0, combined$GarageYrBlt)
combined$GarageFinish <- ifelse(is.na(combined$GarageFinish), "None", combined$GarageFinish)
combined$GarageCars <- ifelse(is.na(combined$GarageCars), 0, combined$GarageCars)
combined$GarageArea <- ifelse(is.na(combined$GarageArea), 0, combined$GarageArea)
combined$GarageQual <- ifelse(is.na(combined$GarageQual), "None", combined$GarageQual)
combined$GarageCond <- ifelse(is.na(combined$GarageCond), "None", combined$GarageCond)
combined$SaleType <- ifelse(is.na(combined$SaleType), "WD", combined$SaleType)
combined$Neighborhood_Dummy<-ifelse(is.na(combined$Neighborhood_Dummy),"Mitchel",combined$Neighborhood_Dummy)
colSums(is.na(combined))
summary(combined)

# bivariate analysis

library(ggplot2)
ggplot(data=train) + geom_point(mapping = aes(x = YearRemodAdd, y = SalePrice, color = Neighborhood_Dummy))
ggplot(data=train) + geom_point(mapping = aes(x = TotalBsmtSF, y = SalePrice, color = Neighborhood_Dummy))
plot(train$SalePrice~ train$MSZoning, col='blue', main = "price by MSZoning", xlab='MSZoning', ylab = 'price')
ggplot(data=train) + geom_point(mapping = aes(x= YearRemodAdd, y = SalePrice, color = Neighborhood_Dummy))
plot(train$SalePrice ~ train$OverallQual, col = 'green', main = "Price by OverallQual", xlab='OverallQual', ylab='Price')

# converting categorical data into numerical data
combined$MSZoning <- ifelse(combined$MSZoning== "RL",1,ifelse(combined$MSZoning=="RH",2,ifelse(combined$MSZoning=="FV",3,ifelse(combined$MSZoning=="RM",4,5))))
combined$Street <- ifelse(combined$Street == "Grvl",1,2)
combined$LotShape<-ifelse(combined$LotShape =="Reg",1,ifelse(combined$LotShape=="IR1",2,ifelse(combined$LotShape=="IR2",3,4 )))
combined$LandContour<-ifelse(combined$LandContour=="Lvl",1,ifelse(combined$LandContour=="Bnk",2,ifelse(combined$LandContour=="HLS",3,4)))

combined$LotConfig <- ifelse(combined$LotConfig == "Corner",1,ifelse(combined$LotConfig== "CulDSac",2,ifelse(combined$LotConfig=="Inside",3,4))) 
combined$LandSlope <- ifelse(combined$LandSlope=="Gtl",1,ifelse(combined$LandSlope=="Mod",2,3))
combined$Condition1<- ifelse(combined$Condition1=="Norm",1,ifelse(combined$Condition1=="Feedr",2,ifelse(combined$Condition1=="Artery",3,4)))
combined$Condition2<- ifelse(combined$Condition2=="Norm",1,ifelse(combined$Condition1=="Feedr",2,3))
combined$BldgType <- ifelse(combined$BldgType=="1Fam",1,ifelse(combined$BldgType=="TwnhsE",2,ifelse(combined$BldgType=="Duplex",3,4))) 
combined$HouseStyle <- ifelse(combined$HouseStyle=="1Story",1,ifelse(combined$HouseStyle=="2Story",2,ifelse(combined$HouseStyle=="1.5Fin",3,ifelse(combined$HouseStyle=="SLvl",4,5))))
combined$RoofStyle<- ifelse(combined$RoofStyle=="Gable",1,ifelse(combined$RoofStyle=="Hip",2,ifelse(combined$RoofStyle %in% c('Flat','Gambrel'),3,4)))
combined$RoofMatl <- ifelse(combined$RoofMatl=="CompShg",1,ifelse(combined$RoofMatl=="Tar&Grv",2,3))
combined$Exterior1st <- ifelse(combined$Exterior1st=="CemntBd",1,ifelse(combined$Exterior1st=="MetalSd",2,3))
combined$Exterior2nd <- ifelse(combined$Exterior2nd=="CemntBd",1,ifelse(combined$Exterior2nd=="MetalSd",2,3))
combined$MasVnrType<-ifelse(combined$MasVnrType %in% c('None,NA'),1,ifelse(combined$MasVnrType=="BrkFace",2,3))
combined$ExterQual<-ifelse(combined$ExterQual=="TA",1,ifelse(combined$ExterQual=="Gd",2,ifelse(combined$ExterQual=="Ex",3,4)))
combined$ExterCond<-ifelse(combined$ExterCond=="TA",1,ifelse(combined$ExterCond=="Gd",2,ifelse(combined$ExterCond=="Fx",3,4)))
combined$Foundation<- ifelse(combined$Foundation=="CBlock",1,ifelse(combined$Foundation=="PConc",2,ifelse(combined$Foundation=="BrkTil",3,4)))
combined$BsmtQual<-ifelse(combined$BsmtQual=="Gd",1,2)
combined$BsmtCond<-ifelse(combined$BsmtCond=="TA",1,2)
combined$BsmtExposure<- ifelse(combined$BsmtExposure=="Av",1,ifelse(combined$BsmtExposure=="Mn",2,3))
combined$BsmtFinType1<- ifelse(combined$BsmtFinType1=="GLQ",1,ifelse(combined$BsmtFinType1=="Unf",2,3))
combined$BsmtFinType2<-ifelse(combined$BsmtFinType2=="Unf",1,2)
combined$Heating<-ifelse(combined$Heating=="GasA",1,ifelse(combined$Heating=="GasW",2,3))
combined$HeatingQC<-ifelse(combined$HeatingQC=="Ex",1,ifelse(combined$HeatingQC=="TA",2,ifelse(combined$HeatingQC=="Gd",3,ifelse(combined$HeatingQC=="Fa",4,5))))
combined$CentralAir<-ifelse(combined$CentralAir=="Y",1,2)                                                     
combined$Electrical<-ifelse(combined$Electrical=="SBrkr",1,2)
combined$KitchenQual<-ifelse(combined$KitchenQual=="Gd",1,ifelse(combined$KitchenQual=="Ex",2,3))
combined$Functional<-ifelse(combined$Functional=="Typ",1,2)
combined$GarageType<-ifelse(combined$GarageType=="Attchd",1,ifelse(combined$GarageType=="Detchd",2,3))
combined$GarageFinish<-ifelse(combined$GarageFinish=="RFn",1,ifelse(combined$GarageFinish=="Fin",2,3))
combined$GarageQual <- ifelse(combined$GarageQual=="TA",1,2)
combined$GarageCond<- ifelse(combined$GarageCond=="TA",1,2)
combined$PavedDrive <- ifelse(combined$PavedDrive=="Y",1,ifelse(combined$PavedDrive=="N",2,3))
combined$SaleType<-ifelse(combined$SaleType == "WD",1,ifelse(combined$SaleType=="New",2,3))
combined$SaleCondition<- ifelse(combined$SaleCondition=="Normal",1,ifelse(combined$SaleCondition=="Abnormal",2,3))
View(combined)
write.csv(combined,"combined.csv")                                                                         
summary(combined)

#finding correlation
correlation1<- cor(combined,combined)
write.csv(correlation1,"correlation1.csv")

# divide data into train and test data set
train1 <- combined[1:1460,]
test1 <- combined[1461:2919,]

#run regression model
reg1 <- lm(SalePrice ~ ., 
           data = train1)
summary(reg1)


install.packages("MASS")
library(MASS) 
install.packages("Metrics")
library(Metrics)
install.packages("corrplot")
library(corrplot)
install.packages("randomForest")
library(randomForest)
install.packages("lars")
library(lars)
install.packages("ggplot2")
library(ggplot2)
install.packages("xgboost")
library(xgboost)
install.packages("Matrix")
library(Matrix)


Reg1<- lm(SalePrice ~  MSSubClass +  MSZoning + LotFrontage
          + LotArea + LotShape + LandContour + LotConfig
          +LandSlope + Condition1 + Condition2 +  BldgType
          +HouseStyle + OverallQual +  OverallCond + YearBuilt
          +YearRemodAdd + RoofStyle + RoofMatl +  Exterior1st
          +Exterior2nd + MasVnrArea + ExterQual
          +ExterCond + Foundation +  BsmtFinSF1
          + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF 
          +Heating + HeatingQC+ CentralAir  + X1stFlrSF + X2ndFlrSF       
          +LowQualFinSF + GrLivArea +  BsmtFullBath + BsmtHalfBath
          +FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr +KitchenQual
          + TotRmsAbvGrd + Functional + Fireplaces + GarageYrBlt 
          + GarageCars + GarageArea +PavedDrive + WoodDeckSF
          + OpenPorchSF +  EnclosedPorch + X3SsnPorch
          + ScreenPorch + PoolArea + MiscVal 
          +  MoSold + YrSold +  SaleType + SaleCondition,data = train1)

summary(reg1)
#Finding variable importance with Boruta
install.packages('Boruta')
library('Boruta')
install.packages('rFerns')
library('rFerns')
set.seed(1113)

bor<- Boruta(SalePrice ~  MSSubClass +  MSZoning + LotFrontage
             + LotArea + LotShape + LandContour + LotConfig
             +LandSlope + Condition1 + Condition2 +  BldgType
             +HouseStyle + OverallQual +  OverallCond + YearBuilt
             +YearRemodAdd + RoofStyle + RoofMatl +  Exterior1st
             +Exterior2nd + MasVnrArea + ExterQual
             +ExterCond + Foundation +  BsmtFinSF1
             + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF 
             +Heating + HeatingQC+ CentralAir  + X1stFlrSF + X2ndFlrSF       
             +LowQualFinSF + GrLivArea +  BsmtFullBath + BsmtHalfBath
             +FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr +KitchenQual
             + TotRmsAbvGrd + Functional + Fireplaces + GarageYrBlt 
             + GarageCars + GarageArea +PavedDrive + WoodDeckSF
             + OpenPorchSF +  EnclosedPorch + X3SsnPorch
             + ScreenPorch + PoolArea + MiscVal 
             +  MoSold + YrSold +  SaleType + SaleCondition, data=train1, doTrace=2)

plot(bor)
attStats(bor)
trainBoruta <- getSelectedAttributes(bor)
# using  Random Forest
library('randomForest')
install.packages
set.seed(12345)
colSums(is.na(train1))
rf_model <- randomForest(SalePrice ~ MSSubClass + LotFrontage
                         + LotArea + LotShape + LandContour
                         +LandSlope + Condition1  +  BldgType
                         +HouseStyle + OverallQual +  OverallCond + YearBuilt
                         +YearRemodAdd + RoofStyle + RoofMatl +  Exterior1st
                         +Exterior2nd + MasVnrArea + ExterQual
                         +ExterCond + Foundation +  BsmtFinSF1
                         + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF 
                         + HeatingQC+ CentralAir  + X1stFlrSF + X2ndFlrSF       
                         + GrLivArea +  BsmtFullBath 
                         +FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr +KitchenQual
                         + TotRmsAbvGrd  + Fireplaces + GarageYrBlt 
                         + GarageCars + GarageArea +PavedDrive + WoodDeckSF
                         + OpenPorchSF +  EnclosedPorch 
                         + ScreenPorch ,data = train1, method="class")
rf_model


varImpPlot(rf_model, main="rf_model")
prediction_1<-predict(rf_model, test1 ,type = "response")
prediction_1
submit_1 <- data.frame(PassengerId = test1$Id,SalePrice = prediction_1)
write.csv(prediction_1, file = "HousingPrice_rm1.csv", row.names = FALSE)


#GRADIENT BOOSTING MACHINE MODEL
install.packages('gbm')
library(gbm)
gbm_model <- gbm(SalePrice ~  MSSubClass +  MSZoning + LotFrontage
                 + LotArea + LotShape + LandContour + LotConfig
                 +LandSlope + Condition1 + Condition2 +  BldgType
                 +HouseStyle + OverallQual +  OverallCond + YearBuilt
                 +YearRemodAdd + RoofStyle + RoofMatl +  Exterior1st
                 +Exterior2nd + MasVnrArea + ExterQual
                 +ExterCond + Foundation +  BsmtFinSF1
                 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF 
                 +Heating + HeatingQC+ CentralAir  + X1stFlrSF + X2ndFlrSF       
                 +LowQualFinSF + GrLivArea +  BsmtFullBath + BsmtHalfBath
                 +FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr +KitchenQual
                 + TotRmsAbvGrd + Functional + Fireplaces + GarageYrBlt 
                 + GarageCars + GarageArea +PavedDrive + WoodDeckSF
                 + OpenPorchSF +  EnclosedPorch + X3SsnPorch
                 + ScreenPorch + PoolArea + MiscVal 
                 +  MoSold + YrSold +  SaleType + SaleCondition
                   , data =train1,
                 shrinkage = 0.05,
                 interaction.depth = 5,
                 bag.fraction = 0.66,
                 n.minobsinnode = 1,
                 cv.folds = 100,
                 keep.data = F,
                 verbose = F,
                 n.trees = 300)

gbm.perf(gbm_model)
gbm_model

predict <- predict(gbm_model, test1, n.trees=300)
predict

submit_1 <- data.frame(PassengerId = test$PassengerId, Survived = prediction4)
write.csv(predict, file = "housing_predict.csv", row.names = FALSE)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(Id = test1$Id, SalePrice = predict)

# Write the solution to file
write.csv(solution, file = 'housesubmission.csv', row.names = F)
