MissouriRainfallData = read.csv("MissouriRainData_Final.csv",header=T)
fix(MissouriRainfallData)
MissouriRainDataOrig = MissouriRainfallData
MissouriRainfallData = MissouriRainfallData[,-c(1,7)]
sum(is.na(MissouriRainfallData$PRCP))
MissouriRainfallData = na.omit(MissouriRainfallData)
dim(MissouriRainfallData)
library(leaps)
regfit.full = regsubsets(PRCP~.,data=MissouriRainfallData, nvmax=10)
summary(regfit.full)

predict.regsubsets = function (object ,newdata ,id ,...){
form=as.formula (object$call [[2]])
mat=model.matrix (form ,newdata )
coefi =coef(object ,id=id)
xvars =names (coefi )
mat[,xvars ]%*% coefi
}

k=10
set.seed (1)
folds = sample(1:k,nrow(MissouriRainfallData), replace = TRUE)
cv.errors = matrix (NA , k, 10, dimnames =list(NULL , paste (1:10) ))

for(j in 1:k){
best.fit = regsubsets(PRCP~.,data=MissouriRainfallData[folds !=j,],nvmax = 10)
for(i in 1:10) {
pred=predict (best.fit ,MissouriRainfallData[folds ==j,], id=i)
cv.errors [j,i]=mean( (MissouriRainfallData$PRCP[folds ==j]-pred)^2)
}
}

mean.cv.errors =apply(cv.errors ,2, mean)
mean.cv.errors

par(mfrow =c(1,1))
plot(mean.cv.errors, type='b', main="Best Subset Selection", xlab = "No. of Predictors")

summary(regfit.full)
coef(regfit.full,6)

train.predictors = MissouriRainfallData[1:177478,c(2,3,5,7,8,10)]
fix(train.predictors)
test.predictors = MissouriRainfallData[177486:177537,c(2,3,5,7,8,10)]
fix(test.predictors)

train.response = MissouriRainfallData[1:177478,6]
fix(train.response)
test.response = MissouriRainfallData[177486:177537,6]
fix(test.response)

knnreg_pred.response38 = knn.reg(train = train.predictors, test = test.predictors, 
			y = train.response, k = 38)


par(mfrow =c(2,3))
plot(test.predictors$LATITUDE, test.response)
plot(test.predictors$ELEVATION, test.response)
plot(test.predictors$TMAX, test.response)
plot(test.predictors$TMIN, test.response)
plot(test.predictors$Month, test.response)

par(mfrow =c(2,3))
plot(test.predictors$LATITUDE, knnreg_pred.response38$pred)
plot(test.predictors$ELEVATION, knnreg_pred.response38$pred)
plot(test.predictors$TMAX, knnreg_pred.response38$pred)
plot(test.predictors$TMIN, knnreg_pred.response38$pred)
plot(test.predictors$Month, knnreg_pred.response38$pred)



serialnums = seq(0.5, 26, by=0.5)
serialnums

df <- data.frame(serialnums,test.response,knnreg_pred.response38$pred)




plot(serialnums,test.response,type="l",col="red",main="True Response vs Predicted Response",
	xlab="Test Sample#",ylab="true response and predicted response")
legend("topright",legend = c("trueresponse" ,"predictedresponse"),
		col=c("red","blue"),lty =1, lwd =2, cex =.8)
lines(serialnums,knnreg_pred.response38$pred,col="blue")



prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=10)



























