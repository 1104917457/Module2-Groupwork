BodyFat = read.csv("BodyFat.csv") 
dim(BodyFat)
head(BodyFat)
tail(BodyFat) 
BodyFat$B = 495/BodyFat$DENSITY-450
inequalList1 = BodyFat$IDNO[abs(BodyFat$BODYFAT-BodyFat$B)>2]
BodyFat=BodyFat[-which(abs(BodyFat$BODYFAT-BodyFat$B)>2),]
plot(BodyFat$BODYFAT,BodyFat$DENSITY,pch=19)
BodyFat$BMI = BodyFat$WEIGHT*0.4536/(BodyFat$HEIGHT*0.0254)^2
inequalList2 = BodyFat$IDNO[abs(BodyFat$BMI-BodyFat$ADIPOSITY)>1]
BodyFat=BodyFat[-which(abs(BodyFat$BMI-BodyFat$ADIPOSITY)>1),]
plot(BodyFat$BMI,BodyFat$ADIPOSITY,pch=19)

plot(ADIPOSITY,BODYFAT,pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="BMI",ylab="Body Fat %",main="Scatterplot of Body Fat % and Age")
lmmodel = lm(BODYFAT ~ ADIPOSITY, data=BodyFat)
abline(lmmodel,col="blue",lwd=5) #lwd is the line weight and makes the line thicker.
legend("bottomright",c("SLR Line"),col="blue",lwd=3,lty=c(1))
summary(lmmodel)

BodyFat2=BodyFat[-which(abs(BodyFat$BMI)>35),]
plot(BodyFat2$ADIPOSITY,BodyFat2$BODYFAT,pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="BMI",ylab="Body Fat %",main="Scatterplot of Body Fat % and Age")
lmmodel2 = lm(BODYFAT ~ ADIPOSITY, data=BodyFat2)
abline(lmmodel2,col="blue",lwd=5) #lwd is the line weight and makes the line thicker.
legend("bottomright",c("SLR Line"),col="blue",lwd=3,lty=c(1))
summary(lmmodel2)

plot(predict(lmmodel2),resid(lmmodel2),pch=19,main="Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=3)

qqnorm(rstandard(lmmodel2),pch=19,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

pii2 = hatvalues(lmmodel2)
cooki2 = cooks.distance(lmmodel2)

n = dim(BodyFat2)[1]
plot(1:n,pii2,type="p",pch=19,main="Leverage Values (Pii)")
plot(1:n,cooki2,type="p",pch=19,main="Influence Values (Cook's Distance)")

BodyFat2$model2res = BodyFat2$BODYFAT - predict(lmmodel2)
plot(BodyFat2$AGE,BodyFat2$model2res,pch=19)
lmage = lm(BodyFat2$model2res ~ BodyFat2$AGE)
summary(lmage)
abline(lmage,col="blue",lwd=5)

lmmodel3 = lm(BODYFAT ~ ADIPOSITY + AGE, data=BodyFat2)
summary(lmmodel3)

plot(predict(lmmodel3),resid(lmmodel3),pch=19,main="Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=3)

qqnorm(rstandard(lmmodel3),pch=19,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

pii3 = hatvalues(lmmodel3)
cooki3 = cooks.distance(lmmodel3)
plot(1:n,pii3,type="p",pch=19,main="Leverage Values (Pii)")
plot(1:n,cooki3,type="p",pch=19,main="Influence Values (Cook's Distance)")

BodyFat2$model3res = BodyFat2$BODYFAT - predict(lmmodel3)
plot(BodyFat2$model2res,BodyFat2$model3res,pch=19)

plot(BodyFat2$WRIST,BodyFat2$model3res,pch=19)
lmwr = lm(BodyFat2$model3res ~ BodyFat2$WRIST)
summary(lmwr)
abline(lmwr,col="blue",lwd=5)

lmmodel4 = lm(BODYFAT ~ ADIPOSITY + AGE + WRIST , data=BodyFat2)
summary(lmmodel4)

plot(predict(lmmodel4),resid(lmmodel4),pch=19,main="Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=3)

qqnorm(rstandard(lmmodel4),pch=19,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

pii4 = hatvalues(lmmodel4)
cooki4 = cooks.distance(lmmodel4)
plot(1:n,pii4,type="p",pch=19,main="Leverage Values (Pii)")
plot(1:n,cooki4,type="p",pch=19,main="Influence Values (Cook's Distance)")

BodyFat2$model4res = BodyFat2$BODYFAT - predict(lmmodel4)