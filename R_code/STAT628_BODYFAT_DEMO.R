library(ggplot2)

###Data loading
BodyFat = read.csv("../data/BodyFat.csv") 


###Data preprocess
#BodyFat calculated by density
#BMI calculated by wight and height
BodyFat$BODYFAT_cal = 495/BodyFat$DENSITY-450  
BodyFat$BMI = BodyFat$WEIGHT*0.4536/(BodyFat$HEIGHT*0.0254)^2

#remove the error data of BodyFat and BMI
BodyFat=BodyFat[-which(abs(BodyFat$BODYFAT-BodyFat$BODYFAT_cal)>2),]
BodyFat=BodyFat[-which(abs(BodyFat$BMI-BodyFat$ADIPOSITY)>1),]


###Model building
#Model1:Bodyfat ~ BMI
#Relationship between Bodyfat and BMI
ggplot(BodyFat,aes(ADIPOSITY,BODYFAT))+
        geom_point()+
        stat_smooth(method="lm",se=FALSE)+
        labs(
                x="BMI",
                y="Body Fat %",
                title="Scatterplot of Body Fat % and BMI"
        )

lmmodel = lm(BODYFAT ~ ADIPOSITY, data=BodyFat)
summary(lmmodel)

#Model2:Bodyfat ~ BMI without outlier   (improved model1)
BodyFat2=BodyFat[-which(abs(BodyFat$BMI)>35),]
ggplot(BodyFat2,aes(ADIPOSITY,BODYFAT))+
        geom_point()+
        stat_smooth(method="lm",se=FALSE)+
        labs(
                x="BMI",
                y="Body Fat %",
                title="Scatterplot of Body Fat % and BMI without outlier"
        )

lmmodel2 = lm(BODYFAT ~ ADIPOSITY, data=BodyFat2)
summary(lmmodel2)


#Model3:add another covariate to model2
#using added variable plot
#waiting for more debate
fit=lm(AGE~ADIPOSITY,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19)
lmage = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmage)
abline(lmage,col="blue",lwd=5)

fit=lm(ABDOMEN~ADIPOSITY,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19)
lmage = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmage)
abline(lmage,col="blue",lwd=5)

fit=lm(ANKLE~ADIPOSITY,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19)
lmage = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmage)
abline(lmage,col="blue",lwd=5)

fit=lm(FOREARM~ADIPOSITY,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19)
lmage = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmage)
abline(lmage,col="blue",lwd=5)


lmmodel3 = lm(BODYFAT ~ ADIPOSITY + AGE, data=BodyFat2)
summary(lmmodel3)



###Model diagnostics
diagnostic = function(model){
        par(mfrow = c(2, 2))
        ##Standardized Residual Plot
        plot(predict(model),resid(model),pch=19,main="Standardized Residual Plot")
        abline(a=0,b=0,col="black",lwd=3)
        ##Normal Q-Q Plot of the Residuals
        qqnorm(rstandard(model),pch=19,
               main="Normal Q-Q Plot of the Residuals")
        abline(a=0,b=1,col="black",lwd=3)
        ##Cook's distance
        plot(model,4)
}

diagnostic(lmmodel2)

#CrossValidation
CV=function(n,Z){
        z=rep(1:Z,ceiling(n/Z))[1:n]
        z=sample(z,n)
        mm=list()
        #mm[[i]]Ϊ??i???±꼯
        for (i in 1:Z) mm[[i]]=(1:n)[z==i];return(mm)
}

nrow=nrow(BodyFat2)

EEMS=function(t=3,Z=10){
        EMSE = numeric(t)
        for(i in 1:t){
                listcv=CV(nrow,Z)
                MSE=numeric(Z)
                for(j in 1:Z){   #ѭ??ʮ??
                        m=listcv[[j]]
                        train = BodyFat2[-m,]
                        pred = BodyFat2[m,]
                        model3 = lm(BODYFAT ~ ADIPOSITY + AGE , data=train)
                        predBodyFat = predict(model3, newdata = pred)
                        MSE[j]=sqrt(sum(predBodyFat - pred$BODYFAT)^2/length(predBodyFat))
                }
                EMSE[i] = mean(MSE)
        }
        EEE = mean(EMSE)
        return(EEE)
}

E=EEMS(100,10)
E
