library(ggplot2)

###Data loading
BodyFat = read.csv("BodyFat.csv") 


###Data preprocess
#BodyFat calculated by density
#BMI calculated by weight and height
inequalList1 = BodyFat$IDNO[abs(BodyFat$BODYFAT-BodyFat$B)>2]
inequalList2 = BodyFat$IDNO[abs(BodyFat$BMI-BodyFat$ADIPOSITY)>1]
BodyFat$BODYFAT_cal = 495/BodyFat$DENSITY-450  
BodyFat$BMI = BodyFat$WEIGHT*0.4536/(BodyFat$HEIGHT*0.0254)^2

#remove the error data of BodyFat and BMI
BodyFat=BodyFat[-which(abs(BodyFat$BODYFAT-BodyFat$BODYFAT_cal)>2),]
BodyFat=BodyFat[-which(abs(BodyFat$BMI-BodyFat$ADIPOSITY)>1),]


###Model building
#Model1:Bodyfat ~ ADIPOSITY
#Relationship between Bodyfat and ABDOMEN
ggplot(BodyFat,aes(ABDOMEN,BODYFAT))+
        geom_point()+
        stat_smooth(method="lm",se=FALSE)+
        labs(
                x="BMI",
                y="Body Fat %",
                title="Scatterplot of Body Fat % and ABDOMEN"
        )

lmmodel = lm(BODYFAT ~ ABDOMEN, data=BodyFat)
summary(lmmodel)

#Model2:Bodyfat ~ ABDOMEN without outlier   (improved model1)
BodyFat2=BodyFat[-which(abs(BodyFat$ABDOMEN)>120),]
outlierList = BodyFat$IDNO[abs(BodyFat$ABDOMEN)>120]
ggplot(BodyFat2,aes(ABDOMEN,BODYFAT))+
        geom_point()+
        stat_smooth(method="lm",se=FALSE)+
        labs(
                x="BMI",
                y="Body Fat %",
                title="Scatterplot of Body Fat % and BMI without outlier"
        )

lmmodel2 = lm(BODYFAT ~ ABDOMEN, data=BodyFat2)
summary(lmmodel2)


#Model3:add another covariate to model2
#using added variable plot
#waiting for more debate
fit=lm(AGE~ABDOMEN,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19)
lmage = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmage)
abline(lmage,col="blue",lwd=5)

fit2=lm(ADIPOSITY~ABDOMEN,data=BodyFat2)
plot(fit2$residuals,lmmodel2$residuals,pch=19)
lmbmi = lm(lmmodel2$residuals ~ fit2$residuals)
summary(lmbmi)
abline(lmbmi,col="blue",lwd=5)


lmmodel3 = lm(BODYFAT ~ ABDOMEN + AGE, data=BodyFat2)
summary(lmmodel3)

#Model4:add another covariate to model3
#using added variable plot
#waiting for more debate

fit3=lm(CHEST~ABDOMEN,data=BodyFat2)
plot(fit3$residuals,lmmodel3$residuals,pch=19)
lmchest = lm(lmmodel3$residuals ~ fit3$residuals)
summary(lmchest)
abline(lmchest,col="blue",lwd=5)

fit4=lm(HIP~ABDOMEN,data=BodyFat2)
plot(fit4$residuals,lmmodel3$residuals,pch=19)
lmhip = lm(lmmodel3$residuals ~ fit4$residuals)
summary(lmhip)
abline(lmhip,col="blue",lwd=5)

fit5=lm(THIGH~ABDOMEN,data=BodyFat2)
plot(fit5$residuals,lmmodel3$residuals,pch=19)
lmthigh = lm(lmmodel3$residuals ~ fit5$residuals)
summary(lmthigh)
abline(lmthigh,col="blue",lwd=5)

fit6=lm(FOREARM~ABDOMEN,data=BodyFat2)
plot(fit6$residuals,lmmodel3$residuals,pch=19)
lmfore = lm(lmmodel3$residuals ~ fit6$residuals)
summary(lmfore)
abline(lmfore,col="blue",lwd=5)

lmmodel4 = lm(BODYFAT ~ ABDOMEN + AGE + CHEST, data=BodyFat2)
summary(lmmodel4)

###Model diagnostics
diagnostic = function(model,n){
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
        pii2 = hatvalues(lmmodel2)
        plot(1:n,pii2,type="p",pch=19,main="Leverage Values (Pii)")
}

diagnostic(lmmodel2,242)
diagnostic(lmmodel3,242)
diagnostic(lmmodel4,242)


#CrossValidation
CV=function(n,Z){
        z=rep(1:Z,ceiling(n/Z))[1:n]
        z=sample(z,n)
        mm=list()
        for (i in 1:Z) mm[[i]]=(1:n)[z==i];return(mm)
}

nrow=nrow(BodyFat2)

EEMS=function(t=3,Z=10,mod){
        EMSE = numeric(t)
        for(i in 1:t){
                listcv=CV(nrow,Z)
                MSE=numeric(Z)
                for(j in 1:Z){   
                        m=listcv[[j]]
                        train = BodyFat2[-m,]
                        pred = BodyFat2[m,]
                        if (mod == 'lmmodel2'){
                                model = lm(BODYFAT ~ ABDOMEN , data=train)
                                predBodyFat = predict(model, newdata = pred)
                        }
                        if (mod == 'lmmodel3'){
                                model = lm(BODYFAT ~ ABDOMEN + AGE, data=train)
                                predBodyFat = predict(model, newdata = pred)
                        }
                        if (mod == 'lmmodel4'){
                                model = lm(BODYFAT ~ ABDOMEN + AGE + CHEST, data=train)
                                predBodyFat = predict(model, newdata = pred)
                        }
                        MSE[j]=sqrt(sum(predBodyFat - pred$BODYFAT)^2/length(predBodyFat))
                }
                EMSE[i] = mean(MSE)
        }
        EEE = mean(EMSE)
        return(EEE)
}

E1=EEMS(100,5,'lmmodel2')
E2=EEMS(100,5,'lmmodel3')
E3=EEMS(100,5,'lmmodel4')
E1
E2
E3