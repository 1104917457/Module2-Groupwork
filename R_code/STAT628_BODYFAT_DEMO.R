library(ggplot2)

###Data loading
BodyFat = read.csv("../data/BodyFat.csv") 


###Data preprocess
#BodyFat calculated by density
BodyFat$BODYFAT_cal = 495/BodyFat$DENSITY-450  

#Find and remove the error data of BodyFat
inequalList1 = BodyFat$IDNO[abs(BodyFat$BODYFAT-BodyFat$BODYFAT_cal)>2]
BodyFat = BodyFat[-which(abs(BodyFat$BODYFAT-BodyFat$BODYFAT_cal)>2),]


###Model building
#choose initial model
#Relationship between Bodyfat and Adiposity
ggplot(BodyFat,aes(ADIPOSITY,BODYFAT))+
        geom_point()+
        geom_smooth(method="lm",se=FALSE)+
        labs(
                x="Adiposity",
                y="Body Fat %",
                title="Scatterplot of Body Fat % and Adiposity"
        )

#Relationship between Bodyfat and Abdomen
ggplot(BodyFat,aes(ABDOMEN,BODYFAT))+
        geom_point()+
        geom_smooth(method="lm",se=FALSE)+
        labs(
                x="Abdomen",
                y="Body Fat %",
                title="Scatterplot of Body Fat % and Abdomen"
        )

#Relationship between Adiposity and Abdomen, finding collinearity
ggplot(BodyFat,aes(ABDOMEN,ADIPOSITY))+
        geom_point()+
        geom_smooth(method="lm",se=FALSE)+
        labs(
                x="Abdomen",
                y="Adiposity",
                title="Scatterplot of Adiposity and Abdomen"
        )

#Compare R square of the two model
lmmodel1_1 = lm(BODYFAT ~ ADIPOSITY, data=BodyFat)
summary(lmmodel1_1)
lmmodel1_2 = lm(BODYFAT ~ ABDOMEN, data=BodyFat)
summary(lmmodel1_2)

#The initial model is Bodyfat ~ Abdomen
lmmodel1=lmmodel1_2

#Model2:Bodyfat ~ Abdomen without influential point   (improved model1)
BodyFat2=BodyFat[-which(abs(BodyFat$ABDOMEN)>120),]
ggplot(BodyFat2,aes(ABDOMEN,BODYFAT))+
        geom_point()+
        stat_smooth(method="lm",se=FALSE)+
        labs(
                x="Abdomen",
                y="Body Fat %",
                title="Scatterplot of Body Fat % and Abdomen without influential point"
        )

lmmodel2 = lm(BODYFAT ~ ABDOMEN, data=BodyFat2)
summary(lmmodel2)




#Model3:add another covariate to model2
#using added variable plot
par(mfrow = c(2, 2))
fit=lm(AGE~ABDOMEN,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19)
lmage = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmage)
abline(lmage,col="blue",lwd=5)

fit=lm(NECK~ABDOMEN,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19)
lmage = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmage)
abline(lmage,col="blue",lwd=5)


#Model3_1:add Age variable
lmmodel3_1 = lm(BODYFAT ~ ABDOMEN + AGE, data=BodyFat2)
summary(lmmodel3_1)

#Model3_2:add Neck variable
lmmodel3_2 = lm(BODYFAT ~ ABDOMEN + AGE, data=BodyFat2)
summary(lmmodel3_2)

###Model comparison
#CrossValidation
CV=function(n,Z){
        z=rep(1:Z,ceiling(n/Z))[1:n]
        z=sample(z,n)
        mm=list()
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


###Model diagnostics
diagnostic = function(model){
        par(mfrow = c(2, 2))
        ##Standardized Residual Plot
        plot(predict(model),resid(model),pch=19,main="Standardized Residual Plot",xlab = "Prediction",ylab = "Residual")
        abline(a=0,b=0,col="black",lwd=3)
        ##Normal Q-Q Plot of the Residuals
        qqnorm(rstandard(model),pch=19,
               main="Normal Q-Q Plot of the Residuals")
        abline(a=0,b=1,col="black",lwd=3)
        ##Cook's distance
        plot(model,4)
        ##Pii
        pii4 = hatvalues(model)
        plot(1:length(model$residual),pii4,type="p",pch=19,main="Leverage Values (Pii)",xlab = "Obs. number",ylab = "Hatvalues")
}

diagnostic(lmmodel2)
