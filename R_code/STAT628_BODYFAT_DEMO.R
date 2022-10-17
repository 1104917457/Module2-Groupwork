library(ggplot2)

summary(BodyFat2)

###Data loading
BodyFat = read.csv("../data/BodyFat.csv") 


###Data preprocess
#BodyFat calculated by density
#BMI calculated by weight and height
BodyFat$BODYFAT_cal = 495/BodyFat$DENSITY-450  
BodyFat$BMI = BodyFat$WEIGHT*0.4536/(BodyFat$HEIGHT*0.0254)^2


#Find and remove the error data of BodyFat and BMI
inequalList1 = BodyFat$IDNO[abs(BodyFat$BODYFAT-BodyFat$BODYFAT_cal)>2]
BodyFat = BodyFat[-which(abs(BodyFat$BODYFAT-BodyFat$BODYFAT_cal)>2),]
inequalList2 = BodyFat$IDNO[abs(BodyFat$BMI-BodyFat$ADIPOSITY)>1]
BodyFat = BodyFat[-which(abs(BodyFat$BMI-BodyFat$ADIPOSITY)>1),]

#change unit of Height to cm
BodyFat$HEIGHT=BodyFat$HEIGHT*2.54


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
BodyFat2=BodyFat[-which(abs(BodyFat$ABDOMEN)>130),]
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
fit=lm(AGE~ABDOMEN,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19,xlab="Residuals of Age ~ Abdomen",ylab="Residuals of BodyFat ~ Abdnmen",main ="Added variable plot of Age ")
lmage = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmage)
abline(lmage,col="blue",lwd=5)

fit=lm(HEIGHT~ABDOMEN,data=BodyFat2)
plot(fit$residuals,lmmodel2$residuals,pch=19,xlab="Residuals of Height ~ Abdomen",ylab="Residuals of BodyFat ~ Abdnmen",main ="Added variable plot of Height ")
lmheight = lm(lmmodel2$residuals ~ fit$residuals)
summary(lmheight)
abline(lmheight,col="blue",lwd=5)


#Model3_1:add Age variable
lmmodel3_1 = lm(BODYFAT ~ ABDOMEN + AGE, data=BodyFat2)
summary(lmmodel3_1)

#Model3_2:add Height variable
lmmodel3_2 = lm(BODYFAT ~ ABDOMEN + HEIGHT, data=BodyFat2)
summary(lmmodel3_2)


###Model comparison
evaluate=rbind(summary(lmmodel1)$r.squared,summary(lmmodel2)$r.squared,summary(lmmodel3_1)$r.squared,summary(lmmodel3_2)$r.squared)
evaluate

#The best model is model3_2: BodyFat ~ Abdomen + height
final_model=lmmodel3_2

###Model diagnostics
diagnostic = function(model){
        par(mfrow = c(1, 1))
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

diagnostic(final_model)
summary(final_model)


###Model weakness
set_accurate=5
Age1=BodyFat2$AGE<40 & BodyFat2$AGE>20
Age2=BodyFat2$AGE<60 & BodyFat2$AGE>40
Age3=BodyFat2$AGE<80 & BodyFat2$AGE>60
precise=function(scope,accurate){
        error_rate=final_model$residuals
        error_rate_partial=error_rate[scope]
        precision=sum(error_rate_partial<accurate)/length(error_rate_partial)
        print(precision)
}
precise(Age1,set_accurate)
precise(Age2,set_accurate)
precise(Age3,set_accurate)

