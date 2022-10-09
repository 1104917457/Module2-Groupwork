CV=function(n,Z){
  z=rep(1:Z,ceiling(n/Z))[1:n]
  z=sample(z,n)
  mm=list()
  #mm[[i]]为第i个下标集
  for (i in 1:Z) mm[[i]]=(1:n)[z==i];return(mm)
}

nrow=nrow(BodyFat2)

EEMS=function(t=3,Z=10){
  EMSE = numeric(t)
  for(i in 1:t){
    listcv=CV(nrow,Z)
    MSE=numeric(Z)
    for(j in 1:Z){   #循环十次
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