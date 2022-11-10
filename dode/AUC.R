calauc <- function(edata,dlabel,f=5){
  meanauc=c()
  tmauc=c()
  for(t in 1:100){
    cled=t(edata)
    dind=which(dlabel==-1)
    nind=which(dlabel==1)
    sanu=floor(table(dlabel)/f)
    teind=list()
    for( m in 1:(f-1)){
      sdind=sample(1:length(dind),sanu[1])
      snind=sample(1:length(nind),sanu[2])
      teind[[m]]=c(dind[sdind],nind[snind])
      dind=dind[-sdind]
      nind=nind[-snind]}
    teind[[f]]=c(dind,nind)
    auc=c()  			
    for(n in 1:f){
      tind=teind[[n]]
      teset=cled[tind,]
      trset=cled[-tind,]
      trla = dlabel[-tind]
      tela = dlabel[tind]     
      pmodel <- svm(trset, trla,cost = 100,gamma = 0.01,probability=TRUE)
      pred <- predict(pmodel, teset, probability=TRUE,decision.values = TRUE)
      auc[n]<-auc(tela,attr(pred,"probabilities")[,2])}
    tmauc[t]=mean(auc)
  }
  meanauc=mean(tmauc)
  return(meanauc)
}