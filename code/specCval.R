## 
##cross-validation code within the test set

pred_cval <- function(pheno, ETA, trait, model, stage, class){
  cortab <- data.frame(folds=1:10, r2=NA, vE=NA, pD=NA, Dic=NA)
  predA <- data.frame()
  
  set.seed(2024)
  for(Rep in 1:10){
    Y <- pheno
    YID <- unique(Y$id)
    N <- length(YID)
    folds <- replicate(10, sample(N, 0.20*N))
    
    for (k in 1:10) {
      y_tst <- YID[folds[,k]]
      tst <- which(Y$id %in% y_tst)
      y <- Y[,trait]
      y_na <- y
      y_na[tst] <- NA
      mod <- BGLR(y=y_na,ETA=ETA,nIter=10000,burnIn=2500, verbose = F)
      tst_e <- mod$yHat[tst]
      cortab$r2[k] <- cor(y[tst],tst_e, use = "complete.obs")
      cortab$vE[k] <- mod$varE
      cortab$pD[k] <- mod$fit$pD
      cortab$Dic[k] <- mod$fit$DIC
    }
    
    predA <- rbind(predA, data.frame(Rep = Rep, PA=mean(cortab$r2), sdcor=sd(cortab$r2), errorV=mean(cortab$vE),pd=mean(cortab$pD),
                                     DIC=mean(cortab$Dic), key=trait, model=model,stage=stage, class=class))
    
    pred_Data <- list(modtab=mod$VarE, cval=predA)
    
  }
  return(pred_Data)
}