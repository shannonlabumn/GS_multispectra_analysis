## 
##model fitting with BGLR

modfit <- function(pheno, ETAs, stage, class){
  load(pheno) 
  load(ETAs)
  ETA_G <- list(Env=list(K=K.E,model='RKHS'), L=list(K=K.G,model='RKHS'))
  ETA_GE <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), LE=list(K=K.GE,model='RKHS'))
  if(stage==1){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W1,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.WE,model='RKHS'))
  }
  if(stage==2){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W2,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.W2E,model='RKHS'))
  }
  if(stage==3){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W3,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.W3E,model='RKHS'))
  }
  if(stage==4){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W4,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.W4E,model='RKHS'))
  }
  if(stage==5){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W5,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.W5E,model='RKHS'))
  }
  if(stage==7){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W7,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.W7E,model='RKHS'))
  }
  if(stage==8){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W8,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.W8E,model='RKHS'))
  }
  if(stage==9){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W9,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.W9E,model='RKHS'))
  }
  if(stage==10){
    ETA_W <- list(Env=list(K=K.E,model='RKHS'), W=list(K=W10,model='RKHS'))
    ETA_GEW <- list(Env=list(K=K.E,model='RKHS'), Line=list(K=K.G,model='RKHS'), WE=list(K=K.W10E,model='RKHS'))
  }
  
  if (class=="chips") {
    ntraits <- c("yield","sg","roundness")
  }else{
    ntraits <- c("yield","redness","lightness", "roundness","lxw")
  }
  
  G.cor1 <- NULL
  modelG1 <- vector(mode = "list",length = 3)
  set.seed(12345)
  for (i in (ntraits)) {
    G.cor <- NULL
    nmod <- c("G","W","GE","GEW")
    
    for (m in (nmod)) {
      if (m=="G") {
        modelG1[[i]] <- pred_cval(pheno = data1, ETA = ETA_G, trait = i, model = "G",stage = stage, class = class)
        G.cor <- rbind(G.cor,modelG1[[i]]$cval)
      }
      if (m=="W"){
        modelG1[[i]] <- pred_cval(pheno = data1, ETA = ETA_W, trait = i, model = "W",stage = stage, class = class)
        G.cor <- rbind(G.cor,modelG1[[i]]$cval)
      }
      if (m=="GE"){
        modelG1[[i]] <- pred_cval(pheno = data1, ETA = ETA_GE, trait = i, model = "G+GE", stage = stage, class = class)
        G.cor <- rbind(G.cor,modelG1[[i]]$cval)
      }
      if (m=="GEW"){
        modelG1[[i]] <- pred_cval(pheno = data1, ETA = ETA_GEW, trait = i, model = "G+WE",stage = stage, class = class)
        G.cor <- rbind(G.cor,modelG1[[i]]$cval)
      }
      
    }
    G.cor1 <- rbind(G.cor1, G.cor)
    
  }
  return(G.cor1)
  
}