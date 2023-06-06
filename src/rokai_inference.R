rokai_inference <- function(X, S, Fkin2site, DF = c()) {
  indA = !is.na(X)
  Fk = Fkin2site[,indA]
  X = X[indA]
  S = S[indA]
  
  if(is.empty(DF)){
    useTtest = FALSE
  } else {
    useTtest = any(!is.infinite(DF) & !is.na(DF), na.rm = T)
  }
  
  Akin = (Fk %*% X) / rowSums(Fk)
  Skin = sqrt((Fk^2)%*%(S^2)) / rowSums(Fk) # standard error
  if(useTtest){
    DFkin = (Fk %*% (S^2))^2 / (Fk^2 %*% ((S^4) / DF));
    Tx = as.numeric(Akin / Skin)
    Zkin = tstat2zscore(as.matrix(Tx), as.matrix(DFkin))
  } else {
    DFkin = rep(Inf, length(Akin))
    Zkin = Akin / Skin
  }
  
  return (list("A" = as.matrix(Akin), "S" = as.matrix(Skin), DF = as.matrix(DFkin), "Z" = as.matrix(Zkin)))
}