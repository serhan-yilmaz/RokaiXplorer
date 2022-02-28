
processed_dataset <- reactive({
  req(filtered_dataset())
  req(filtered_metadata())
  ds <- filtered_dataset()
  Ts <- ds$Ts
  ST <- ds$ST
  Tmeta <- filtered_metadata()
  caseSamples <- Tmeta$caseSamples
  
  Tcase <- as.matrix(log2(Ts[, caseSamples]))
  Tcontrol <- as.matrix(log2(Ts[, !caseSamples]))
  
  nCase = ncol(Tcase)
  nControl = ncol(Tcontrol)
  
  validate(
    need((nCase+nControl)>0, "There are no samples in the selected subgroup."), 
    need((nCase)>0, "There are no case samples in the selected subgroup."), 
    need((nControl)>0, "There are no control samples in the selected subgroup.")
  )
  
  ## Sample Mean Balancing
  
  #####
  
  w_s = 1
  
  if(analyze_group_differences()){
    #  gd <- selected_group_differences()
    TcaseA = as.matrix(Tcase[, Tmeta$samplesA[caseSamples]])
    TcaseB = as.matrix(Tcase[, Tmeta$samplesB[caseSamples]])
    TcontrolA = as.matrix(Tcontrol[, Tmeta$samplesA[!caseSamples]])
    TcontrolB = as.matrix(Tcontrol[, Tmeta$samplesB[!caseSamples]])
    
    # message(colnames(TcaseB))
    # message(colnames(TcontrolB))
    
    McaseA <- apply(TcaseA, 1, function(x) mean(x, na.rm=T))
    McaseB <- apply(TcaseB, 1, function(x) mean(x, na.rm=T))
    McontrolA <- apply(TcontrolA, 1, function(x) mean(x, na.rm=T))
    McontrolB <- apply(TcontrolB, 1, function(x) mean(x, na.rm=T))
    
    Q_A <- McaseA - McontrolA;
    Q_B <- McaseB - McontrolB;
    
    Q <- Q_A - Q_B
    
    NcaseA <- apply(TcaseA, 1, function(x) nnzero(!is.na(x)))
    NcaseB <- apply(TcaseB, 1, function(x) nnzero(!is.na(x)))
    NcontrolA <- apply(TcontrolA, 1, function(x) nnzero(!is.na(x)))
    NcontrolB <- apply(TcontrolB, 1, function(x) nnzero(!is.na(x)))
    
    nCaseA = ncol(TcaseA)
    nCaseB = ncol(TcaseB)
    nControlA = ncol(TcontrolA)
    nControlB = ncol(TcontrolB)
    
    if((nCaseA >= 2) & (nControlA >= 2)){
      ScaseA <- apply(TcaseA, 1, function(x) sd(x, na.rm=T))
      SEcaseA <- ScaseA / sqrt(NcaseA - w_s)
      ScontrolA <- apply(TcontrolA, 1, function(x) sd(x, na.rm=T))
      SEcontrolA <- ScontrolA / sqrt(NcontrolA - w_s)
      SE_A <- sqrt(SEcaseA^2 + SEcontrolA^2)
      validsA <- (NcaseA >= 2) & (NcontrolA >= 2)
    } else {
      SE_A <- rep(sd(Q_A, na.rm = T), length(Q_A))
      validsA <- (NcaseA >= 1) & (NcontrolA >= 1)
    }
    
    if((nCaseB >= 2) & (nControlB >= 2)){
      ScaseB <- apply(TcaseB, 1, function(x) sd(x, na.rm=T))
      SEcaseB <- ScaseB / sqrt(NcaseB - w_s)
      ScontrolB <- apply(TcontrolB, 1, function(x) sd(x, na.rm=T))
      SEcontrolB <- ScontrolB / sqrt(NcontrolB - w_s)
      SE_B <- sqrt(SEcaseB^2 + SEcontrolB^2)
      validsB <- (NcaseB >= 2) & (NcontrolB >= 2)
    } else {
      SE_B <- rep(sd(Q_B, na.rm = T), length(Q_B))
      validsB <- (NcaseB >= 1) & (NcontrolB >= 1)
    }
    
    SE <- sqrt(SE_A^2 + SE_B^2)
    
    validSites = !is.na(Q) & !is.na(SE) & validsA & validsB
    
  } else {
    Mcase <- apply(Tcase, 1, function(x) mean(x, na.rm=T))
    Mcontrol <- apply(Tcontrol, 1, function(x) mean(x, na.rm=T))
    
    Q <- Mcase - Mcontrol;
    
    Ncase <- apply(Tcase, 1, function(x) nnzero(!is.na(x)))
    Ncontrol <- apply(Tcontrol, 1, function(x) nnzero(!is.na(x)))
    
    if((nCase >= 2) & (nControl >= 2)){
      Scase <- apply(Tcase, 1, function(x) sd(x, na.rm=T))
      SEcase <- Scase / sqrt(Ncase - w_s)
      Scontrol <- apply(Tcontrol, 1, function(x) sd(x, na.rm=T))
      SEcontrol <- Scontrol / sqrt(Ncontrol - w_s)
      SE <- sqrt(SEcase^2 + SEcontrol^2)
      valids <- (Ncase >= 2) & (Ncontrol >= 2)
    } else {
      SE <- rep(sd(Q, na.rm = T), length(Q))
      valids <- (Ncase >= 1) & (Ncontrol >= 1)
    }
    validSites = (!is.na(Q)) & valids
  }
  
  Xv = Q[validSites]
  Sx = SE[validSites]
  ST = ST[validSites, ]
  
  #Scase = apply(Tcase, 2, function(x) sd(x, na.rm=T))
  return (list("Xv" = Xv, "Sx" = Sx, "ST"= ST,"validSites" = validSites))
})

preprocessed_dataset <- reactive({
  req(processed_dataset())
  ds <- processed_dataset()
  ds$Xv = ds$Xv - mean(ds$Xv, na.rm = T)
  return (ds)
})