selected_ks_network <- reactive({
  req(reactive_network())
  NetworkData <- reactive_network()
  Wk2s <- NetworkData$net$Wkin2site.psp
  
  #input$ksNetwork
  switch("PSP+Signor", 
         "PhosphoSitePlus" = ropts <- list("includeSignor" = F),
         "PSP+Signor" = ropts <- list("includeSignor" = T)
  )
  
  if(ropts$includeSignor){
    Wk2s = Wk2s | NetworkData$net$Wkin2site.signor
  }
  
  #message(nnzero(Wk2s))
  return (Wk2s)
})


kinase_table <- reactive({
  req(preprocessed_dataset())
  req(selected_ks_network())
  
  ds <- preprocessed_dataset();
  
  validSites = !is.na(ds$ST$NetworkDataIndex)
  Xv = ds$Xv[validSites]
  Sx = ds$Sx[validSites]
  ST = ds$ST[validSites, ]
  
  networkDataIndices = ST$NetworkDataIndex
  
  NetworkData <- reactive_network()
  Wk2s <- selected_ks_network()
  nSite = ncol(Wk2s)
  
  wk2s = Wk2s[, networkDataIndices];
  
  # browser()
  nSubs = (wk2s %*% rep(1, length(Xv)))
  
  #input$rokaiNetwork
  switch("KinaseSubstrate", 
         "KinaseSubstrate" = ropts <- list("ppi" = F, "sd" = F, "coev" = F),
         "KS+PPI" = ropts <- list("ppi" = T, "sd" = F, "coev" = F),
         "KS+PPI+SD" = ropts <- list("ppi" = T, "sd" = T, "coev" = F),
         "KS+PPI+SD+CoEv" = ropts <- list("ppi" = T, "sd" = T, "coev" = T))
  
  
  #input$rokaiEnabled
  rokaiEnabled = TRUE
  if(rokaiEnabled){
    if(ropts$ppi){
      Wk2k = NetworkData$net$Wkin2kin * 1e-3
    } else {
      Wk2k = NULL
    }
    Ws2s = sparseMatrix(
      i = c(),
      j = c(), 
      x = T,
      dims = c(nSite, nSite)
    )
    if(ropts$sd){
      Ws2s = Ws2s | NetworkData$net$Wsite2site.sd
    }
    if(ropts$coev){
      Ws2s = Ws2s | NetworkData$net$Wsite2site.coev
    }
    Ws2s = Ws2s[networkDataIndices, networkDataIndices]
    
    rc <- rokai_core(Xv, Sx, wk2s, Wk2k, Ws2s)
    Fk = rokai_kinase_weights(Xv, wk2s, rc$F)
    ri <- rokai_inference(Xv, Sx, Fk)
    A <- ri$A
    S <- ri$S
    Z <- ri$Z
  } else {
    A <- as.numeric((wk2s %*% Xv) / nSubs)
    S = as.numeric(sqrt((wk2s^2)%*%(Sx^2)) / nSubs)
    Z = as.numeric(A / S)
    # A = (wk2s %*% Xv) / nSubs
    # S = sd(Xv) / sqrt(nSubs)
    # Z = A / S
  }
  
  res = compute_pvalues(as.matrix(Z))
  
  K = NetworkData$Kinase
  K$NumSubs = as.matrix(nSubs)
  K$Activity = as.matrix(A)
  K$StdErr = as.matrix(S)
  K$ZScore = as.matrix(Z)
  #K$ZScore = as.matrix(Z)
  K$PValue = res$PValues
  K$FDR = res$QValues
  K$MagnitudeAdj <- abs(K$Activity) - 3 * K$StdErr;
  K$EffectiveMag = pmax(K$MagnitudeAdj, 0)
  
  # isPhosphatase = K$Type == "Phosphatase"
  # K$Activity[isPhosphatase] = -1 * K$Activity[isPhosphatase]
  # K$ZScore[isPhosphatase] = -1 * K$ZScore[isPhosphatase]
  
  nameX = K$Name
  nameX[is.na(nameX)] = K$ID[is.na(nameX)]
  K$Identifier = nameX
  
  # K$isSignificant = (K$FDR <= 0.1) & (abs(PT$Phos) >= log2(1.25))
  
  return (K)
})

kinase_table_processed <- reactive({
  req(kinase_table())
  KT <- kinase_table()
  max_fdr = input$kinaselevel_volcano_maxfdr
  min_logfc = input$kinaselevel_volcano_minlogfc
  KT$isSignificant = (KT$FDR <= max_fdr) & (abs(KT$Activity) >= min_logfc)
  return(KT)
})