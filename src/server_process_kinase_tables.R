selected_ks_network <- reactive({
  req(reactive_network())
  NetworkData <- reactive_network()
  Wk2s <- NetworkData$net$Wkin2site.psp
  
  switch(input$ksNetwork, 
         "PhosphoSitePlus" = ropts <- list("includeSignor" = F),
         "PSP+Signor" = ropts <- list("includeSignor" = T)
  )
  
  if(ropts$includeSignor){
    Wk2s = Wk2s | NetworkData$net$Wkin2site.signor
  }
  
  #message(nnzero(Wk2s))
  return (Wk2s)
})


foPrepareKinaseTable <- function(ds){
  mFactor = mean(ds$Xv, na.rm = T)
  # ds$Xv = ds$Xv - mean(ds$Xv, na.rm = T)
  
  validSites = !is.na(ds$ST$NetworkDataIndex)
  
  Xv = ds$Xv[validSites] - mFactor
  Sx = ds$Sx[validSites]
  ST = ds$ST[validSites, ]
  
  validate(
    need(nrow(ST) > 0, "There are no phosphosites identified in the selected subgroup. Please make sure there are no conflicts in the subgroup selection.")
  )
  
  networkDataIndices = ST$NetworkDataIndex
  
  NetworkData <- reactive_network()
  Wk2s <- selected_ks_network()
  nSite = ncol(Wk2s)
  
  wk2s = Wk2s[, networkDataIndices];
  
  # browser()
  nSubs = (wk2s %*% rep(1, length(Xv)))
  
  network = input$rokaiNetwork
  switch(network, 
         "KinaseSubstrate" = ropts <- list("ppi" = F, "sd" = F, "coev" = F),
         "KS+PPI" = ropts <- list("ppi" = T, "sd" = F, "coev" = F),
         "KS+PPI+SD" = ropts <- list("ppi" = T, "sd" = T, "coev" = F),
         "KS+PPI+SD+CoEv" = ropts <- list("ppi" = T, "sd" = T, "coev" = T),
         stop("Invalid network for RoKAI"))
  
  rokaiEnabled = input$rokaiEnabled
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
  } else {
    # A <- as.numeric((wk2s %*% Xv) / nSubs)
    # S = as.numeric(sqrt((wk2s^2)%*%(Sx^2)) / nSubs)
    # Z = as.numeric(A / S)
    # # A = (wk2s %*% Xv) / nSubs
    # # S = sd(Xv) / sqrt(nSubs)
    # # Z = A / S
    Fk = wk2s
  }
  
  ##
  # ST$PDK1w = Fk[231, ]
  # ST$Xv = Xv
  # ST$SE = Sx
  # ST$DF = ds$DF[validSites]
  # write_xlsx(ST, "abcd.xlsx")
  
  Fk_main = Fk * wk2s
  Fk_neigh = Fk - Fk_main
  wMain = rowSums(Fk_main) / rowSums(Fk)
  wNeigh = rowSums(Fk_neigh) / rowSums(Fk)
  # Akin_main = (Fk_main %*% Xv) / rowSums(Fk_main)
  # Akin_neigh = (Fk_neigh %*% Xv) / rowSums(Fk_neigh)
  
  if(ds$useTtest){
    S = Sx
    # S = (Sx / ds$sd.inflationfactor[validSites])
    DF = ds$DF[validSites]
    ri <- rokai_inference(Xv, S, Fk, DF = DF)
    ri_main <- rokai_inference(Xv, S, Fk_main, DF = DF)
    ri_neigh <- rokai_inference(Xv, S, Fk_neigh, DF = DF)
  } else {
    ri <- rokai_inference(Xv, Sx, Fk)
    ri_main <- rokai_inference(Xv, Sx, Fk_main)
    ri_neigh <- rokai_inference(Xv, Sx, Fk_neigh)
  }
  Akin_main = ri_main$A
  Akin_neigh = ri_neigh$A
  Zkin_main = ri_main$Z
  Zkin_neigh = ri_neigh$Z
  
  A <- ri$A
  S <- ri$S
  DF <- ri$DF
  Z <- ri$Z
  res = compute_pvalues(as.matrix(Z))
  
  if(!is.null(input$kinase_activityscore)){
    switch(input$kinase_activityscore, 
           "Scaled" = scale_activities <- T, 
           "Original" = scale_activities <- F, 
           stop("Invalid kinase activity scoring option."))
    if(scale_activities == TRUE){
      A = A / wMain
      S = S / wMain
      wMain = wMain / wMain
      wNeigh = wNeigh / wMain
    }
  }
  
  num_subs_rokai = rowSums(Fk)^2 / rowSums(Fk^2);
  
  K = NetworkData$Kinase
  K$NumSubs = as.matrix(nSubs)
  K$NumSubsRokai = num_subs_rokai
  K$Activity = as.matrix(A)
  K$StdErr = as.matrix(S)
  K$DF = as.matrix(DF)
  K$WeightSubs = as.matrix(wMain)
  K$PhosSubs = as.matrix(Akin_main)
  K$ZScoreSubs = as.matrix(Zkin_main)
  K$WeightNeigh = as.matrix(wNeigh)
  K$PhosNeigh = as.matrix(Akin_neigh)
  K$ZScoreNeigh = as.matrix(Zkin_neigh)
  # browser()
  K$ZScore = as.matrix(Z)
  #K$ZScore = as.matrix(Z)
  K$PValue = res$PValues
  K$FDR = res$QValues
  K$MagnitudeAdj <- abs(K$Activity) - 2 * K$StdErr;
  K$EffectiveMag = pmax(K$MagnitudeAdj, 0)
  
  # isPhosphatase = K$Type == "Phosphatase"
  # K$Activity[isPhosphatase] = -1 * K$Activity[isPhosphatase]
  # K$ZScore[isPhosphatase] = -1 * K$ZScore[isPhosphatase]
  
  nameX = K$Name
  nameX[is.na(nameX)] = K$ID[is.na(nameX)]
  K$Identifier = nameX
  
  # K$isSignificant = (K$FDR <= 0.1) & (abs(PT$Phos) >= log2(1.25))
  
  return (K)
}

kinase_table <- reactive({
  req(preprocessed_dataset())
  req(selected_ks_network())
  
  ds <- preprocessed_dataset();
  return(foPrepareKinaseTable(ds))
})

foProcessKinaseTable <- function(KT){
  valids = (KT$NumSubs >= input$kinaselevel_minsubs)
  KT <- KT[valids, ]
  
  KT$FDR <- p.adjust(KT$PValue, method = "BH")
  
  max_fdr = input$kinaselevel_volcano_maxfdr
  if(input$kinaselevel_volcano_fdrcorrection == TRUE){
    pvals = KT$FDR
  } else {
    pvals = KT$PValue
  }
  KT$isSignificant = (pvals <= max_fdr)
  
  # min_logfc = input$kinaselevel_volcano_minlogfc
  # KT$isSignificant = (KT$FDR <= max_fdr) & (abs(KT$Activity) >= min_logfc)
  return(KT)
}

kinase_table_processed <- reactive({
  req(kinase_table())
  KT <- kinase_table()
  return(foProcessKinaseTable(KT))
})


foKinaseTargetsTable <- function(ST){
  NetworkData <- reactive_network()
  K = NetworkData$Kinase
  
  # Wks_depod = NetworkData$net$Wkin2site.depod
  Wks_psp = NetworkData$net$Wkin2site.psp
  Wks_signor = NetworkData$net$Wkin2site.signor
  
  Wkin2site <- selected_ks_network()
  
  validSites = !is.na(ST$NetworkDataIndex)
  ST = ST[validSites, ]
  
  wk2s = Wkin2site[, ST$NetworkDataIndex];
  # Wks_depod = Wks_depod[, validSites]
  Wks_psp = Wks_psp[, ST$NetworkDataIndex]
  Wks_signor = Wks_signor[, ST$NetworkDataIndex]
  
  indices = which(wk2s, arr.ind = T)
  i1 = indices[, 1]
  i2 = indices[, 2]
  # indices = which(wk2s)
  # i1 = indices %% nrow(wk2s) ## Modulo
  # i2 = floor(indices/nrow(wk2s))+ 1
  
  # withinDepod = Wks_depod[indices]
  withinPSP = Wks_psp[indices]
  datasource <- ifelse(withinPSP, "PhosphoSitePlus", "Signor")
  # datasource <- ifelse(withinDepod, "Depod", ifelse(withinPSP, "PhosphoSitePlus", "Signor"))
  
  KS = data.frame(
    UniprotID = K$KinaseID[i1],
    Name = K$KinaseName[i1],
    Gene = K$Gene[i1],
    # Type = K$Type[i1],
    SubsID = ST$Protein[i2],
    SubsProtein = ST$ProteinName[i2],
    Position = ST$Position[i2],
    DataSource = datasource, 
    # Flanking = ST$Flanking[i2],
    Phos = ST$Phos[i2],
    ZScore = ST$ZScore[i2],
    PValue = ST$PValue[i2],
    FDR = ST$FDR[i2], 
    isSignificant = ST$isSignificant[i2]
  )
  
  return (KS)
}


kinase_targets_table <- reactive({
  req(preprocessed_dataset())
  req(selected_ks_network())
  req(site_table_processed())
  
  ST  <- site_table_processed()
  KS = foKinaseTargetsTable(ST)
  
  return(KS)
})

foPrepareKinaseTableWithtargets <- function(KS, GT){
  KS$TargetSite = paste(KS$SubsProtein, KS$Position, sep = "-");
  # browser()
  concatenated <- aggregate(TargetSite ~ UniprotID, KS, function(x) paste(x, collapse = "; "));
  indices = match(concatenated$UniprotID, GT$KinaseID)
  values = rep(NA, nrow(GT))
  values[indices[!is.na(indices)]] = concatenated$TargetSite[!is.na(indices)]
  GT$Targets = values;
  
  return(GT)
}

kinase_table_processed_withtargets <- reactive({
  req(kinase_targets_table())
  KS = kinase_targets_table()
  KT = kinase_table_processed()
  KT = foPrepareKinaseTableWithtargets(KS, KT)
  return(KT)
})

foComputeKinaseTable <- function(ds){
  ST = foProcessSiteTable(foPrepareSiteTable(ds))
  KS = foKinaseTargetsTable(ST)
  KT = foProcessKinaseTable(foPrepareKinaseTable(ds))
  KT = foPrepareKinaseTableWithtargets(KS, KT)
  return(KT)
}


