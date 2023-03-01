

kinase_targets_table <- reactive({
  req(preprocessed_dataset())
  req(selected_ks_network())
  req(site_table())
  
  ST  <- site_table()
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
    FDR = ST$FDR[i2]
  )
  
  return (KS)
})
