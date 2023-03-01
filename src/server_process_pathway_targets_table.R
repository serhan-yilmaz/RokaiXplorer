

pathway_targets_table <- reactive({
  req(go_enrichment_table())
  req(go_enrichment_table_processed())
  
  ds <- go_enrichment_table()
  GT <- go_enrichment_table_processed()
  
  # Wuniprotgene2goterm
  # Wuniprotgene2goterm_significant
  # UniprotGene
  
  UniprotGene = ds$UniprotGene
  Wuniprotgene2goterm = ds$Wuniprotgene2goterm[, GT$Index]
  Wuniprotgene2goterm_significant = ds$Wuniprotgene2goterm_significant[, GT$Index]
  
  indices = which(Wuniprotgene2goterm, arr.ind = T)
  i1 = indices[, 2]
  i2 = indices[, 1]
  # indx = which(Wuniprotgene2goterm)
  
  # ind2sub(mat = Wuniprotgene2goterm, indices = 366498)
  # i1 = indices %% nrow(Wuniprotgene2goterm) ## Modulo
  # i2 = floor(indices/nrow(Wuniprotgene2goterm)) + 1
  
  # browser()
  
  KS = data.frame(
    ID = GT$ID[i1],
    Name = GT$Name[i1],
    Category = GT$Category[i1],
    numProtein = GT$numProtein[i1],
    numIdentified = GT$numIdentified[i1],
    numSignificant = GT$numSignificant[i1],
    LogRiskRatio = GT$LogRiskRatio[i1], 
    EffectiveMag = GT$EffectiveMag[i1], 
    ZScore = GT$ZScore[i1], 
    FDR = GT$FDR[i1], 
    isSignificant = GT$isSignificant[i1], 
    TargetID = UniprotGene$ID[i2],
    TargetProtein = UniprotGene$Gene[i2],
    IsHit = UniprotGene$isSignificant[i2]
  )
  
  # browser()
  # ST  <- site_table()
  # NetworkData <- reactive_network()
  # K = NetworkData$Kinase
  # 
  # # Wks_depod = NetworkData$net$Wkin2site.depod
  # Wks_psp = NetworkData$net$Wkin2site.psp
  # Wks_signor = NetworkData$net$Wkin2site.signor
  # 
  # Wkin2site <- selected_ks_network()
  # 
  # validSites = !is.na(ST$NetworkDataIndex)
  # ST = ST[validSites, ]
  # 
  # wk2s = Wkin2site[, ST$NetworkDataIndex];
  # # Wks_depod = Wks_depod[, validSites]
  # Wks_psp = Wks_psp[, ST$NetworkDataIndex]
  # Wks_signor = Wks_signor[, ST$NetworkDataIndex]
  # 
  # indices = which(wk2s)
  # i1 = indices %% nrow(wk2s) ## Modulo
  # i2 = floor(indices/nrow(wk2s))+ 1
  # 
  # # withinDepod = Wks_depod[indices]
  # withinPSP = Wks_psp[indices]
  # datasource <- ifelse(withinPSP, "PhosphoSitePlus", "Signor")
  # # datasource <- ifelse(withinDepod, "Depod", ifelse(withinPSP, "PhosphoSitePlus", "Signor"))
  
  return (KS)
})







