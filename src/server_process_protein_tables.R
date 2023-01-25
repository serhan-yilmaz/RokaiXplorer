reactive_site2protein_mapping <- reactive({
  req(site_table())
  req(reactive_network())
  NetworkData <- reactive_network()
  #Wprotein2site <- t(NetworkData$Wsite2protein)
  
  ST <- site_table();
  proteins = unique(ST$Protein)
  indices = match(proteins, ST$Protein)
  names <- ST$ProteinName[indices]
  Protein <- data.frame(ID = proteins, Name = names)
  
  Protein$KinaseIndex = match(Protein$ID, NetworkData$Kinase$KinaseID)
  
  indices = match(ST$Protein, Protein$ID)
  Wprotein2site <- sparseMatrix(
    i = indices,
    j = 1:nrow(ST), 
    x = TRUE,
    dims = c(nrow(Protein), nrow(ST))
  )
  
  A <- as.numeric((Wprotein2site %*% ST$Phos) / rowSums(Wprotein2site))
  SE = as.numeric(sqrt((Wprotein2site^2)%*%(ST$StdErr^2)) / rowSums(Wprotein2site))
  Z = as.numeric(A / SE)
  valids = !is.na(Z)
  Wprotein2site = Wprotein2site[valids, ]
  
  return(list("Protein" = Protein, "Wprotein2site" = Wprotein2site, 
              "A" = A, "SE" = SE, "Z" = Z, "valids" = valids))
})

protein_table <- reactive({
  req(reactive_site2protein_mapping())
  out <- reactive_site2protein_mapping()
  Protein = out$Protein
  valids = out$valids
  
  PT = Protein[valids, c("ID", "Name")]
  PT$KinaseIndex = Protein$KinaseIndex
  PT$Phos = out$A[valids]
  PT$StdErr = out$SE[valids]
  PT$ZScore = out$Z[valids]
  res = compute_pvalues(as.matrix(PT$ZScore))
  PT$PValue = res$PValues
  PT$FDR = res$QValues
  PT$MagnitudeAdj <- abs(PT$Phos) - 3 * PT$StdErr;
  PT$EffectiveMag = pmax(PT$MagnitudeAdj, 0)
  
  nameX = PT$Name
  nameX[is.na(nameX)] = PT$ID[is.na(nameX)]
  PT$Identifier = nameX
  
  #PT$isSignificant = (PT$FDR <= 0.1) & (abs(PT$Phos) >= log2(1.25))
  return (PT)
})

protein_table_processed <- reactive({
  req(protein_table())
  req(site_table_processed())
  req(reactive_site2protein_mapping())
  PT <- protein_table()
  ST <- site_table_processed()
  mapping <- reactive_site2protein_mapping()
  max_fdr = input$proteinlevel_volcano_maxfdr
  min_logfc = input$proteinlevel_volcano_minlogfc
  PT$isSignificant = (PT$FDR <= max_fdr) & (abs(PT$Phos) >= min_logfc)
  PT$hasSignificantPSite = as.matrix(mapping$Wprotein2site %*% ST$isSignificant) > 0
  
  return(PT)
})