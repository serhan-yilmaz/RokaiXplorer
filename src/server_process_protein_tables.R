foSite2ProteinMapping <- function(ST){
  NetworkData <- reactive_network()
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
  
  useTtest = any(!is.infinite(ST$DF) & !is.na(ST$DF), na.rm = T)
  
  if(useTtest){
    ### Welch–Satterthwaite approximation
    DF = (Wprotein2site %*% (ST$StdErr^2))^2 / (Wprotein2site^2 %*% ((ST$StdErr^4) / ST$DF));
    Tx = as.numeric(A / SE)
    Z = tstat2zscore(as.matrix(Tx), as.matrix(DF))
    # logpvals = pt(q=abs(as.matrix(Tx)), as.matrix(DF), lower.tail=FALSE, log.p = T)
    # Z = as.numeric(-1 * qnorm(logpvals, log.p = T))
    # res = compute_pvalues(as.matrix(Tx), as.matrix(DF))
    # Z = -1 * qnorm(res$PValues/2) ## T-test equivalent Zscores
  } else {
    DF = as.numeric(rep(Inf, length(A)))
    Z = as.numeric(A / SE)
  }
  valids = !is.na(Z)
  Wprotein2site = Wprotein2site[valids, ]
  
  return(list("Protein" = Protein, "Wprotein2site" = Wprotein2site, 
              "A" = A, "SE" = SE, "DF" = DF, "Z" = Z, "valids" = valids))
}

reactive_site2protein_mapping <- reactive({
  req(site_table())
  req(reactive_network())
  #Wprotein2site <- t(NetworkData$Wsite2protein)
  
  ST <- site_table();
  return(foSite2ProteinMapping(ST))
})

foPrepareProteinTable <- function(site2protein_mapping){
  out = site2protein_mapping
  Protein = out$Protein
  valids = out$valids
  
  PT = Protein[valids, c("ID", "Name")]
  PT$KinaseIndex = Protein$KinaseIndex
  PT$Phos = out$A[valids]
  PT$StdErr = out$SE[valids]
  PT$DF = out$DF[valids]
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
}

protein_table <- reactive({
  req(reactive_site2protein_mapping())
  mapping <- reactive_site2protein_mapping()
  return(foPrepareProteinTable(mapping))
})


foProcessProteinTable <- function(ST, PT, mapping){
  max_fdr = input$proteinlevel_volcano_maxfdr
  min_logfc = input$proteinlevel_volcano_minlogfc
  if(input$proteinlevel_volcano_fdrcorrection == TRUE){
    pvals = PT$FDR
  } else {
    pvals = PT$PValue
  }
  PT$isSignificant = (pvals <= max_fdr) & (abs(PT$Phos) >= min_logfc)
  PT$hasSignificantPSite = as.matrix(mapping$Wprotein2site %*% ST$isSignificant) > 0
  return(PT)
}

foComputeProteinTable <- function(ST){
  mapping = foSite2ProteinMapping(ST)
  PT = foPrepareProteinTable(mapping)
  return(foProcessProteinTable(ST, PT, mapping))
}

protein_table_processed <- reactive({
  req(protein_table())
  req(site_table_processed())
  req(reactive_site2protein_mapping())
  PT <- protein_table()
  ST <- site_table_processed()
  mapping <- reactive_site2protein_mapping()
  return(foProcessProteinTable(ST, PT, mapping))
})
