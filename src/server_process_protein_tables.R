protein_table <- reactive({
  req(site_table())
  #req(reactive_network())
  #NetworkData <- reactive_network()
  #Wprotein2site <- t(NetworkData$Wsite2protein)
  
  ST <- site_table();
  proteins = unique(ST$Protein)
  indices = match(proteins, ST$Protein)
  names <- ST$ProteinName[indices]
  Protein <- data.frame(ID = proteins, Name = names)
  
  # validate(
  #    need(nrow(ST) > 0, "There are no proteins identified in the selected subgroup. Please make sure there are no concflicts in the subgroup selection.")
  # )
  
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
  
  PT = Protein[valids, c("ID", "Name")]
  PT$Phos = A[valids]
  PT$StdErr = SE[valids]
  PT$ZScore = Z[valids]
  res = compute_pvalues(as.matrix(PT$ZScore))
  PT$PValue = res$PValues
  PT$FDR = res$QValues
  PT$MagnitudeAdj <- abs(PT$Phos) - 3 * PT$StdErr;
  
  nameX = PT$Name
  nameX[is.na(nameX)] = PT$ID[is.na(nameX)]
  PT$Identifier = nameX
  
  #PT$isSignificant = (PT$FDR <= 0.1) & (abs(PT$Phos) >= log2(1.25))
  
  return (PT)
})

protein_table_processed <- reactive({
  req(protein_table())
  PT <- protein_table()
  max_fdr = input$proteinlevel_volcano_maxfdr
  min_logfc = input$proteinlevel_volcano_minlogfc
  PT$isSignificant = (PT$FDR <= max_fdr) & (abs(PT$Phos) >= min_logfc)
  return(PT)
})