site_table <- reactive({
  req(preprocessed_dataset())
  #    req(processed_protein_data_bysample())
  #    req(processed_data_bysample())
  ds <- preprocessed_dataset();
  
  validSites = ds$validSites
  Xv = ds$Xv
  Sx = ds$Sx
  Zx = Xv / Sx
  res = compute_pvalues(as.matrix(Zx))
  
  NetworkData <- reactive_network()
  ST = ds$ST[, c("Protein", "ProteinName", "Position", "Identifier")]
  ST$Phos = Xv
  ST$StdErr = Sx
  ST$ZScore = Zx
  ST$PValue = res$PValues
  ST$FDR = res$QValues
  ST$MagnitudeAdj <- abs(Xv) - 3 * Sx;
  ST$EffectiveMag = pmax(ST$MagnitudeAdj, 0)
  
  validate(
    need(nrow(ST) > 0, "There are no sites identified in the selected subgroup. Please make sure there are no conflicts in the subgroup selection.")
  )
  
  return (ST)
})

site_table_processed <- reactive({
  req(site_table())
  ST <- site_table()
  max_fdr = input$sitelevel_volcano_maxfdr
  min_logfc = input$sitelevel_volcano_minlogfc
  ST$isSignificant = (ST$FDR <= max_fdr) & (abs(ST$Phos) >= min_logfc)
  return(ST)
})