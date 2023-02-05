site_table <- reactive({
  req(preprocessed_dataset())
  #    req(processed_protein_data_bysample())
  #    req(processed_data_bysample())
  ds <- preprocessed_dataset();
  
  validSites = ds$validSites
  Xv = ds$Xv
  Sx = ds$Sx
  Zx = Xv / Sx
  
  if(ds$useTtest){
    Tx = Xv / (Sx / ds$sd.inflationfactor)
    res = compute_pvalues(as.matrix(Tx), as.matrix(ds$DF))
  } else {
    res = compute_pvalues(as.matrix(Zx))
  }
  
  NetworkData <- reactive_network()
  ST = ds$ST[, c("Protein", "ProteinName", "Position", "Identifier")]
  ST$InRef = !is.na(ds$ST$NetworkDataIndex)
  ST$Phos = Xv
  ST$StdErr = Sx
  ST$StdErrT  = Sx / ds$sd.inflationfactor
  ST$DF = ds$DF
  ST$TStat = ST$Phos / ST$StdErrT
  ST$ZScore = Zx
  ST$InflationFactor = ds$sd.inflationfactor
  ST$PValue = res$PValues
  ST$FDR = res$QValues
  ST$MagnitudeAdj <- abs(Xv) - 3 * Sx;
  ST$EffectiveMag = pmax(ST$MagnitudeAdj, 0)
  
  # browser()
  # browser()
  
  validate(
    need(nrow(ST) > 0, "There are no ptms identified in the selected subgroup. Please make sure there are no conflicts in the subgroup selection.")
  )
  
  return (ST)
})

site_table_processed <- reactive({
  req(site_table())
  ST <- site_table()
  max_fdr = input$sitelevel_volcano_maxfdr
  min_logfc = input$sitelevel_volcano_minlogfc
  ST$isSignificant = (ST$FDR <= max_fdr) & (abs(ST$Phos) >= min_logfc)
  
  # ST$isSignificant[is.na(ST$isSignificant)] = FALSE
  return(ST)
})


protexpression_table <- reactive({
  req(preprocessed_expression_dataset())
  #    req(processed_protein_data_bysample())
  #    req(processed_data_bysample())
  ds <- preprocessed_expression_dataset();
  
  validSites = ds$validSites
  Xv = ds$Xv
  Sx = ds$Sx
  Zx = Xv / Sx
  
  if(ds$useTtest){
    Tx = Xv / (Sx / ds$sd.inflationfactor)
    res = compute_pvalues(as.matrix(Tx), as.matrix(ds$DF))
  } else {
    res = compute_pvalues(as.matrix(Zx))
  }
  
  NetworkData <- reactive_network()
  ST = ds$ST[, c("Protein", "ProteinName", "Position", "Identifier")]
  ST$Identifier = ST$ProteinName
  # ST$InRef = !is.na(ds$ST$NetworkDataIndex)
  ST$Phos = Xv
  ST$StdErr = Sx
  ST$StdErrT  = Sx / ds$sd.inflationfactor
  ST$DF = ds$DF
  ST$TStat = ST$Phos / ST$StdErrT
  ST$ZScore = Zx
  ST$InflationFactor = ds$sd.inflationfactor
  ST$PValue = res$PValues
  ST$FDR = res$QValues
  ST$MagnitudeAdj <- abs(Xv) - 3 * Sx;
  ST$EffectiveMag = pmax(ST$MagnitudeAdj, 0)
  
  validate(
    need(nrow(ST) > 0, "There are no proteins identified in the selected subgroup. Please make sure there are no conflicts in the subgroup selection.")
  )
  
  return (ST)
})

protexpression_table_processed <- reactive({
  req(protexpression_table())
  ST <- protexpression_table()
  max_fdr = input$protexpression_volcano_maxfdr
  min_logfc = input$protexpression_volcano_minlogfc
  ST$isSignificant = (ST$FDR <= max_fdr) & (abs(ST$Phos) >= min_logfc)
  
  # ST$isSignificant[is.na(ST$isSignificant)] = FALSE
  return(ST)
})




