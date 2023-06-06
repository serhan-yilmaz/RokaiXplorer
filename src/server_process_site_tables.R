foPrepareSiteTable <- function(ds, sitetable = TRUE){
  if(sitetable == TRUE){
    item_txt = "ptms"
    col_names = c("Protein", "ProteinName", "Position", "Identifier", "ID")
  } else {
    item_txt = "proteins"
    col_names = c("Protein", "ProteinName", "Position", "Identifier")
  }
  
  validSites = ds$validSites
  Xv = ds$Xv
  Sx = ds$Sx
  Zx = Xv / Sx
  
  if(ds$useTtest){
    Tx = Xv / (Sx)
    # Tx = Xv / (Sx / ds$sd.inflationfactor)
    res = compute_pvalues(as.matrix(Tx), as.matrix(ds$DF))
    Zx = tstat2zscore(as.matrix(Tx), as.matrix(ds$DF))
    # Zx = -1 * qnorm(res$PValues/2) ## T-test equivalent Zscores
  } else {
    res = compute_pvalues(as.matrix(Zx))
  }
  
  NetworkData <- reactive_network()
  ST = ds$ST[, col_names]
  if(sitetable == FALSE){
    ST$Identifier = ST$ProteinName
  }
  ST$InRef = !is.na(ds$ST$NetworkDataIndex)
  ST$Phos = Xv
  ST$StdErr = Sx
  # ST$StdErrT  = Sx / ds$sd.inflationfactor
  ST$DF = ds$DF
  ST$TStat = ST$Phos / ST$StdErr
  ST$ZScore = Zx
  ST$InflationFactor = ds$sd.inflationfactor
  ST$PValue = res$PValues
  ST$FDR = res$QValues
  ST$MagnitudeAdj <- abs(Xv) - 3 * Sx;
  ST$EffectiveMag = pmax(ST$MagnitudeAdj, 0)
  ST$NetworkDataIndex = ds$ST$NetworkDataIndex
  
  validate(
    need(nrow(ST) > 0, paste0("There are no ", item_txt, " identified in the selected subgroup. Please make sure there are no conflicts in the subgroup selection."))
  )
  
  return (ST)
}

foProcessSiteTable <- function(ST){
  max_fdr = input$sitelevel_volcano_maxfdr
  min_logfc = input$sitelevel_volcano_minlogfc
  if(input$sitelevel_volcano_fdrcorrection == TRUE){
    pvals = ST$FDR
  } else {
    pvals = ST$PValue
  }
  ST$isSignificant = (pvals <= max_fdr) & (abs(ST$Phos) >= min_logfc)
  return(ST)
}

site_table <- reactive({
  req(preprocessed_dataset())
  ds <- preprocessed_dataset();
  return(foPrepareSiteTable(ds));
})
  
site_table_processed <- reactive({
  req(site_table())
  ST <- site_table()
  return(foProcessSiteTable(ST))
})


protexpression_table <- reactive({
  req(preprocessed_expression_dataset())
  #    req(processed_protein_data_bysample())
  #    req(processed_data_bysample())
  ds <- preprocessed_expression_dataset();
  return(foPrepareSiteTable(ds, sitetable = FALSE))
  # 
  # validSites = ds$validSites
  # Xv = ds$Xv
  # Sx = ds$Sx
  # Zx = Xv / Sx
  # 
  # if(ds$useTtest){
  #   Tx = Xv / (Sx)
  #   # Tx = Xv / (Sx / ds$sd.inflationfactor)
  #   res = compute_pvalues(as.matrix(Tx), as.matrix(ds$DF))
  #   Zx = tstat2zscore(as.matrix(Tx), as.matrix(ds$DF))
  #   # Zx = qnorm(1 - res$PValues/2) ## T-test equivalent Zscores
  # } else {
  #   res = compute_pvalues(as.matrix(Zx))
  # }
  # 
  # NetworkData <- reactive_network()
  # ST = ds$ST[, c("Protein", "ProteinName", "Position", "Identifier")]
  # ST$Identifier = ST$ProteinName
  # # ST$InRef = !is.na(ds$ST$NetworkDataIndex)
  # ST$Phos = Xv
  # ST$StdErr = Sx
  # # ST$StdErrT  = Sx / ds$sd.inflationfactor
  # ST$DF = ds$DF
  # ST$TStat = ST$Phos / ST$StdErr
  # ST$ZScore = Zx
  # ST$InflationFactor = ds$sd.inflationfactor
  # ST$PValue = res$PValues
  # ST$FDR = res$QValues
  # ST$MagnitudeAdj <- abs(Xv) - 3 * Sx;
  # ST$EffectiveMag = pmax(ST$MagnitudeAdj, 0)
  # 
  # validate(
  #   need(nrow(ST) > 0, "There are no proteins identified in the selected subgroup. Please make sure there are no conflicts in the subgroup selection.")
  # )
  # return (ST)
})

foProcessProtExpressionTable <- function(ST){
  max_fdr = input$protexpression_volcano_maxfdr
  min_logfc = input$protexpression_volcano_minlogfc
  if(input$protexpression_volcano_fdrcorrection == TRUE){
    pvals = ST$FDR
  } else {
    pvals = ST$PValue
  }
  ST$isSignificant = (pvals <= max_fdr) & (abs(ST$Phos) >= min_logfc)
  return(ST)
}

protexpression_table_processed <- reactive({
  req(protexpression_table())
  ST <- protexpression_table()
  # max_fdr = input$protexpression_volcano_maxfdr
  # min_logfc = input$protexpression_volcano_minlogfc
  # if(input$protexpression_volcano_fdrcorrection == TRUE){
  #   pvals = ST$FDR
  # } else {
  #   pvals = ST$PValue
  # }
  # ST$isSignificant = (pvals <= max_fdr) & (abs(ST$Phos) >= min_logfc)
  
  # ST$isSignificant[is.na(ST$isSignificant)] = FALSE
  return(foProcessProtExpressionTable(ST))
})




