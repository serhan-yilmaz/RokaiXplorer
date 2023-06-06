fo_process_data_bysample <- function(ds, Tmeta, norm_by = c(), type = "phosphorylation"){
  Ts <- ds$Ts
  ST <- ds$ST
  valid_rows = ST$Type == type
  Ts = Ts[valid_rows, ]
  ST = ST[valid_rows, ]
  validate(
    need(nnzero(valid_rows) > 0, paste0(tools::toTitleCase(type), " data is not available"))
  )
  
  caseSamples <- Tmeta$caseSamples
  
  # browser()
  
  Tcase <- as.matrix(log2(Ts[, caseSamples]))
  Tcontrol <- as.matrix(log2(Ts[, !caseSamples]))
  
  nCase = ncol(Tcase)
  nControl = ncol(Tcontrol)
  
  validate(
    need((nCase+nControl)>0, "There are no samples in the selected subgroup."), 
    need((nCase)>0, "There are no case samples in the selected subgroup."), 
    need((nControl)>0, "There are no control samples in the selected subgroup.")
  )
  
  nGrouping = length(norm_by)
  if(nGrouping > 0){
    Tx <- as.data.frame(t(Tmeta$Tsample_metadata))
    Tx$Identifier = rep("", nrow(Tx))
    for(iGrouping in 1:nGrouping){
      Tx$Identifier = paste(Tx$Identifier, Tx[[norm_by[iGrouping]]], sep = "_")
    }
    unique_groupings = unique(Tx$Identifier)
    
    Q <- Tcase
    for(iGrouping in 1:length(unique_groupings)){
      indices = Tx$Identifier == unique_groupings[iGrouping]
      caseGroupSamples = indices[caseSamples]
      controlGroupSamples = indices[!caseSamples]
      
      nnz_control = nnzero(controlGroupSamples)
      nnz_case = nnzero(caseGroupSamples)
      if((nnz_case > 0) & (nnz_control > 0)){
        TcaseV = Tcase[, caseGroupSamples]
        TcontrolV = as.matrix(Tcontrol[, controlGroupSamples])
        McontrolV <- apply(TcontrolV, 1, function(x) mean(x, na.rm=T))
        Q[, caseGroupSamples] = Q[, caseGroupSamples] - McontrolV
      }
    }
  } else {
    Mcontrol <- apply(Tcontrol, 1, function(x) mean(x, na.rm=T))
    Q <- Tcase - Mcontrol;
  }
  
  SE <-apply(Q, 2, function(x) rep(sd(x, na.rm = T), length(x)))
  
  Ncase <- apply(Tcase, 1, function(x) nnzero(!is.na(x)))
  Ncontrol <- apply(Tcontrol, 1, function(x) nnzero(!is.na(x)))
  
  valids <- (Ncase >= 1) & (Ncontrol >= 1)
  validSites = valids
  
  Xv = Q[validSites, ]
  Sx = SE[validSites, ]
  ST = ST[validSites, ]
  Ts = Ts[validSites, ]
  Ts <- log2(Ts)
  if(identical(input$options_var_stabilization, "Centering")){
    Ts = Ts - apply(Ts, 1, function(x) mean(x, na.rm=T))
  }
  return (list("Xv" = Xv, "Sx" = Sx, "ST"= ST, "Ts" = Ts, "validSites" = validSites, "Tmeta" = Tmeta))
}

processed_data_bysample <- reactive({
  req(filtered_dataset())
  req(filtered_metadata())
  ds <- filtered_dataset()
  Tmeta <- filtered_metadata()
  fo_process_data_bysample(ds, Tmeta)
})

processed_data_bysample_unfiltered <- reactive({
  req(current_dataset_mapped())
  req(current_metadata())
  ds <- current_dataset_mapped()
  Tmeta <- current_metadata()
  
  if(cache$cached_mbox_main_normgroup() == T){
    norm_by = input$mbox_site_plot_select_group
    if(is.null(norm_by)){norm_by = c()}
  } else {
    norm_by = c()
  }
  #norm_by = c("Gender", "Timepoint")
  
  fo_process_data_bysample(ds, Tmeta, norm_by)
})

processed_expression_data_bysample <- reactive({
  req(filtered_dataset())
  req(filtered_metadata())
  ds <- filtered_dataset()
  Tmeta <- filtered_metadata()
  fo_process_data_bysample(ds, Tmeta, type = "expression")
})

processed_expression_data_bysample_unfiltered <- reactive({
  req(current_dataset_mapped())
  req(current_metadata())
  ds <- current_dataset_mapped()
  Tmeta <- current_metadata()
  
  if(cache$cached_mbox_main_normgroup() == T){
    norm_by = input$mbox_site_plot_select_group
    if(is.null(norm_by)){norm_by = c()}
  } else {
    norm_by = c()
  }
  # is_expression_data_available
  #norm_by = c("Gender", "Timepoint")
  
  fo_process_data_bysample(ds, Tmeta, norm_by, type = "expression")
})



