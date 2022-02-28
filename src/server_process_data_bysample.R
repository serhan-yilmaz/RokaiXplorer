fo_process_data_bysample <- function(ds, Tmeta){
  Ts <- ds$Ts
  ST <- ds$ST
  caseSamples <- Tmeta$caseSamples
  
  Tcase <- as.matrix(log2(Ts[, caseSamples]))
  Tcontrol <- as.matrix(log2(Ts[, !caseSamples]))
  
  nCase = ncol(Tcase)
  nControl = ncol(Tcontrol)
  
  validate(
    need((nCase+nControl)>0, "There are no samples in the selected subgroup."), 
    need((nCase)>0, "There are no case samples in the selected subgroup."), 
    need((nControl)>0, "There are no control samples in the selected subgroup.")
  )
  
  Mcontrol <- apply(Tcontrol, 1, function(x) mean(x, na.rm=T))
  
  Q <- Tcase - Mcontrol;
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
  fo_process_data_bysample(ds, Tmeta)
})