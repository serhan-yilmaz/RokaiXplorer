
fo_process_dataset <- function(ds, Tmeta, type = "phosphorylation"){
  Ts <- ds$Ts
  ST <- ds$ST
  valid_rows = ST$Type == type
  Ts = Ts[valid_rows, ]
  ST = ST[valid_rows, ]
  validate(
    need(nnzero(valid_rows) > 0, paste0(tools::toTitleCase(type), " data is not available"))
  )
  
  # browser()
  
  caseSamples <- Tmeta$caseSamples
  
  Tcase <- as.matrix(log2(Ts[, caseSamples]))
  Tcontrol <- as.matrix(log2(Ts[, !caseSamples]))
  
  Tcase[is.infinite(Tcase)] = NA
  Tcontrol[is.infinite(Tcontrol)] = NA
  
  if(identical(input$options_var_stabilization, "Centering")){
    Mcase_samples <- apply(Tcase, 2, function(x) mean(x, na.rm=T))
    Mcontrol_samples <- apply(Tcontrol, 2, function(x) mean(x, na.rm=T))
    
    Tcase = Tcase - Mcase_samples
    Tcontrol = Tcontrol - Mcontrol_samples
  }
  
  nCase = ncol(Tcase)
  nControl = ncol(Tcontrol)
  
  validate(
    need((nCase+nControl)>0, "There are no samples in the selected subgroup."), 
    need((nCase)>0, "There are no case samples in the selected subgroup."), 
    need((nControl)>0, "There are no control samples in the selected subgroup.")
  )
  
  ## Sample Mean Balancing
  
  #####
  
  # w_s = 1
  
  # apply_ttest = TRUE
  apply_ttest = input$options_var_across_samples
  MIN_NSAMPLES = input$options_minsamples
  moderated = input$options_moderated_ttest
  
  # MIN_NSAMPLES = 3
  
  if(analyze_group_differences()){
    #  gd <- selected_group_differences()
    TcaseA = as.matrix(Tcase[, Tmeta$samplesA[caseSamples]])
    TcaseB = as.matrix(Tcase[, Tmeta$samplesB[caseSamples]])
    TcontrolA = as.matrix(Tcontrol[, Tmeta$samplesA[!caseSamples]])
    TcontrolB = as.matrix(Tcontrol[, Tmeta$samplesB[!caseSamples]])
    
    McaseA <- apply(TcaseA, 1, function(x) mean(x, na.rm=T))
    McaseB <- apply(TcaseB, 1, function(x) mean(x, na.rm=T))
    McontrolA <- apply(TcontrolA, 1, function(x) mean(x, na.rm=T))
    McontrolB <- apply(TcontrolB, 1, function(x) mean(x, na.rm=T))
    
    Q_A <- McaseA - McontrolA;
    Q_B <- McaseB - McontrolB;
    
    Q <- Q_A - Q_B
    
    NcaseA <- apply(TcaseA, 1, function(x) nnzero(!is.na(x)))
    NcaseB <- apply(TcaseB, 1, function(x) nnzero(!is.na(x)))
    NcontrolA <- apply(TcontrolA, 1, function(x) nnzero(!is.na(x)))
    NcontrolB <- apply(TcontrolB, 1, function(x) nnzero(!is.na(x)))
    
    nCaseA = ncol(TcaseA)
    nCaseB = ncol(TcaseB)
    nControlA = ncol(TcontrolA)
    nControlB = ncol(TcontrolB)
    
    if((nCaseA >= MIN_NSAMPLES) & (nControlA >= MIN_NSAMPLES) & (nCaseB >= MIN_NSAMPLES) & (nControlB >= MIN_NSAMPLES) & apply_ttest == TRUE){
      ScaseA <- apply(TcaseA, 1, function(x) sd(x, na.rm=T))
      ScontrolA <- apply(TcontrolA, 1, function(x) sd(x, na.rm=T))
      ScaseB <- apply(TcaseB, 1, function(x) sd(x, na.rm=T))
      ScontrolB <- apply(TcontrolB, 1, function(x) sd(x, na.rm=T))
      valids <- (NcaseA >= MIN_NSAMPLES) & (NcontrolA >= MIN_NSAMPLES) & (NcaseB >= MIN_NSAMPLES) & (NcontrolB >= MIN_NSAMPLES)
      
      ## Pooled standard deviations
      S = sqrt((NcaseA - 1) * ScaseA^2 + (NcontrolA - 1) * ScontrolA^2 +
                 (NcaseB - 1) * ScaseB^2 + (NcontrolB - 1) * ScontrolB^2) / 
        sqrt(NcaseA + NcontrolA + NcaseB + NcontrolB - 4);
      se_factor = sqrt(1/NcaseA + 1/NcontrolA + 1/NcaseB + 1/NcontrolB)
      DF = NcaseA + NcontrolA + NcaseB + NcontrolB - 4
      
      S[!valids] = NA
      
      ## Add moderated t-test here
      # moderated = TRUE
      if(moderated){
        fit = limma::squeezeVar(S^2, DF)
        S = sqrt(fit$var.post)
        DF = DF + fit$df.prior
      }
      
      SE = se_factor * S;
      
      useTtest = TRUE
    } else {
      SE <- rep(sd(Q, na.rm = T), length(Q))
      DF <- rep(Inf, length(Q))
      valids <- (NcaseA >= 1) & (NcontrolA >= 1) & (NcaseB >= 1) & (NcontrolB >= 1)
      useTtest = FALSE
    }
    
    validSites = (!is.na(Q)) & !is.na(SE) & !is.infinite(SE) & valids
  } else {
    Mcase <- apply(Tcase, 1, function(x) mean(x, na.rm=T))
    Mcontrol <- apply(Tcontrol, 1, function(x) mean(x, na.rm=T))
    
    Q <- Mcase - Mcontrol;
    
    Ncase <- apply(Tcase, 1, function(x) nnzero(!is.na(x)))
    Ncontrol <- apply(Tcontrol, 1, function(x) nnzero(!is.na(x)))
    
    if((nCase >= MIN_NSAMPLES) & (nControl >= MIN_NSAMPLES) & apply_ttest == TRUE){
      Scase <- apply(Tcase, 1, function(x) sd(x, na.rm=T))
      # SEcase <- Scase / sqrt(Ncase)
      Scontrol <- apply(Tcontrol, 1, function(x) sd(x, na.rm=T))
      # SEcontrol <- Scontrol / sqrt(Ncontrol)
      
      ## Pooled standard deviations
      S = sqrt((Ncase - 1) * Scase^2 + (Ncontrol - 1) * Scontrol^2) / 
        sqrt(Ncase + Ncontrol - 2);
      se_factor = sqrt(1/Ncase + 1/Ncontrol)
      DF = Ncase + Ncontrol - 2
      
      ## Add moderated t-test here
      # moderated = TRUE
      if(moderated){
        fit = limma::squeezeVar(S^2, DF)
        S = sqrt(fit$var.post)
        DF = DF + fit$df.prior
      }
      
      SE = se_factor * S;
      
      # SE <- sqrt(SEcase^2 + SEcontrol^2)
      valids <- (Ncase >= MIN_NSAMPLES) & (Ncontrol >= MIN_NSAMPLES)
      useTtest = TRUE
    } else {
      SE <- rep(sd(Q, na.rm = T), length(Q))
      DF <- rep(Inf, length(Q))
      valids <- (Ncase >= 1) & (Ncontrol >= 1)
      useTtest = FALSE
    }
    validSites = (!is.na(Q)) & !is.na(SE) & !is.infinite(SE) & valids
  }
  
  Xv = Q[validSites]
  Sx = SE[validSites]
  DF = DF[validSites]
  ST = ST[validSites, ]
  
  #Scase = apply(Tcase, 2, function(x) sd(x, na.rm=T))
  return (list("Xv" = Xv, "Sx" = Sx, "ST"= ST, "DF" = DF, "validSites" = validSites, 
               "useTtest" = useTtest))
}

processed_dataset <- reactive({
  req(filtered_dataset())
  req(filtered_metadata())
  ds <- filtered_dataset()
  Tmeta <- filtered_metadata()
  fo_process_dataset(ds, Tmeta)
})

foPreprocessDataset <- function(ds){
  if(input$options_center_foldchanges){
    ds$Xv = ds$Xv - mean(ds$Xv, na.rm = T)
  }
  
  if(ds$useTtest){
    ## Update zcrit
    out = t2zstatistic(ds$Xv / ds$Sx, ds$DF, zcrit = 4)
    ds$Sx_inflated = ds$Sx * out$sd.inflationfactor
    sd.inflationfactor = out$sd.inflationfactor
  } else {
    sd.inflationfactor = rep(1, length(ds$Xv))
  }
  
  ds$sd.inflationfactor = sd.inflationfactor
  return (ds)
}

preprocessed_dataset <- reactive({
  req(processed_dataset())
  ds <- processed_dataset()
  return(foPreprocessDataset(ds))
})

preprocessed_expression_dataset <- reactive({
  req(filtered_dataset())
  req(filtered_metadata())
  ds <- filtered_dataset()
  Tmeta <- filtered_metadata()
  ds <- fo_process_dataset(ds, Tmeta, type = "expression")
  return(foPreprocessDataset(ds))
  # if(input$options_center_foldchanges){
  #   ds$Xv = ds$Xv - mean(ds$Xv, na.rm = T)
  # }
  # 
  # if(ds$useTtest){
  #   ## Update zcrit
  #   out = t2zstatistic(ds$Xv / ds$Sx, ds$DF, zcrit = 4)
  #   ds$Sx_inflated = ds$Sx * out$sd.inflationfactor
  #   sd.inflationfactor = out$sd.inflationfactor
  # } else {
  #   sd.inflationfactor = rep(1, length(ds$Xv))
  # }
  # 
  # ds$sd.inflationfactor = sd.inflationfactor
  # return (ds)
})



