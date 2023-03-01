fo_process_kinase_data_bysample <- function(ds){
  
  ds$Xv = ds$Xv - mean(ds$Xv, na.rm = T)
  
  validSites = !is.na(ds$ST$NetworkDataIndex)
  Tmeta = ds$Tmeta
  Xv = ds$Xv[validSites, ]
  Sx = ds$Sx[validSites, ]
  ST = ds$ST[validSites, ]
  Ts = ds$Ts[validSites, ]
  
  networkDataIndices = ST$NetworkDataIndex
  
  NetworkData <- reactive_network()
  Wk2s <- selected_ks_network()
  nSite = ncol(Wk2s)
  
  wk2s = Wk2s[, networkDataIndices];
  
  # browser()
  nSubs = (wk2s %*% matrix(1, nrow(Xv), 1))
  
  #input$rokaiNetwork
  switch("KinaseSubstrate", 
         "KinaseSubstrate" = ropts <- list("ppi" = F, "sd" = F, "coev" = F),
         "KS+PPI" = ropts <- list("ppi" = T, "sd" = F, "coev" = F),
         "KS+PPI+SD" = ropts <- list("ppi" = T, "sd" = T, "coev" = F),
         "KS+PPI+SD+CoEv" = ropts <- list("ppi" = T, "sd" = T, "coev" = T))
  
  
  #input$rokaiEnabled
  rokaiEnabled = FALSE
  if(rokaiEnabled){
    if(ropts$ppi){
      Wk2k = NetworkData$net$Wkin2kin * 1e-3
    } else {
      Wk2k = NULL
    }
    Ws2s = sparseMatrix(
      i = c(),
      j = c(), 
      x = T,
      dims = c(nSite, nSite)
    )
    if(ropts$sd){
      Ws2s = Ws2s | NetworkData$net$Wsite2site.sd
    }
    if(ropts$coev){
      Ws2s = Ws2s | NetworkData$net$Wsite2site.coev
    }
    Ws2s = Ws2s[networkDataIndices, networkDataIndices]
    
    rc <- rokai_core(Xv, Sx, wk2s, Wk2k, Ws2s)
    Fk = rokai_kinase_weights(Xv, wk2s, rc$F)
    ri <- rokai_inference(Xv, Sx, Fk)
    A <- ri$A
    S <- ri$S
    Z <- ri$Z
  } else {
    Xvp = Xv
    Xvp[is.na(Xvp)] = 0
    Ina = !is.na(Xv)
    Sxp = Sx
    Sxp[is.na(Sxp)] = 0
    A <- as.matrix((wk2s %*% Xvp) / (wk2s %*% Ina))
    # A <- as.numeric((wk2s %*% Xv) / nSubs)
    S = as.matrix(sqrt((wk2s^2)%*%(Sxp^2)) / (wk2s %*% Ina))
    # S = as.numeric(sqrt((wk2s^2)%*%(Sx^2)) / nSubs)
    Z = as.numeric(A / S)
    # A = (wk2s %*% Xv) / nSubs
    # S = sd(Xv) / sqrt(nSubs)
    # Z = A / S
  }
  Kinase = NetworkData$Kinase
  Kinase$Identifier = Kinase$KinaseName
  
  Tsp = Ts
  Tsp[is.na(Tsp)] = 0
  Ina_ts = !is.na(Ts)

  Tp <- as.matrix((wk2s %*% Tsp) / (wk2s %*% Ina_ts))
  
  # ST <- ds$ST
  # Xv <- ds$Xv
  # Sx <- ds$Sx
  # Ts <- ds$Ts
  # Tmeta = ds$Tmeta
  # 
  # proteins = unique(ST$Protein)
  # indices = match(proteins, ST$Protein)
  # names <- ST$ProteinName[indices]
  # Protein <- data.frame(ID = proteins, Name = names)
  # nameX = Protein$Name
  # nameX[is.na(nameX)] = Protein$ID[is.na(nameX)]
  # Protein$Identifier = nameX
  # 
  # 
  # indices = match(ST$Protein, Protein$ID)
  # Wprotein2site <- sparseMatrix(
  #   i = indices,
  #   j = 1:nrow(ST), 
  #   x = TRUE,
  #   dims = c(nrow(Protein), nrow(ST))
  # )
  # 
  # Xvp = Xv
  # Xvp[is.na(Xvp)] = 0
  # Ina = !is.na(Xv)
  # Sxp = Sx
  # Sxp[is.na(Sxp)] = 0
  # 
  # # A <- as.matrix((Wprotein2site %*% Xv) / rowSums(Wprotein2site))
  # A <- as.matrix((Wprotein2site %*% Xvp) / (Wprotein2site %*% Ina))
  # #SE = as.matrix(sqrt((Wprotein2site^2)%*%(Sx^2)) / rowSums(Wprotein2site))
  # SE = as.matrix(sqrt((Wprotein2site^2)%*%(Sxp^2)) / (Wprotein2site %*% Ina))
  # Z = as.matrix(A / SE)
  # 
  # Tsp = Ts
  # Tsp[is.na(Tsp)] = 0
  # Ina_ts = !is.na(Ts)
  # 
  # Tp <- as.matrix((Wprotein2site %*% Tsp) / (Wprotein2site %*% Ina_ts))
  
  #Navailable <- apply(A, 1, function(x) nnzero(!is.na(x)))
  
  return (list("Xv" = A, "Sx" = S, "Ts" = Tp, "KT"= Kinase, "Tmeta" = Tmeta))
}

processed_kinase_data_bysample <- reactive({
  req(processed_data_bysample())
  ds <- processed_data_bysample()
  return(fo_process_kinase_data_bysample(ds))
})

processed_kinase_data_bysample_unfiltered <- reactive({
  req(processed_data_bysample_unfiltered())
  ds <- processed_data_bysample_unfiltered()
  return(fo_process_kinase_data_bysample(ds))
})