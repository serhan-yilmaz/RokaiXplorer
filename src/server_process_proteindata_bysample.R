fo_process_protein_data_bysample <- function(ds){
  ST <- ds$ST
  Xv <- ds$Xv
  Sx <- ds$Sx
  Ts <- ds$Ts
  Tmeta = ds$Tmeta
  proteins = unique(ST$Protein)
  indices = match(proteins, ST$Protein)
  names <- ST$ProteinName[indices]
  Protein <- data.frame(ID = proteins, Name = names)
  nameX = Protein$Name
  nameX[is.na(nameX)] = Protein$ID[is.na(nameX)]
  Protein$Identifier = nameX
  
  indices = match(ST$Protein, Protein$ID)
  Wprotein2site <- sparseMatrix(
    i = indices,
    j = 1:nrow(ST), 
    x = TRUE,
    dims = c(nrow(Protein), nrow(ST))
  )
  
  Xvp = Xv
  Xvp[is.na(Xvp)] = 0
  Ina = !is.na(Xv)
  Sxp = Sx
  Sxp[is.na(Sxp)] = 0
  
  # A <- as.matrix((Wprotein2site %*% Xv) / rowSums(Wprotein2site))
  A <- as.matrix((Wprotein2site %*% Xvp) / (Wprotein2site %*% Ina))
  #SE = as.matrix(sqrt((Wprotein2site^2)%*%(Sx^2)) / rowSums(Wprotein2site))
  SE = as.matrix(sqrt((Wprotein2site^2)%*%(Sxp^2)) / (Wprotein2site %*% Ina))
  Z = as.matrix(A / SE)
  
  Tsp = Ts
  Tsp[is.na(Tsp)] = 0
  Ina_ts = !is.na(Ts)
  
  Tp <- as.matrix((Wprotein2site %*% Tsp) / (Wprotein2site %*% Ina_ts))
  
  #Navailable <- apply(A, 1, function(x) nnzero(!is.na(x)))
  
  return (list("Xv" = A, "Sx" = SE, "Ts" = Tp, "PT"= Protein, "Tmeta" = Tmeta))
}

processed_protein_data_bysample <- reactive({
  req(processed_data_bysample())
  ds <- processed_data_bysample()
  return(fo_process_protein_data_bysample(ds))
})

processed_protein_data_bysample_unfiltered <- reactive({
  req(processed_data_bysample_unfiltered())
  ds <- processed_data_bysample_unfiltered()
  return(fo_process_protein_data_bysample(ds))
})