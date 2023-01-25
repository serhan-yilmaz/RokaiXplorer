fo_process_goenrichment_bysample <- function(ds){
  # startTime <- Sys.time()
  Tmeta = ds$Tmeta
  Xv = ds$Xv
  Sx = ds$Sx
  ST = ds$ST
  
  NetworkData = reactive_network()
  indices = match(ST$Protein, NetworkData$UniprotGene$ID)
  row_indices = 1:nrow(ST)
  valids = !is.na(indices)
  indices = indices[valids]
  row_indices = row_indices[valids]
  
  Wprotein2site <- sparseMatrix(
    i = indices,
    j = row_indices, 
    x = 1,
    dims = c(nrow(NetworkData$UniprotGene), nrow(ST))
  )
  
  max_fdr = input$sitelevel_volcano_maxfdr
  min_logfc = input$sitelevel_volcano_minlogfc
  
  Zx = Xv / Sx
  res = compute_pvalues(as.matrix(Zx))
  FDR = matrix(res$QValues, nrow = nrow(Xv))
  
  Ssig = (FDR <= max_fdr) & (abs(Xv) >= min_logfc)
  I = !is.na(Ssig)
  Ssig[is.na(Ssig)] = FALSE
  
  Psig = as.matrix(Wprotein2site %*% Ssig) > 0
  Pidentified = as.matrix(Wprotein2site %*% I) > 0
  isProteinIdentified = rowSums(Pidentified) > 0
  
  Psig = Psig[isProteinIdentified, ]
  Pidentified = Pidentified[isProteinIdentified, ]
  
  validGoterms <- colSums(NetworkData$Wuniprotgene2goterm) > 0
  Wgoterm2uniprotgene = t(NetworkData$Wuniprotgene2goterm[isProteinIdentified, validGoterms])
  Wgoterm2sample_identified = (Wgoterm2uniprotgene %*% Pidentified)
  Wgoterm2sample_significant = (Wgoterm2uniprotgene %*% Psig)
  
  nCol = ncol(Xv)
  numIdentified = as.matrix(Wgoterm2sample_identified, ncol = nCol)
  numSignificant = as.matrix(Wgoterm2sample_significant, ncol  = nCol)
  
  validGoterms2 = as.matrix(rowSums(numIdentified), ncol = 1) > 0
  numIdentified = as.matrix(numIdentified[validGoterms2, ], ncol = nCol)
  numSignificant = as.matrix(numSignificant[validGoterms2, ], ncol  = nCol)
  
  n_identified = as.matrix(colSums(Pidentified), nrow = 1)
  n_significant = as.matrix(colSums(Psig), nrow = 1)
  
  # browser()
  
  nRow = nrow(numIdentified)
  
  
  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }
  rep.col<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
  }
  
  nSigIn = numSignificant ## m[1, 1]
  nSigOut = rep.row(n_significant, nRow) - nSigIn ## m[1, 2]
  nNotSigIn = numIdentified-numSignificant ## m[2, 1]
  nNotSigOut = rep.row(n_identified, nRow) - nSigIn - nSigOut - nNotSigIn ## m[2, 2]
  
  log_odds = suppressWarnings(log2((nSigIn * nNotSigOut) / (nSigOut * nNotSigIn)))
  std_err = suppressWarnings(sqrt(1/nSigIn + 1/nSigOut + 1/nNotSigIn + 1/nNotSigOut)/log(2))
  # invalids = is.infinite(log_odds) | is.infinite(std_err)
  # log_odds[invalids] = NA
  # std_err[invalids] = NA
  
  lor_means <- apply(log_odds, 2, function(x) mean(x, na.rm=T))
  # log_odds = log_odds - lor_means
  
  yx = rep.row((n_identified - n_significant) / n_significant, nRow)
  ## This is the bayesian estimate of risk ratio (median of posteriori dist)
  r1_est = as.matrix(qbeta(0.5, nSigIn+1, nNotSigIn+yx), ncol = nCol)
  r2_est = as.matrix(qbeta(0.5, nSigOut+1, nNotSigOut+yx), ncol = nCol)
  rist_ratio_est = r1_est / r2_est
  log_risk_ratio = log2(rist_ratio_est)
  
  correction_factor = T
  if(correction_factor == TRUE){
    lor_means <- apply(log_risk_ratio, 2, function(x) mean(x, na.rm=T))
    # lor_mean = mean(log_risk_ratio, na.rm = T)
    log_risk_ratio = log_risk_ratio - lor_means
    factor = 2^(lor_means) ## Find the point where log_risk_ratio = lor_mean
  } else {
    factor = 1 ## Find the null point risk ratio = 1
  }
  
  P_est = pbeta(rep.row(n_significant/n_identified, nRow) * factor, nSigIn+1, nNotSigIn+yx)
  P_est = pmin(P_est, 1 - P_est)
  Z_est = sign(log_risk_ratio) * qnorm(P_est/2, lower.tail = FALSE);
  S_est = log_risk_ratio / Z_est
  
  # browser()
  # log_odds = log_odds - lor_means
  
  GO = NetworkData$GO[validGoterms, ]
  GO = GO[validGoterms2, ]
  GO["Definition"] <- NULL
  GO$Identifier = GO$ID
  
  # valids = rowSums(!is.na(log_odds)) > 0
  # # valids = 
  # GO = GO[valids, ]
  # log_odds = log_odds[valids, ]
  # std_err = std_err[valids, ]
  
  # message(Sys.time() - startTime)
  
  return (list("Xv" = log_risk_ratio, "Sx" = S_est, "GO"= GO, "Tmeta" = Tmeta))
  # return (list("Xv" = log_odds, "Sx" = std_err, "GO"= GO, "Tmeta" = Tmeta))
}

processed_go_enrichment_bysample <- reactive({
  req(processed_data_bysample())
  ds <- processed_data_bysample()
  return(fo_process_goenrichment_bysample(ds))
})

processed_go_enrichment_bysample_unfiltered <- reactive({
  req(processed_data_bysample_unfiltered())
  ds <- processed_data_bysample_unfiltered()
  return(fo_process_goenrichment_bysample(ds))
})


