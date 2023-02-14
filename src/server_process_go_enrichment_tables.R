
protein_with_phosphorylated_site_table <- reactive({
  req(site_table_processed())
  req(reactive_network())
  NetworkData <- reactive_network()
  
  ST <- site_table_processed()
  indices = match(ST$Protein, NetworkData$UniprotGene$ID)
  row_indices = 1:nrow(ST)
  valids = !is.na(indices)
  indices = indices[valids]
  row_indices = row_indices[valids]
  
  Wsite2protein <- sparseMatrix(
    i = row_indices,
    j = indices, 
    x = 1,
    dims = c(nrow(ST), nrow(NetworkData$UniprotGene))
  )
  proteinIsIdentified = colSums(Wsite2protein) > 0
  proteinIsSignificant = (ST$isSignificant %*% Wsite2protein) > 0
  
  return(list("proteinIsIdentified" = proteinIsIdentified, 
              "proteinIsSignificant" = proteinIsSignificant))
})


go_enrichment_table <- reactive({
  req(protein_with_phosphorylated_site_table())
  
  # startTime <- Sys.time()
  
  out <- protein_with_phosphorylated_site_table()
  NetworkData <- reactive_network()
  proteinIsSignificant = out$proteinIsSignificant[out$proteinIsIdentified]
  validGoterms <- colSums(NetworkData$Wuniprotgene2goterm) > 0
  Wuniprotgene2goterm_all = NetworkData$Wuniprotgene2goterm[, validGoterms]
  Wuniprotgene2goterm = Wuniprotgene2goterm_all[out$proteinIsIdentified, ]
  Wuniprotgene2goterm_significant = proteinIsSignificant %*% Wuniprotgene2goterm
  numProteinAll = colSums(Wuniprotgene2goterm_all)
  numIdentified = colSums(Wuniprotgene2goterm)
  numSignificant = colSums(Wuniprotgene2goterm_significant)
  # browser()
  n_identified = nrow(Wuniprotgene2goterm)
  n_significant = nnzero(proteinIsSignificant)
  
  message(sprintf("NumIdentified: %d", n_identified))
  message(sprintf("NumSignificant: %d", n_significant))
  # remain = n_identified - n_significant - numIdentified + numSignificant
  # v <- c(numSignificant, numIdentified-numSignificant, n_significant - numSignificant, remain)
  # m <- matrix(v, nrow = 2)
  
  nSigIn = numSignificant ## m[1, 1]
  nSigOut = n_significant - nSigIn ## m[1, 2]
  nNotSigIn = numIdentified-numSignificant ## m[2, 1]
  nNotSigOut = n_identified - nSigIn - nSigOut - nNotSigIn ## m[2, 2]
  
  log_odds = log2((nSigIn * nNotSigOut) / (nSigOut * nNotSigIn))
  std_err = sqrt(1/nSigIn + 1/nSigOut + 1/nNotSigIn + 1/nNotSigOut)/log(2)
  
  r1 = nSigIn / (nSigIn + nNotSigIn)
  r2 = nSigOut / (nSigOut + nNotSigOut)
  n1 = (nSigIn + nNotSigIn)
  n2 = (nSigOut + nNotSigOut)
  risk_ratios = r1 / r2
  std_err_rr = sqrt((1 - r1) / nSigIn + (1 - r2) / nSigOut)/log(2)
  
  yx = (n_identified - n_significant) / n_significant
  ## This is the bayesian estimate of risk ratio (median of posteriori dist)
  r1_est = qbeta(0.5, nSigIn+1, nNotSigIn+yx)
  r2_est = qbeta(0.5, nSigOut+1, nNotSigOut+yx)
  rist_ratio_est = r1_est / r2_est
  log_risk_ratio = log2(rist_ratio_est)
  
  correction_factor = T
  if(correction_factor == TRUE){
    lor_mean = mean(log_risk_ratio, na.rm = T)
    log_risk_ratio = log_risk_ratio - lor_mean
    factor = 2^(lor_mean) ## Find the point where log_risk_ratio = lor_mean
  } else {
    factor = 1 ## Find the null point risk ratio = 1
  }
  
  P_est = pbeta(n_significant/n_identified * factor, nSigIn+1, nNotSigIn+yx)
  P_est = pmin(P_est, 1 - P_est)
  Z_est = sign(log_risk_ratio) * qnorm(P_est/2, lower.tail = FALSE);
  S_est = log_risk_ratio / Z_est
  
  # r1_min = qbeta(0.05, nSigIn+1, nNotSigIn+1)
  # r2_max = qbeta(0.95, nSigOut+1, nNotSigOut+1)
  # risk_ratio_min = r1_min / r2_max
  
  # std_est = 
  
  # log_odds = log2(m[1, 1] * m[2, 2] / (m[1,2] * m[2, 1]))
  # std_err = (sum(sum(1/m))/log(2))
  # invalids = is.infinite(log_odds) | is.infinite(std_err)
  # log_odds[invalids] = NA
  # std_err[invalids] = NA
  
  lor_mean = mean(log_risk_ratio, na.rm = T)
  # message(paste0("logRR mean: ", lor_mean))
  
  # log_odds = log_odds - lor_mean
  # browser()
  
  Z = (log_odds) / std_err
  
  # Z = (log_odds - lor_mean) / std_err
  
  # Z = log_odds / std_err
  # Z2 = (log_odds - lor_mean) / std_err
  
  allnums <- cbind(nSigIn, nSigOut, nNotSigIn, nNotSigOut)
  
  rSig = n_significant / n_identified
  rIn = numIdentified / n_identified
  
  expectedSigIn = n_identified * rSig * rIn
  expectedSigOut = n_identified * rSig * (1 - rIn)
  expectedNotSigIn = n_identified * (1 - rSig) * rIn
  expectedNotSigOut = n_identified * (1 - rSig) * (1 - rIn)
  foChiSqr <- function(n, exp){
    (abs(n - exp) -  0.5)^2/exp ## With Yates Correction
    # (n-exp)^2/exp ## With Yates Correction
  }
  # message(sprintf("nSignificant: %d", n_significant))
  # message(sprintf("nIdentified: %d", n_identified))
  chi_squared = foChiSqr(nSigIn, expectedSigIn) + 
                foChiSqr(nSigOut, expectedSigOut) + 
                foChiSqr(nNotSigIn, expectedNotSigIn) + 
                foChiSqr(nNotSigOut, expectedNotSigOut)
  valids = (expectedSigIn > 0) & (expectedSigOut > 0) & 
          (expectedNotSigIn > 0) & (expectedNotSigOut > 0)
  chi_squared[!valids] = NaN
  pvalues = pchisq(chi_squared, df=1, lower.tail=FALSE)
  qvalues <- p.adjust(pvalues, method = "BH")
  
  # chi_squared <- suppressWarnings(apply(allnums,1, function(x) chisq.test(matrix(x,nr=2), correct = TRUE)$statistic))
  # allpvals <- suppressWarnings(apply(allnums,1, function(x) chisq.test(matrix(x,nr=2), correct = TRUE)$p.value))
  # allp_vals <- apply(allnums,1, function(x) fisher.test(matrix(x,nr=2))$p.value)
  # allfdr <- p.adjust(allp_vals, method = "BH")
  
  # res = compute_pvalues(as.matrix(Z)) ## Odds Ratio Test
  # res2 = compute_pvalues(as.matrix(Z2)) ## Odds Ratio Test
  
  GO = NetworkData$GO[validGoterms, c("ID", "Name", "Namespace")]
  GO$numProtein = numProteinAll
  GO$numIdentified = numIdentified
  GO$numSignificant = numSignificant
  # GO$LogOdds = as.matrix(log_odds)
  GO$LogRiskRatio = log_risk_ratio
  GO$StdErr = S_est
  # GO$P_est = as.matrix(P_est)
  GO$ZScore = as.matrix(Z_est)
  # GO$StdErr = as.matrix(std_err)
  # GO$ZScore = as.matrix(Z)
  # GO$ChiSquared = allchi2
  GO$ChiSqr = chi_squared
  # GO$PValueA = res$PValues
  # GO$PValueB = res2$PValues
  GO$PValue = pvalues
  GO$FDR = qvalues
  # GO$Pvalue2 = pvalues
  # GO$PValue = res$PValues
  # GO$FDR = res$QValues
  # GO$PValue = allp_vals
  # GO$FDR = allfdr
  GO$MagnitudeAdj <- GO$LogRiskRatio - 2 * GO$StdErr;
  GO$EffectiveMag = pmax(GO$MagnitudeAdj, 0)
  # elapsed <- (Sys.time() - startTime)
  # message(elapsed)
  
  # browser()
  
  return(GO)
})

go_enrichment_table_processed <- reactive({
  req(go_enrichment_table())
  GT <- go_enrichment_table()
  # max_fdr = input$kinaselevel_volcano_maxfdr
  # min_logfc = input$kinaselevel_volcano_minlogfc
  max_fdr = 0.1
  min_logfc = 0
  GT$isSignificant = (GT$FDR <= max_fdr) & (GT$LogRiskRatio >= min_logfc)
  return(GT)
})











