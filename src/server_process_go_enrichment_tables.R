
foDirectionCutoff <- function(phos, direction){
  switch(direction, 
         "Positive and Negative" = out <- (abs(phos) >= 0), 
         "Positive Only" = out <- (phos >= 0), 
         "Negative Only" = out <- (phos <= 0),
         stop("Invalid direction parameter")
  )
  return(out)
}

foEnrichmentBackgroundSet <- function(ST){
  NetworkData <- reactive_network()
  indices = match(ST$Protein, NetworkData$UniprotGene$ID)
  row_indices = 1:nrow(ST)
  valids = !is.na(indices)
  indices = indices[valids]
  row_indices = row_indices[valids]
  
  is_fdr_enabled = input$enrichment_fdrcorrection
  max_pvalue = input$enrichment_maxpvalue
  min_logfc = input$enrichment_minlogfc
  if(is_fdr_enabled){
    Pvals = ST$FDR
  } else {
    Pvals = ST$PValue
  }
  direction = input$enrichment_logfcdirection
  ST$isSignificant = (Pvals <= max_pvalue) & (abs(ST$Phos) >= min_logfc) & 
    foDirectionCutoff(ST$Phos, direction)
  
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
}

enrichment_background_protein_set <- reactive({
  req(reactive_network())
  switch(input$enrichment_datasource, 
         "Phosphosites" = {
           req(site_table_processed()); 
           ST <- site_table_processed()
           },
         "Phosphoproteins" = {
           req(protein_table_processed()); 
           ST <- protein_table_processed(); 
           ST$Protein = ST$ID; 
           },
         "Protein Expression" = {
           req(protexpression_table_processed());
           ST <- protexpression_table_processed();
         }, 
         stop("Invalid data source for enrichment")
  )
  return(foEnrichmentBackgroundSet(ST))
})

# 
# enrichment_background_protein_set <- reactive({
#   return(protein_with_phosphorylated_site_table())
# })

foPrepareEnrichmentTable <- function(out){
  NetworkData <- reactive_network()
  proteinIsSignificant = out$proteinIsSignificant[out$proteinIsIdentified]
  validGoterms <- colSums(NetworkData$Wuniprotgene2goterm) > 0
  Wuniprotgene2goterm_all = NetworkData$Wuniprotgene2goterm[, validGoterms]
  
  UniprotGene = NetworkData$UniprotGene[out$proteinIsIdentified, ]
  UniprotGene$isSignificant = proteinIsSignificant
  Wuniprotgene2goterm = Wuniprotgene2goterm_all[out$proteinIsIdentified, ]
  Wuniprotgene2goterm_significant = proteinIsSignificant %*% Wuniprotgene2goterm
  numProteinAll = colSums(Wuniprotgene2goterm_all)
  numIdentified = colSums(Wuniprotgene2goterm)
  numSignificant = colSums(Wuniprotgene2goterm_significant)
  # browser()
  n_identified = nrow(Wuniprotgene2goterm)
  n_significant = nnzero(proteinIsSignificant)
  
  # message(sprintf("NumIdentified: %d", n_identified))
  # message(sprintf("NumSignificant: %d", n_significant))
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
  
  correction_factor = F
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
  
  ### Option for centering
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
  
  if(input$enrichment_apply_yates_correction == TRUE){
    foChiSqr <- function(n, exp){
      (abs(n - exp) -  0.5)^2/exp ## With Yates's Correction
    }
  } else {
    foChiSqr <- function(n, exp){
      (n-exp)^2/exp ## Without Yates's Correction
    }
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
  logpvalues = pchisq(chi_squared, df=1, lower.tail=FALSE, log.p = T)
  pvalues = pchisq(chi_squared, df=1, lower.tail=FALSE)
  qvalues <- p.adjust(pvalues, method = "BH")
  
  Zeq = as.numeric(-1 * qnorm(logpvalues-log(2), log.p = T) * sign(log_risk_ratio))
  # Zeq = -qnorm(logpvalues, log.p = T)
  # Zeq = 1 - qnorm(logpvalues, log.p = T)
  
  # chi_squared <- suppressWarnings(apply(allnums,1, function(x) chisq.test(matrix(x,nr=2), correct = TRUE)$statistic))
  # allpvals <- suppressWarnings(apply(allnums,1, function(x) chisq.test(matrix(x,nr=2), correct = TRUE)$p.value))
  # allp_vals <- apply(allnums,1, function(x) fisher.test(matrix(x,nr=2))$p.value)
  # allfdr <- p.adjust(allp_vals, method = "BH")
  
  # res = compute_pvalues(as.matrix(Z)) ## Odds Ratio Test
  # res2 = compute_pvalues(as.matrix(Z2)) ## Odds Ratio Test
  
  GO = NetworkData$GO[validGoterms, c("ID", "Name", "Namespace")]
  colnames(GO)[3] <- "Category"
  GO$numProtein = numProteinAll
  GO$numIdentified = numIdentified
  GO$numSignificant = numSignificant
  GO$HitRatio <- paste0(GO$numSignificant, " / ", GO$numIdentified)
  GO$ObsRatio <- paste0(GO$numIdentified, " / ", GO$numProtein)
  # GO$LogOdds = as.matrix(log_odds)
  GO$LogRiskRatio = log_risk_ratio
  GO$StdErr = S_est
  GO$ZScore = as.matrix(Zeq)
  # GO$P_est = as.matrix(P_est)
  # GO$ZScore = as.matrix(Z_est)
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
  GO$Index = 1:nrow(GO)
  # elapsed <- (Sys.time() - startTime)
  # message(elapsed)
  
  # browser()
  
  return(list(GO = GO, Wuniprotgene2goterm = Wuniprotgene2goterm, Wuniprotgene2goterm_significant = Wuniprotgene2goterm_significant, UniprotGene = UniprotGene))
  
}

go_enrichment_table <- reactive({
  req(enrichment_background_protein_set())
  out <- enrichment_background_protein_set()
  return(foPrepareEnrichmentTable(out))
})

foEnrichmentCategories <- function(cats){
  out = cats
  index = 1
  for(cat in cats){
    x <- cat
    switch(cat, 
           "Cellular Component" = x <- "cellular_component",
           "Biological Process" = x <- "biological_process",
           "Molecular Function" = x <- "molecular_function"
    )
    out[[index]] = x
    index = index + 1
  }
  return(out)
}

foProcessEnrichmentTable <- function(GT){
  # max_fdr = input$kinaselevel_volcano_maxfdr
  # min_logfc = input$kinaselevel_volcano_minlogfc
  categories = foEnrichmentCategories(input$enrichment_categories)
  mintargets = input$enrichment_mintargets
  minobservedratio = input$enrichment_minobservedratio
  valids = !is.na(match(GT$Category, categories))
  valids = valids & (GT$numIdentified >= mintargets)
  ratios = GT$numIdentified / GT$numProtein
  valids = valids & (ratios >= minobservedratio/100)
  GT <- GT[valids, ]
  
  ### Filter by Overlap
  if(input$enrichment_filterbyoverlap == TRUE){
    maxoverlap = input$enrichment_maxoverlap/100
    Wuniprotgene2goterm = go_enrichment_table()$Wuniprotgene2goterm[, valids]
    # k = 20 ## MinHash Parameter
    
    # ### This is the minHash algorithm (not complete)
    # set.seed(1)
    # t = Sys.time()
    # for(iK in (1:k)){
    #   px = randperm(nprot)
    #   A = Wuniprotgene2goterm[px, ]
    #   qx = which(A, arr.ind = TRUE)
    #   a = match(1:nterm, qx[, 2])
    #   ix = qx[a, 1] ## These are the hMin values for minHash algorithm
    #   # ix <- apply(A,2,which.max); ## Slow way of doing it
    #   ib = order(ix)
    #   ixs = ix[ib]
    # }
    # Sys.time() - t
    
    # browser()
    
    t = Sys.time()
    cats = unique(GT$Category)
    valids_all = rep(TRUE, ncol(Wuniprotgene2goterm))
    for(iC in (1:length(cats))){
      cat = cats[iC]
      indices = GT$Category == cat
      
      Ntotal = GT$numProtein[indices]
      A = Wuniprotgene2goterm[, indices]
      nterm = ncol(A)
      nprot = nrow(A)
      C = t(A) %*% A; ## Overlaps
      U = sparseDiag(colSums(A)) %*% (C > 0) + 
        (C > 0) %*% sparseDiag(colSums(A)) - C
      qi = which(C>0)
      qx = which(C>0, arr.ind =T)
      J = C[qi] / U[qi]
      valids = qx[, 1] != qx[, 2]
      qx = qx[valids, ]
      J = J[valids]
      V = J >= maxoverlap
      Jv = J[V]
      qxx = qx[V, ]
      D = colSums(A) ## Num proteins for each term
      R = D / Ntotal ## Observance ratio for each term
      
      swap_indices = qxx[, 2] > qxx[, 1]
      temp = qxx[swap_indices, 1]
      qxx[swap_indices, 1] = qxx[swap_indices, 2]
      qxx[swap_indices, 2] = temp
      
      d1 = R[qxx[, 1]]
      d2 = R[qxx[, 2]]
      ##if (d1 < d2) get d1, otherwise get d2
      invalid_index = qxx[sub2ind(length(d1), 2, 1:length(d1), (d1 < d2) + 1)]
      valids = rep(TRUE, nterm)
      valids[invalid_index] = FALSE
      valids_all[indices] = valids
      
      # Names = GT$Name[indices]
      # for(iX in (1:nrow(qxx))){
      #   jaccard = round(Jv[iX],3)
      #   name1 = Names[qxx[iX, 1]]
      #   name2 = Names[qxx[iX, 2]]
      #   if(jaccard <= 0.9){
      #     message(paste0(name1, " - ", name2, " - ", jaccard, '\n'))
      #   }
      # }
      
    }
    GT = GT[valids_all, ]
    
    # message(Sys.time() - t)
    
    
    # nterm = ncol(Wuniprotgene2goterm)
    # nprot = nrow(Wuniprotgene2goterm)
    # C = t(Wuniprotgene2goterm) %*% Wuniprotgene2goterm; ## Overlaps
    # U = sparseDiag(colSums(Wuniprotgene2goterm)) %*% (C > 0) + 
    #   (C > 0) %*% sparseDiag(colSums(Wuniprotgene2goterm)) - C
    # qi = which(C>0)
    # qx = which(C>0, arr.ind =T)
    # J = C[qi] / U[qi]
    # valids = qx[, 1] != qx[, 2]
    # qx = qx[valids, ]
    # J = J[valids]
    # V = J >= maxoverlap
    # qxx = qx[V, ]
    # D = colSums(Wuniprotgene2goterm) ## Num proteins for each term
    # 
    # swap_indices = qxx[, 2] > qxx[, 1]
    # temp = qxx[swap_indices, 1]
    # qxx[swap_indices, 1] = qxx[swap_indices, 2]
    # qxx[swap_indices, 2] = temp
    # 
    # d1 = D[qxx[, 1]]
    # d2 = D[qxx[, 2]]
    # ##if (d1 < d2) get d1, otherwise get d2
    # invalid_index = qxx[sub2ind(length(d1), 2, 1:length(d1), (d1 < d2) + 1)]
    # valids = rep(TRUE, nterm)
    # valids[invalid_index] = FALSE
    # GT = GT[valids, ]
    
    
    # D =  * Cl + colSums(Wuniprotgene2goterm) * Cl
    # browser()
    
    
    # foX <- function(){
    #   
    #   px = randperm(nprot)
    #   A = Wuniprotgene2goterm[px, ]
    #   qx = which(A, arr.ind = TRUE)
    #   # qx[, 1] = px[qx[, 1]]
    #   # pp = order(qx[, 1] + qx[, 2]*(nprot+1))
    #   # qx = qx[pp, ]
    #   a = match(1:nterm, qx[, 2])
    #   b = qx[a, 1]
    # }
    
  }
  ### End - Filter by Overlap
  
  GT$FDR <- p.adjust(GT$PValue, method = "BH")
  
  max_fdr = input$pathwaylevel_volcano_maxfdr
  min_logfc = 0
  if(input$pathwaylevel_volcano_fdrcorrection == TRUE){
    pvals = GT$FDR
  } else {
    pvals = GT$PValue
  }
  GT$isSignificant = (pvals <= max_fdr) & (GT$LogRiskRatio >= min_logfc)
  return(GT)
}

go_enrichment_table_processed <- reactive({
  req(go_enrichment_table())
  GT <- go_enrichment_table()$GO
  return(foProcessEnrichmentTable(GT))
})

foPathwayTargetsTable <- function(ds, GT){
  UniprotGene = ds$UniprotGene
  Wuniprotgene2goterm = ds$Wuniprotgene2goterm[, GT$Index]
  Wuniprotgene2goterm_significant = ds$Wuniprotgene2goterm_significant[, GT$Index]
  
  indices = which(Wuniprotgene2goterm, arr.ind = T)
  i1 = indices[, 2]
  i2 = indices[, 1]
  
  KS = data.frame(
    ID = GT$ID[i1],
    Name = GT$Name[i1],
    Category = GT$Category[i1],
    numProtein = GT$numProtein[i1],
    numIdentified = GT$numIdentified[i1],
    numSignificant = GT$numSignificant[i1],
    LogRiskRatio = GT$LogRiskRatio[i1], 
    EffectiveMag = GT$EffectiveMag[i1], 
    ZScore = GT$ZScore[i1], 
    FDR = GT$FDR[i1], 
    isSignificant = GT$isSignificant[i1], 
    TargetID = UniprotGene$ID[i2],
    TargetProtein = UniprotGene$Gene[i2],
    IsHit = UniprotGene$isSignificant[i2]
  )
  
  return(KS)
}

foPrepareEnrichmentWithtargets <- function(KS, GT){
  KSv = KS[KS$IsHit, ]
  
  concatenated <- aggregate(TargetProtein ~ ID, KSv, function(x) paste(x, collapse = "; "));
  indices = match(concatenated$ID, GT$ID)
  values = rep(NA, nrow(GT))
  values[indices[!is.na(indices)]] = concatenated$TargetProtein[!is.na(indices)]
  GT$Hits = values;
  
  concatenated <- aggregate(TargetProtein ~ ID, KS, function(x) paste(x, collapse = "; "));
  indices = match(concatenated$ID, GT$ID)
  values = rep(NA, nrow(GT))
  values[indices[!is.na(indices)]] = concatenated$TargetProtein[!is.na(indices)]
  GT$AllTargets = values;
  
  return(GT)
}

pathway_targets_table <- reactive({
  req(go_enrichment_table())
  req(go_enrichment_table_processed())
  
  ds <- go_enrichment_table()
  GT <- go_enrichment_table_processed()
  KS = foPathwayTargetsTable(ds, GT)
  # GT = foPrepareEnrichmentWithtargets(KS, GT)
  # browser()
  
  return (KS)
})

go_enrichment_table_processed_withtargets <- reactive({
  req(pathway_targets_table())
  KS = pathway_targets_table()
  GT = go_enrichment_table_processed()
  GT = foPrepareEnrichmentWithtargets(KS, GT)
  return(GT)
})

foComputeEnrichmentTable <- function(ST){
  set = foEnrichmentBackgroundSet(ST)
  out = foPrepareEnrichmentTable(set)
  GT = foProcessEnrichmentTable(out$GO)
  KS = foPathwayTargetsTable(out, GT)
  GT = foPrepareEnrichmentWithtargets(KS, GT)
  return(GT)
}









