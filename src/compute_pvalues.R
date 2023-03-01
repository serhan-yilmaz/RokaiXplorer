compute_pvalues <- function(zscores, DF = c()) {
  valids = !is.na(zscores);
  
  Z = zscores[valids]
  DF = DF[valids]
  
  if(is.empty(DF)){ 
    ## Use Normal distribution
    ncdf = pnorm(q=Z, lower.tail=FALSE)
  } else {
    ## Use T distribution
    ncdf = pt(q=Z, DF, lower.tail=FALSE)
  }
  
  pvals = 2 * pmin(ncdf, 1 - ncdf)
  qvals = p.adjust(pvals, method = "BH")
  
  P = rep(NA, length(zscores))
  Q = rep(NA, length(zscores))
  P[valids] = pvals
  Q[valids] = qvals
  
  res = list("PValues" = P, "QValues" = Q)
  return (res)
}

tstat2zscore <- function(tstats, DF){
  logpvals = pt(q=abs(as.matrix(tstats)), as.matrix(DF), lower.tail=FALSE, log.p = T)
  Z = as.numeric(-1 * qnorm(logpvals, log.p = T) * sign(tstats))
}

