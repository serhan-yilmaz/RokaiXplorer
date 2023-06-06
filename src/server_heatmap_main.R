heatmapMain <- function(ST, STx, ds, minzscore, topk, show_significant_only, intensity_fc_style, items_txt, groupings = c()){
  show_intensity = intensity_fc_style == "Both case and control"
  
  #Z = ds$Xv/ds$Sx
  Z = as.matrix(ds$Xv)
  Ts <- ds$Ts
  Tmeta <- ds$Tmeta
  
  indices = match(ST$ID, STx$ID)
  valids = !is.na(indices)
  error_no_items_txt <- paste("There are no", items_txt, "to show for the specified options (at least two required).")
  validate(
    need(nnzero(valids) > 1, error_no_items_txt)
  )
  # browser()
  Z <- as.matrix(Z[valids, ])
  ST <- ST[valids, ]
  Ts <- Ts[valids, ]
  indices <- indices[valids]
  
  STv = STx[indices, ]
  
  # Ts = Ts - apply(Ts, 2, function(x) mean(x, na.rm=T))
  # Ts = Ts - apply(Ts, 1, function(x) mean(x, na.rm=T))
  
  #Ts <- as.data.frame(normalize.quantiles(as.matrix(Ts)))
  #Z[is.na(Z)] = 0
  
  
  rownames(Z) <- ST$ID
  colnames(Ts) <- colnames(ds$Ts)
  rownames(Ts) <- ST$ID
  
  # avgZ = apply(Z, 1, function(x) mean(x, na.rm=T))
  # avgAbsZ = apply(Z, 1, function(x) mean(abs(x), na.rm=T))
  sumAbsZ = apply(Z, 1, function(x) sum(abs(x), na.rm=T)) / ncol(Z)
  #valids = (abs(sumAbsZ) >= (2 * ncol(Z))) & (abs(avgZ) >= 1)
  # valids = STv$isSignificant
  #valids = rep(T, nrow(STv), 1) & (abs(STv$ZScore) >= minzscore)
  
  if(!identical(items_txt, "kinases")){
    valids = !is.na(sumAbsZ) & (abs(sumAbsZ) >= minzscore)
  } else {
    valids = !is.na(sumAbsZ) & (abs(STv$ZScore) >= minzscore)
  }
  
  # valids[is.na(sumAbsZ)] = FALSE
  #valids = !is.na(avgAbsZ) & (abs(avgAbsZ) >= minzscore)
  if(show_significant_only){
    valids = valids & (STv$isSignificant)
    # browser()
  }
  #& (abs(STv$ZScore) >= 3.5)
  
  validate(
    need(nnzero(valids) > 1, error_no_items_txt)
  )
  Z <- as.matrix(Z[valids, ])
  ST <- ST[valids, ]
  Ts <- Ts[valids, ]
  STv <- STv[valids, ]
  
  sort_score = abs(STv$Phos) - 3 * STv$StdErr
  #avgZ = apply(Z, 1, function(x) mean(x, na.rm=T))
  si <- order(sort_score, decreasing = TRUE)
  # topk = length(Z)
  valids <- si[1:min(topk, length(si))]
  
  Z <- as.matrix(Z[valids, ])
  ST <- ST[valids, ]
  STv <- STv[valids, ]
  Ts <- Ts[valids, ]
  
  valids <- order(STv$Phos, decreasing = TRUE)
  Z <- as.matrix(Z[valids, ])
  ST <- ST[valids, ]
  Ts <- Ts[valids, ]
  
  #caseSamples = !is.na(match(colnames(Ts), colnames(Z)))

  #groupings = c("Timepoint", "Gender")
  if(show_intensity){
    Tq <- Ts
    # Tq$caseSamples = caseSamples
    data_melt <- melt(Ts)
    data_melt$caseSamples <- ifelse(!is.na(match(data_melt$X2, colnames(Z))), "Case", "Control")
    
    #data_melt$Grouping = t(Tmeta$Tsample_metadata["Gender", indices])
    
    fill_txt = "Log2-Intensity"
  } else {
    data_melt <- melt(Z)
    fill_txt = "Log2-FC"
  }
  indices = match(data_melt$X2, colnames(Tmeta$Tsample_metadata))
  nGrouping = length(groupings)
  if(nGrouping > 0){
    for(iGroup in range(1, nGrouping)){
      grp_txt = paste("Grouping", iGroup, sep = "")
      data_melt[[grp_txt]] = t(Tmeta$Tsample_metadata[groupings[iGroup], indices])
    }
  }
  
  # data_melt[data_melt < -4] = -4
  # data_melt[data_melt > 4] = 4
  
  # defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
  
  ggp <- ggplot(data_melt, aes(X1, X2)) +                           # Create heatmap with ggplot2
    geom_tile(aes(fill = value))
  if(show_intensity){
    #ggp = ggp + facet_grid(caseSamples+Grouping~., scales='free', switch = "y")
    # ggp = ggp + facet_grid(caseSamples~., scales='free', switch = "y")
    
    if(nGrouping <= 0){
      ggp = ggp + facet_grid(rows = vars(caseSamples), scales='free', switch = "y")
    }
    if(nGrouping == 1){
      ggp = ggp + facet_grid(rows = vars(caseSamples, Grouping1), scales='free', switch = "y")
    }
    if(nGrouping >= 2){
      ggp = ggp + facet_grid(rows = vars(caseSamples, Grouping1, Grouping2), scales='free', switch = "y")
    }
  } else {
    if(nGrouping == 1){
      ggp = ggp + facet_grid(rows = vars(Grouping1), scales='free', switch = "y")
    }
    if(nGrouping >= 2){
      ggp = ggp + facet_grid(rows = vars(Grouping1, Grouping2), scales='free', switch = "y")
    }
  }
  
  ggp = ggp + theme(strip.text.y = element_text(size = 16))
  ggp = ggp + theme(strip.background = element_rect(color = "#000000", fill = "#F9F9B7"))
  
  ggp = ggp + scale_fill_gradient2(name = fill_txt, low = "blue", mid = "white", high = "red", na.value = "#336633")
  #ggp = ggp + scale_fill_gradientn(colours = c("blue", "white", "red"))
  
  #ggp = ggp + scale_fill_gradient(low = "white", high = "red", na.value = "#336633")
  ggp = ggp + theme(axis.text.x=element_text(angle =- 90, vjust = 0.5, hjust = 0, size=15))
  ggp = ggp + theme(axis.text.y=element_text(vjust = 0, hjust = 0.5, size=11))
  ggp = ggp + theme(legend.title = element_text(size = 16), legend.text = element_text(size = 14))
  ggp = ggp + labs(x = "", y = "")
  
  return(ggp)
}