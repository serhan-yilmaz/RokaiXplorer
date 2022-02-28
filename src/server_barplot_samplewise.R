barplot_samplewise <- function(ds, ST, mds, groupings, item_txt){
  Tmeta <- ds$Tmeta
  # ST <- ds$ST
  
  index = match(mds$identifier, ST$Identifier)
  validate(need(!is.na(index), paste("This", tolower(item_txt), "is not identified in the experiment.", sep = " ")))
  
  ST = ST[index, ]
  
  Xv = as.numeric(ds$Xv[index, ])
  Sx = as.numeric(ds$Sx[index, ])
  Z = Xv/Sx
  
  sample_names = colnames(ds$Xv)
  
  Ks = data.frame(ID = sample_names, Phos = Xv, StdErr = Sx, ZScore = Z)
  
  c_limit = 4
  Ks$ColoringVar = Ks$ZScore
  Ks$ColoringVar = pmax( -1*c_limit, pmin(Ks$ColoringVar, c_limit))
  
  Ks$Sorting = 1:nrow(Ks)
  Ks$Yaxis = Ks$Phos
  yaxisText = "Log2-FC"
  showErrorBars = FALSE
  
  indices = match(Ks$ID, colnames(Tmeta$Tsample_metadata))
  
  
  nGrouping = length(groupings)
  for(iGroup in range(1, nGrouping)){
    grp_txt = paste("Grouping", iGroup, sep = "")
    Ks[[grp_txt]] = t(Tmeta$Tsample_metadata[groupings[iGroup], indices])
  }
  # Ks$Grouping = t(Tmeta$Tsample_metadata["Timepoint", indices])
  
  dyMin = min(Ks$Yaxis, na.rm = T)
  dyMax = max(Ks$Yaxis, na.rm = T) 
  
  if((dyMin > 0) & (dyMax > 0)){
    dyMin = 0
  }
  
  if((dyMin < 0) & (dyMax < 0)){
    dyMax = 0
  }
  
  dyRange = abs(dyMax - dyMin)
  dyRangeMin = 2.5
  rangeDif = dyRangeMin - dyRange
  if(dyRange < dyRangeMin){
    if(dyMin == 0){ # All positive 
      dyMax = dyMax + rangeDif
    } else {
      if(dyMax == 0){
        dyMin = dyMin - rangeDif
      } else {
        dyMax = dyMax + rangeDif * 0.5
        dyMin = dyMin - rangeDif * 0.5
      }
    }
  }
  
  dyRange = abs(dyMax - dyMin)
  yMin = dyMin - (dyRange * 0.02)
  yMax = dyMax + (dyRange * 0.02)
  
  
  p <- ggplot(data=Ks, aes(x=reorder(ID, Sorting), y=Yaxis, fill = ColoringVar)) +
    geom_bar(stat="identity", width=0.8, col="#333333", size = 0.75) +
    theme_minimal() +
    theme(text = element_text(size=18),
          axis.text.x = element_text(size = 16, angle=90, hjust=1, face = "bold"))
  
  if(showErrorBars){ 
    p <- p + geom_errorbar(aes(ymin=Phos-1.96*StdErr, ymax=Phos+1.96*StdErr), width=.5, size = 0.95)
  }
  
  p <- p + scale_fill_distiller(palette = "RdYlBu", type = "div", limit = c_limit * c(-1, 1))
  p <- p + labs(fill = "Z-Score", x = "", y = yaxisText)
  p <- p + geom_hline(yintercept = 0, color = "#333333")
  p <- p + ylim(yMin, yMax)
  if(nGrouping == 1){
    p <- p + facet_grid(cols = vars(Grouping1), scales='free')
  }
  if(nGrouping >= 2){
    p <- p + facet_grid(cols = vars(Grouping1, Grouping2), scales='free')
  }
  
  p = p + theme(strip.text.x = element_text(size = 16))
  #p = p + theme(strip.background = element_rect(color = "#000000", fill = "#F9F9B7"))
  p = p + theme(strip.background = element_rect(color = "#AAAAAA", fill = "#FDFDF3"))
  
  return(p)
}