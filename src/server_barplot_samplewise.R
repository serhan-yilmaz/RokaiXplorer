boxplot_drawer <- function(Ks, mainGroup, opts){
  
  if(is.null(opts$showSampleNames)){
    opts$showSampleNames = TRUE
  }
  
  # if(opts$show_intensity){
  #   Ks = Ks[Ks$GroupingMain == mainGroup, ] 
  # }
  # 
  # browser()
  
  # geom_bar(stat="identity", width=0.8, col="#333333", size = 0.75) +
  # p <- ggplot(data=Ks, aes(x=reorder(ID, Sorting), y=Yaxis)) +
  # p <- ggplot(data=Ks, aes(x=reorder(ID, Sorting), y=Yaxis)) +
  p <- ggplot(data=Ks, aes(x=reorder(GroupingCombined, Sorting), y=Yaxis, fill = GroupingCombined)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=2, alpha=0.9) +
    stat_boxplot(geom = 'errorbar') +
    theme(legend.position="none") + 
    xlab("") + 
    ylab(opts$yaxisText)
    # theme_minimal() +
    # theme(text = element_text(size=18),
    #       axis.text.x = element_text(size = 15, angle=90, hjust=1, face = "bold"))
  
  
  
  if(opts$showErrorBars){ 
    p <- p + geom_errorbar(aes(ymin=Phos-1.96*StdErr, ymax=Phos+1.96*StdErr), width=.5, size = 0.95)
  }
  
  # p <- p + scale_fill_distiller(palette = "RdYlBu", type = "div", limit = opts$c_limit * c(-1, 1))
  # p <- p + labs(fill = "Z-Score", x = "", y = opts$yaxisText)
  
  

  
  
  # p <- p + geom_hline(yintercept = 0, color = "#333333")
  # p <- p + ylim(opts$yMin, opts$yMax)
  if(opts$bothCaseControl){
    # p <- p + facet_grid(rows = vars(GroupingMain), scales='free')
    # if(opts$nGrouping == 1){
    #   p <- p + facet_grid(rows = vars(GroupingMain), cols = vars(Grouping1), scales='free')
    # }
    # if(opts$nGrouping >= 2){
    #   p <- p + facet_grid(rows = vars(GroupingMain), cols = vars(Grouping1, Grouping2), scales='free')
    # }
    p <- p + facet_grid(cols = vars(GroupingMain), scales='free')
    if(opts$nGrouping == 1){
      p <- p + facet_grid(cols = vars(GroupingMain, Grouping1), scales='free')
    }
    if(opts$nGrouping >= 2){
      p <- p + facet_grid(cols = vars(GroupingMain, Grouping1, Grouping2), scales='free')
    }
  }else {
    if(opts$nGrouping == 1){
      p <- p + facet_grid(cols = vars(Grouping1), scales='free')
    }
    if(opts$nGrouping >= 2){
      p <- p + facet_grid(cols = vars(Grouping1, Grouping2), scales='free')
    }
  }
  
  # p = p + theme(strip.text.x = element_text(size = 16))
  #p = p + theme(strip.background = element_rect(color = "#000000", fill = "#F9F9B7"))
  # p = p + theme(strip.background = element_rect(color = "#AAAAAA", fill = "#FDFDF3"))
  
  # p <- p+ facet_wrap(~GroupingMain, scale="free_x") +
  p <- p+
    theme_economist() +
    theme(strip.text.x = element_text(size=20, angle=0,face="bold"),
          strip.background = element_rect(colour=c("grey7"), fill=c("cornsilk2")),
          plot.background = element_rect("white"),
          legend.title = element_text(color = "black", face="bold",size = 1),
          axis.title = element_text(color = "black", size = 18),
          plot.title = element_text(size=40, face="bold",hjust = 0.5))
  
  if(!opts$showSampleNames){
    p = p + theme(axis.text.x = element_blank())
    p = p + theme(legend.position="bottom")
  }
  
  return(p)
}

barplot_drawer <- function(Ks, mainGroup, opts){
  
  if(is.null(opts$showSampleNames)){
    opts$showSampleNames = TRUE
  }
  
  # if(opts$show_intensity){
  #   Ks = Ks[Ks$GroupingMain == mainGroup, ] 
  # }
  # 
  p <- ggplot(data=Ks, aes(x=reorder(ID, Sorting), y=Yaxis, fill = ColoringVar)) +
    geom_bar(stat="identity", width=0.8, col="#333333", size = 0.75) +
    theme_minimal() +
    theme(text = element_text(size=18),
          axis.text.x = element_text(size = 15, angle=90, hjust=1, face = "bold"))
  
  
  
  if(opts$showErrorBars){ 
    p <- p + geom_errorbar(aes(ymin=Phos-1.96*StdErr, ymax=Phos+1.96*StdErr), width=.5, size = 0.95)
  }
  
  p <- p + scale_fill_distiller(palette = "RdYlBu", type = "div", limit = opts$c_limits)
  p <- p + labs(fill = opts$coloringText, x = "", y = opts$yaxisText)
  
  
  if(!opts$showSampleNames){
    p = p + theme(axis.text.x = element_blank(), axis.title.x = element_text(size = 18))
    p = p + labs(x = "Samples")
  }
  
  p <- p + geom_hline(yintercept = 0, color = "#333333")
  p <- p + ylim(opts$yMin, opts$yMax)
  if(opts$bothCaseControl){
    # p <- p + facet_grid(rows = vars(GroupingMain), scales='free')
    # if(opts$nGrouping == 1){
    #   p <- p + facet_grid(rows = vars(GroupingMain), cols = vars(Grouping1), scales='free')
    # }
    # if(opts$nGrouping >= 2){
    #   p <- p + facet_grid(rows = vars(GroupingMain), cols = vars(Grouping1, Grouping2), scales='free')
    # }
    p <- p + facet_grid(cols = vars(GroupingMain), scales='free')
    if(opts$nGrouping == 1){
      p <- p + facet_grid(cols = vars(GroupingMain, Grouping1), scales='free')
    }
    if(opts$nGrouping >= 2){
      p <- p + facet_grid(cols = vars(GroupingMain, Grouping1, Grouping2), scales='free')
    }
  }else {
    if(opts$nGrouping == 1){
      p <- p + facet_grid(cols = vars(Grouping1), scales='free')
    }
    if(opts$nGrouping >= 2){
      p <- p + facet_grid(cols = vars(Grouping1, Grouping2), scales='free')
    }
  }
  
  p = p + theme(strip.text.x = element_text(size = 16))
  #p = p + theme(strip.background = element_rect(color = "#000000", fill = "#F9F9B7"))
  p = p + theme(strip.background = element_rect(color = "#AAAAAA", fill = "#FDFDF3"))
  
  return(p)
}


barplot_samplewise <- function(ds, ST, mds, groupings, item_txt, 
                               case_control_option, 
                               showSampleNames = T,
                               optx = list(),
							   plot_option = "barplot"){
  Tmeta <- ds$Tmeta
  # ST <- ds$ST
  not_identified_text = paste("This", tolower(item_txt), "is not identified in the experiment.", sep = " ")
  
  
  index = match(mds$identifier, ST$Identifier)
  validate(need(!is.na(index), not_identified_text))
  
  ST = ST[index, ]
  
  controlSamplesOnly = FALSE
  bothCaseControl = FALSE
  if(is.null(nrow(ds$Xv))){
    ds$Xv = matrix(ds$Xv, ncol = 1)
    ds$Sx = matrix(ds$Sx, ncol = 1)
  }
  # browser()
  if(case_control_option == "Case samples"){
    Xv = as.numeric(ds$Xv[index, ])
    Sx = as.numeric(ds$Sx[index, ])
    sample_names = colnames(ds$Xv)
    if(is.null(sample_names)){
      names = colnames(Tmeta$Tsample_metadata)
      sample_names = names[ds$Tmeta$caseSamples]
    }
    show_intensity = FALSE
    Z = Xv/Sx
  } else {
    Ts = ds$Ts
    #Ts = Ts - apply(Ts, 2, function(x) mean(x, na.rm=T))
    # Ts = Ts - apply(Ts, 1, function(x) mean(x, na.rm=T))
    sample_names = colnames(ds$Ts)
    show_intensity = TRUE
    Xv = as.numeric(Ts[index, ])
    Sx = as.numeric(apply(Ts, 2, function(x) sd(x, na.rm = T)))
    if(case_control_option == "Control samples"){
      controlSamplesOnly = TRUE
    } else {
      bothCaseControl = TRUE
    }
    Z = Xv/Sx
    # browser()
    # Xv_centered = Xv - apply(Ts, 2, function(x) mean(x, na.rm=T))
    # Z = Xv_centered /Sx
  }
  
  
  validate(need(sum(!is.na(Z))>0, not_identified_text))
  # browser()
  Ks = data.frame(ID = sample_names, Phos = Xv, StdErr = Sx, ZScore = Z)
  
  
  c_limit = 4
  Ks$ColoringVar = Ks$ZScore
  coloringText = "Z-Score"
  if(show_intensity == TRUE){
    c_limit = 3
    Ks$ColoringVar = Ks$Phos
    coloringText = "Log2-Intensity"
  }
  
  c_limits = c_limit * c(-1, 1)
  Ks$ColoringVar = pmax( -1*c_limit, pmin(Ks$ColoringVar, c_limit))
  
  Ks$Sorting = 1:nrow(Ks)
  Ks$Yaxis = Ks$Phos
  showErrorBars = FALSE
  
  if(show_intensity){
    yaxisText = "Log2-Intensity"
  } else {
    yaxisText = "Log2-FC"
    if(item_txt == "Kinase"){
      yaxisText = "Activity"
    }
    if(item_txt == "GOTerm"){
      yaxisText = "Enrichment (log2-RR)"
    }
  }
  
  indices = match(Ks$ID, colnames(Tmeta$Tsample_metadata))
  
  groupingMain = ifelse(Tmeta$caseSamples[indices], "Case", "Control")
  Ks$GroupingCombined = groupingMain
  Ks$GroupingMain = groupingMain
  nGrouping = length(groupings)
  for(iGroup in (1:nGrouping)){
    if(nGrouping <= 0){break;}
    grp_txt = paste("Grouping", iGroup, sep = "")
    Ks[[grp_txt]] = t(Tmeta$Tsample_metadata[groupings[iGroup], indices])
    Ks$GroupingCombined = paste(Ks$GroupingCombined, Ks[[grp_txt]], sep = "_")
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
  
  Sxp = Sx
  Sxp[is.infinite(Sxp)] = NA
  dyScaling = sqrt(mean(Sxp^2, na.rm = T))
  if(is.null(dyScaling) || is.na(dyScaling) || is.infinite(dyScaling)){
    dyScaling = 1
    # browser()
  }
  
  dyRange = abs(dyMax - dyMin)
  dyRangeMin = 2.5 * dyScaling
  if(identical(item_txt, "Kinase")){
    dyRangeMin = 1.5 * dyScaling
  }
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
  #show_intensity, yMin, yMax, showErrorBars, yaxisText, c_limit
  opts = list(show_intensity = show_intensity, yMin = yMin, yMax = yMax, 
              showErrorBars = showErrorBars, yaxisText = yaxisText, 
              c_limits = c_limits, nGrouping = nGrouping, bothCaseControl = bothCaseControl,
              showSampleNames = showSampleNames, coloringText = coloringText)
  
  if(controlSamplesOnly){
    Ks = Ks[Ks$GroupingMain == "Control", ] 
  }
  
  if(identical(plot_option, "barplot")){
    if(show_intensity == TRUE){
      Ks$StdErr = NULL
      Ks$ZScore = NULL
    }
    p1 <- barplot_drawer(Ks, "", opts)
  } else {
    Ks$StdErr = NULL
    Ks$ZScore = NULL
    Ks$ColoringVar = NULL
    Ks = Ks %>% relocate(Sorting, .before = ID)
    p1 <- boxplot_drawer(Ks, "", opts)
  }
  # p1 <- barplot_drawer(Ks, "", opts)
  #if(opts$show_intensity){
   # p2 <- barplot_drawer(Ks, "Control", opts)
    #return(p1 + p2 + plot_layout(nrow = 2))
  #}
  
  for(iGroup in prange(1, nGrouping)){
    grp_txt = paste("Grouping", iGroup, sep = "")
    colnames(Ks)[which(names(Ks) == grp_txt)] <- paste0("Grouping", iGroup, " (", groupings[iGroup], ")")
  }
  if(!is.null(optx$QColName)){
    colnames(Ks)[which(names(Ks) == "Phos")] <- optx$QColName
  }
  
  # colnames(Ks)[which(names(Ks) == "GroupingMain")] <- "GroupMain"
  
  # browser()
  
  return(list(plot = p1, plotdata = Ks, opts = opts))
}