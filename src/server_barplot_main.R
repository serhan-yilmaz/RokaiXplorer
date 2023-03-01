barplot <- function(K, minzscore, topk, yaxis, coloring, show_significant_only){
  if(is.null(yaxis)){
    yaxis = "Log2-FC"
  }
  Ks <- K[!is.na(K$Phos),]
  Ks <- Ks[abs(Ks$ZScore) >= minzscore,]
  if(show_significant_only == T){
    Ks <- Ks[Ks$isSignificant, ]
  }
  
  Ks$zmult = qt(0.05/2, Ks$DF)
  
  #zstar = max(minzscore, 3)
  zstar = 3
  si <- order(abs(Ks$Phos) - zstar*Ks$StdErr, decreasing = TRUE)
  #si <- order(abs(Ks$ZScore), decreasing = TRUE)
  
  valids <- si[1:min(topk, length(si))]
  Ks <- Ks[valids, ]
  
  si <- order(Ks$Phos, decreasing = TRUE)
  Ks <- Ks[si,]
  
  c_limit = 4
  Ks$ColoringVar = Ks$ZScore
  Ks$ColoringVar = pmax( -1*c_limit, pmin(Ks$ColoringVar, c_limit))
  
  significanceColoring = FALSE
  if(coloring == "Significance"){
    Ks$ColoringVar <- !Ks$isSignificant
    #Ks$ColoringVar <- ifelse(!Ks$isSignificant, "NotSig", ifelse(Ks$Phos > 0, "SigPos", "SigNeg"))
    significanceColoring = TRUE
  }
  
  ## Add custom color scaling - If needed
  ## Add XLabel Coloring - If needed
  
  Ks$Sorting = -1*Ks$Phos
  Ks$Yaxis = Ks$Phos
  # yaxisText = "Log2-FC"
  yaxisText = yaxis
  # yaxisText = yaxistxt_main
  showErrorBars = TRUE
  if(yaxis == "Z-Score"){
    Ks$Yaxis = Ks$ZScore
    Ks$Sorting = -1*Ks$ZScore
    yaxisText = "Z-Score"
    showErrorBars = FALSE
  }
  
  p <- ggplot(data=Ks, aes(x=reorder(ID, Sorting), y=Yaxis, fill = ColoringVar)) +
    geom_bar(stat="identity", width=0.8, col="#333333", size = 0.75) +
    theme_minimal() +
    theme(text = element_text(size=18),
          axis.text.x = element_text(size = 16, angle=90, hjust=1, face = "bold"),
          legend.key.height = unit(1.25, "cm"))
  
  if(showErrorBars){ # Show errorabars
    p <- p + geom_errorbar(aes(ymin=Phos-zmult*StdErr, ymax=Phos+zmult*StdErr), width=.5, size = 0.95)
  }
  
  #  defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
  #clr <- c('#F10000', '#CCCCCC')
  clr <- c('#E69F00', '#CCCCCC')
  #clr <- c('#F10000', '#0000F1', '#CCCCCC')
  # clr <- c('#CCCCCC', '#F10000', '#0000F1')
  # , labels = c("Not Significant", "Significant Neg", "Significant Pos")
  
  if(significanceColoring){
    p <- p + scale_fill_manual(name = "", values=clr, labels = c("Significant", "Not Significant"))
  } else {
    p <- p + scale_fill_distiller(palette = "RdYlBu", type = "div", limit = c_limit * c(-1, 1))
  }
  
  p <- p + labs(fill = "Z-Score", x = "", y = yaxisText)
  return (p)
}