main_volcanoplot <- function(ST, min_logfc, maxfdr, group_txt, volcano_pmax = 8, show_legend = T, fdrcorrection = T){
  ST$logPvalue = pmin(-log10(ST$PValue), volcano_pmax)
  ST$log2FC = paste(round(ST$Phos, 3), sep = "")
  ST$log10Pvalue = paste(round(ST$logPvalue, 3), sep = "")
  ST$ID = ST$Identifier
  ST$log10Pvalue[ST$logPvalue>=volcano_pmax] = list(paste0(">", volcano_pmax))
  
  xaxistext = "Log2-FC"
  xx_name = "log2FC"
  group_txt_ = group_txt
  two_tailed = TRUE
  show_vline = TRUE
  if(group_txt == "Kinase"){
    xaxistext = "Activity"
    show_vline = FALSE
  } else {
    ST$NumSubs = rep(1, nrow(ST))
  }

  if(group_txt == "Pathway"){
    two_tailed = FALSE
    show_vline = FALSE
    ST$numProteins = ST$numIdentified
    ST$log2RR = ST$log2FC
    xx_name = "log2RR"
    xaxistext = "Enrichment (Log2-RR)"
    group_txt_ = "GOTerm"
    ST$Name = tools::toTitleCase(ST$Name)
    ST$Name2 = strTrim(strtrim(ST$Name, 24))
    if(nrow(ST) > 0){
      for(iX in 1:nrow(ST)){
        name1 = ST$Name[iX]
        name2 = ST$Name2[iX]
        if(nchar(name1) != nchar(name2)){
          ST$Name[iX] = paste0(name2, '...');
        } else {
          ST$Name[iX] = name2
        }
        ST$Category[iX] = gsub("_", " ", foGetGoCategoryText(ST$Category[iX]))
        
        numprots = ST$numProteins[iX]
        numsig = ST$numSignificant[iX]
        ratio = round(100 * numsig / numprots, 1)
        
        ST$numHits[iX] = paste0(numsig, ' / ', numprots, ' (', ratio, '%)');
      }
    }
    
  } else {
    ST$numProteins = rep(1, nrow(ST))
    ST$Name = rep(1, nrow(ST))
    ST$Category = rep(1, nrow(ST))
    ST$numHits = rep(1, nrow(ST))
  }
  
  if(fdrcorrection == TRUE){
    nSig = nnzero((ST$FDR <= maxfdr))
    nTest = nnzero(!is.na(ST$PValue))
    # thr = min(ST$logPvalue[ST$isSignificant]);
    thr = -log10((nSig + 1) * maxfdr / nTest)
  } else {
    thr = -log10(maxfdr)
  }

  xMax = ceil(max(abs(ST$Phos)), digits = 1)
  # xMax = ceil(max(abs(ST$Phos)), sigdigits = 2, digits = c())
  
  defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
  
  multiplier = 0.7
  
  # clr <- c('#0072BD','#E69F00')
  clr <- defaultcolors[1:2]
  
  p = ggplot(ST, aes(x=Phos, y=.data[["logPvalue"]], color = isSignificant, id = ID, name = Name, cat = Category, xx = .data[[xx_name]], yy = log10Pvalue, zz = isSignificant, nn = NumSubs, np = numProteins, nh = numHits, text = paste("ID: ", Identifier), key = paste(Identifier, group_txt_, sep = "_"))) + 
    theme_bw() +
    geom_point(size=3.5*multiplier) + 
    geom_hline(yintercept=(thr), linetype="dashed", color = '#555555') + 
    theme(text = element_text(size = 18*multiplier)) + 
	#theme(text = element_text(size = 14)) + 
    labs(x = xaxistext, y = "-log10(P-Value)") + 
    ylim(0, volcano_pmax) + 
    xlim(-xMax, xMax)
  # theme(legend.position="top") 

  p = p + scale_color_manual(name = "IsSignificant", values=clr, labels = c("Not Significant", "Significant"))
  
  if(show_vline){
    if(two_tailed){
      p = p + geom_vline(xintercept=c(-min_logfc, min_logfc), linetype="dashed", color = '#333333')
    } else {
      p = p + geom_vline(xintercept=c(min_logfc), linetype="dashed", color = '#333333')
    }
  }
  
  if(!show_legend){
    p = p + theme(legend.position = "none")
  }
  
  return(p)
}

plotly_volcano <- function(p, tooltip = c("ID", "xx", "yy", "zz")){
  px <- with_options(list(digits = 2, scipen = 3, nsmall = 2), ggplotly(p, tooltip = tooltip)) %>%
    onRender("
    function(el) { 
      el.on('plotly_hover', function(d) { 
        console.log('Hover: ', d); 
      });
      el.on('plotly_click', function(d) { 
        var index = d.points[0].pointIndex;
		txt = d.points[0].data.key[index]
        console.log('Click-Key: ', txt);
		Shiny.setInputValue('site_kinase_network_doubleclick', txt, {priority: 'event'});
      });
      el.on('plotly_selected', function(d) { 
        console.log('Select: ', d); 
      });
    }
  ")
}

output$sitelevel_volcano <- renderPlotly({
  req(site_table_processed())
  ST <- site_table_processed()
  identifier = "sitelevel_volcano_"
  minlogfc = input[[paste0(identifier, "minlogfc")]]
  maxfdr = input[[paste0(identifier, "maxfdr")]]
  fdrcorrection = input[[paste0(identifier, "fdrcorrection")]]
  showlegend = input[[paste0(identifier, "showlegend")]]
  plotly_volcano(main_volcanoplot(ST, minlogfc, maxfdr, "Phosphosite", show_legend = showlegend, fdrcorrection = fdrcorrection))
})


output$protexpression_volcano <- renderPlotly({
  req(protexpression_table_processed())
  ST <- protexpression_table_processed()
  
  identifier = "protexpression_volcano_"
  minlogfc = input[[paste0(identifier, "minlogfc")]]
  maxfdr = input[[paste0(identifier, "maxfdr")]]
  fdrcorrection = input[[paste0(identifier, "fdrcorrection")]]
  showlegend = input[[paste0(identifier, "showlegend")]]
  plotly_volcano(main_volcanoplot(ST, minlogfc, maxfdr, "Protein", show_legend = showlegend, fdrcorrection = fdrcorrection))
})

output$proteinlevel_volcano <- renderPlotly({
  req(protein_table_processed())
  PT <- protein_table_processed()
  
  identifier = "proteinlevel_volcano_"
  
  minlogfc = input[[paste0(identifier, "minlogfc")]]
  maxfdr = input[[paste0(identifier, "maxfdr")]]
  fdrcorrection = input[[paste0(identifier, "fdrcorrection")]]
  showlegend = input[[paste0(identifier, "showlegend")]]
  
  plotly_volcano(main_volcanoplot(PT, minlogfc, maxfdr, "Protein", show_legend = showlegend, fdrcorrection = fdrcorrection))
})

output$kinaselevel_volcano <- renderPlotly({
  req(kinase_table_processed())
  KT <- kinase_table_processed()
  KT$Phos = KT$Activity
  KT$Identifier = KT$KinaseName
  KT <- KT[!is.na(KT$Activity),]
  
  identifier = "kinaselevel_volcano_"
  
  # minlogfc = input$kinaselevel_volcano_minlogfc
  minlogfc = NULL
  maxfdr = input[[paste0(identifier, "maxfdr")]]
  fdrcorrection = input[[paste0(identifier, "fdrcorrection")]]
  showlegend = input[[paste0(identifier, "showlegend")]]
  tooltip = c("ID", "nn", "xx", "yy", "zz")
  
  plotly_volcano(main_volcanoplot(KT, minlogfc, maxfdr, "Kinase", volcano_pmax = 6, show_legend = showlegend, fdrcorrection = fdrcorrection), tooltip)
})

output$pathwaylevel_volcano <- renderPlotly({
  req(go_enrichment_table_processed())
  KT <- go_enrichment_table_processed()
  KT$Phos = KT$LogRiskRatio
  KT$Identifier = KT$ID
  KT <- KT[!is.na(KT$LogRiskRatio),]
  
  identifier = "pathwaylevel_volcano_"
  
  # minlogfc = input$pathwaylevel_volcano_minlogfc
  minlogfc = NULL
  maxfdr = input[[paste0(identifier, "maxfdr")]]
  fdrcorrection = input[[paste0(identifier, "fdrcorrection")]]
  showlegend = input[[paste0(identifier, "showlegend")]]
  tooltip = c("ID", "name", "cat", "nh", "xx", "yy", "zz")
  
  plotly_volcano(main_volcanoplot(KT, minlogfc, maxfdr, "Pathway", volcano_pmax = 6, show_legend = showlegend, fdrcorrection = fdrcorrection), tooltip)
})


output$sitelevel_volcano_summary <- renderText({
  req(site_table_processed())
  ST <- site_table_processed()
  # browser()
  return(paste("Significant phosphosites:", nnzero(ST$isSignificant)))
})

output$proteinlevel_volcano_summary <- renderText({
  req(protein_table_processed())
  PT <- protein_table_processed()
  return(paste("Significant proteins:", nnzero(PT$isSignificant)))
})

output$protexpression_volcano_summary <- renderText({
  req(protexpression_table_processed())
  PT <- protexpression_table_processed()
  return(paste("Significant proteins:", nnzero(PT$isSignificant)))
})

output$kinaselevel_volcano_summary <- renderText({
  req(kinase_table_processed())
  KT <- kinase_table_processed()
  KT$isSignificant[is.na(KT$isSignificant)] = FALSE
  return(paste("Significant kinases:", nnzero(KT$isSignificant)))
})

output$pathwaylevel_volcano_summary <- renderText({
  req(go_enrichment_table_processed())
  KT <- go_enrichment_table_processed()
  KT$isSignificant[is.na(KT$isSignificant)] = FALSE
  return(paste("Significant terms:", nnzero(KT$isSignificant)))
})
