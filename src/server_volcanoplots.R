main_volcanoplot <- function(ST, min_logfc, group_txt){
  ST$logPvalue = pmin(-log10(ST$PValue), 10)
  ST$log2FC = paste(round(ST$Phos, 3), sep = "")
  ST$log10Pvalue = paste(round(ST$logPvalue, 3), sep = "")
  ST$ID = ST$Identifier
  ST$log10Pvalue[ST$logPvalue>=10] = list(">10")
  
  if(group_txt != "Kinase"){
    ST$NumSubs = rep(1, nrow(ST))
    xaxistext = "Log2-FC"
  } else {
    xaxistext = "Activity"
  }
  
  thr = min(ST$logPvalue[ST$isSignificant]);
  xMax = round(max(abs(ST$Phos))*1.05, digits = 1)
  
  defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
  
  multiplier = 0.7
  
  # clr <- c('#0072BD','#E69F00')
  clr <- defaultcolors[1:2]
  ggplot(ST, aes(x=Phos, y=logPvalue, color = isSignificant, id = ID, xx = log2FC, yy = log10Pvalue, zz = isSignificant, nn = NumSubs, text = paste("ID: ", Identifier), key = paste(Identifier, group_txt, sep = "_"))) + 
    theme_bw() +
    geom_point(size=3.5*multiplier) + 
    geom_hline(yintercept=(thr), linetype="dashed", color = '#555555') + 
    geom_vline(xintercept=c(-min_logfc, min_logfc), linetype="dashed", color = '#333333') + 
    
    theme(text = element_text(size = 18*multiplier)) + 
	#theme(text = element_text(size = 14)) + 
    labs(x = xaxistext, y = "-log10(P-Value)") + 
    scale_color_manual(name = "IsSignificant", values=clr, labels = c("Not Significant", "Significant")) +
    ylim(0, 10) + 
    xlim(-xMax, xMax)
  #+ theme(legend.position="top") 
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
  minlogfc = input$sitelevel_volcano_minlogfc
  plotly_volcano(main_volcanoplot(ST, minlogfc, "Phosphosite"))
})

output$proteinlevel_volcano <- renderPlotly({
  req(protein_table_processed())
  PT <- protein_table_processed()
  minlogfc = input$proteinlevel_volcano_minlogfc
  #ggplotly(main_volcanoplot(PT, minlogfc))
  plotly_volcano(main_volcanoplot(PT, minlogfc, "Protein"))
})

output$kinaselevel_volcano <- renderPlotly({
  req(kinase_table_processed())
  KT <- kinase_table_processed()
  KT$Phos = KT$Activity
  KT$Identifier = KT$KinaseName
  KT <- KT[!is.na(KT$Activity),]
  minlogfc = input$kinaselevel_volcano_minlogfc
  tooltip = c("ID", "nn", "xx", "yy", "zz")
  
  plotly_volcano(main_volcanoplot(KT, minlogfc, "Kinase"), tooltip)
})

output$sitelevel_volcano_summary <- renderText({
  req(site_table_processed())
  ST <- site_table_processed()
  return(paste("Number of significant sites:", nnzero(ST$isSignificant)))
})

output$proteinlevel_volcano_summary <- renderText({
  req(protein_table_processed())
  PT <- protein_table_processed()
  return(paste("Number of significant proteins:", nnzero(PT$isSignificant)))
})

output$kinaselevel_volcano_summary <- renderText({
  req(kinase_table_processed())
  KT <- kinase_table_processed()
  KT$isSignificant[is.na(KT$isSignificant)] = FALSE
  return(paste("Number of significant kinases:", nnzero(KT$isSignificant)))
})
