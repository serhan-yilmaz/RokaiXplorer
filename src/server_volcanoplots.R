main_volcanoplot <- function(ST, min_logfc){
  ST$logP = pmin(-log10(ST$PValue), 10)
  
  thr = min(ST$logP[ST$isSignificant]);
  xMax = round(max(abs(ST$Phos)), digits = 0)
  
  defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
  
  # clr <- c('#0072BD','#E69F00')
  clr <- defaultcolors[1:2]
  ggplot(ST, aes(x=Phos, y=logP, color = isSignificant)) + 
    theme_bw() +
    geom_point(size=3.5) + 
    geom_hline(yintercept=(thr), linetype="dashed", color = '#555555') + 
    geom_vline(xintercept=c(-min_logfc, min_logfc), linetype="dashed", color = '#333333') + 
    
    theme(text = element_text(size = 18)) + 
    labs(x = "Log2-FC", y = "-log10(P-Value)") + 
    scale_color_manual(name = "", values=clr, labels = c("Not Significant", "Significant")) +
    ylim(0, 10) + 
    xlim(-xMax, xMax)
  #+ theme(legend.position="top") 
}

output$sitelevel_volcano <- renderPlot({
  req(site_table_processed())
  ST <- site_table_processed()
  minlogfc = input$sitelevel_volcano_minlogfc
  main_volcanoplot(ST, minlogfc)
})

output$proteinlevel_volcano <- renderPlot({
  req(protein_table_processed())
  PT <- protein_table_processed()
  minlogfc = input$proteinlevel_volcano_minlogfc
  main_volcanoplot(PT, minlogfc)
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