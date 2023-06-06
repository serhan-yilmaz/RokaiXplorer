## Protein Bar Plots

proteinBarPlot <- reactive({
  req(protein_table_processed())
  PT <- protein_table_processed()
  PT$NameX = PT$Name
  PT$NameX[is.na(PT$NameX)] = PT$ID[is.na(PT$NameX)]
  PT$ID <- PT$NameX
  minzscore = input$protein_barplot_minzscore
  topk = input$protein_barplot_maxitems
  yaxis = input$protein_barplot_yaxis
  # coloring = input$protein_barplot_coloring
  coloring = "Z-Score";
  #  yaxistxt_main = "Protein Phosphorylation"
  show_significant_only = input$protein_barplot_significant_only
  barplot(PT, minzscore, topk, yaxis, coloring, show_significant_only)
})

output$protein_barplot_plot <- renderPlot({
  proteinBarPlot()
})

output$protein_barplot_downloadPlotPNG <- downloadPlotDLHandler(
  proteinBarPlot, file_name = "protein-barplot", file_type = "png")

output$protein_barplot_downloadPlotPDF <- downloadPlotDLHandler(
  proteinBarPlot, file_name = "protein-barplot", file_type = "pdf")