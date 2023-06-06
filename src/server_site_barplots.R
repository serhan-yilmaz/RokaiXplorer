
siteBarPlot <- reactive({
  req(site_table_processed())
  ST <- site_table_processed()
  ST$NameX = ST$ProteinName
  ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
  ST$ID <- str_c(ST$NameX, ST$Position, sep = "-")
  minzscore = input$site_barplot_minzscore
  topk = input$site_barplot_maxitems
  yaxis = input$site_barplot_yaxis
  coloring = "Z-Score";
  # coloring = input$site_barplot_coloring
  #  yaxistxt_main = "Site Phosphorylation"
  show_significant_only = input$site_barplot_significant_only
  barplot(ST, minzscore, topk, yaxis, coloring, show_significant_only)
})

output$site_barplot_plot <- renderPlot({
  siteBarPlot()
})

output$site_barplot_downloadPlotPNG <- downloadPlotDLHandler(
  siteBarPlot, file_name = "phosphosite-barplot", file_type = "png")

output$site_barplot_downloadPlotPDF <- downloadPlotDLHandler(
  siteBarPlot, file_name = "phosphosite-barplot", file_type = "pdf")



protExpressionBarPlot <- reactive({
  req(protexpression_table_processed())
  ST <- protexpression_table_processed()
  ST$NameX = ST$ProteinName
  ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
  ST$ID <- ST$NameX
  minzscore = input$protexpression_barplot_minzscore
  topk = input$protexpression_barplot_maxitems
  yaxis = input$protexpression_barplot_yaxis
  coloring = "Z-Score";
  # coloring = input$protexpression_barplot_coloring
  #  yaxistxt_main = "Site Phosphorylation"
  show_significant_only = input$protexpression_barplot_significant_only
  barplot(ST, minzscore, topk, yaxis, coloring, show_significant_only)
})

output$protexpression_barplot_plot <- renderPlot({
  protExpressionBarPlot()
})

output$protexpression_barplot_downloadPlotPNG <- downloadPlotDLHandler(
  protExpressionBarPlot, file_name = "expression-barplot", file_type = "png")

output$protexpression_barplot_downloadPlotPDF <- downloadPlotDLHandler(
  protExpressionBarPlot, file_name = "expression-barplot", file_type = "pdf")


