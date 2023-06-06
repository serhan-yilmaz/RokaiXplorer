## Kinase Bar Plots

kinaseBarPlot <- reactive({
  req(kinase_table_processed())
  KT <- kinase_table_processed()
  KT$ID <- KT$KinaseName
  KT$Phos <- KT$Activity
  minzscore = input$kinase_barplot_minzscore
  topk = input$kinase_barplot_maxitems
  yaxis = input$kinase_barplot_yaxis
  coloring = "Z-Score";
  # coloring = input$kinase_barplot_coloring
  show_significant_only = input$kinase_barplot_significant_only
  minsubs = input$kinase_barplot_minsubs
  KT = KT[KT$NumSubs >= minsubs, ]
  barplot(KT, minzscore, topk, yaxis, coloring, show_significant_only)
})

output$kinase_barplot_plot <- renderPlot({
  kinaseBarPlot()
})

output$kinase_barplot_downloadPlotPNG <- downloadPlotDLHandler(
  kinaseBarPlot, file_name = "kinase-barplot", file_type = "png")

output$kinase_barplot_downloadPlotPDF <- downloadPlotDLHandler(
  kinaseBarPlot, file_name = "kinase-barplot", file_type = "pdf")