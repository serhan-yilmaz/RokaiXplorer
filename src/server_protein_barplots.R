## Protein Bar Plots

proteinDownloadPlotDLHandler <- function(plot, file_name, file_type){
  downloadHandler(
    filename = function() { paste(file_name, file_type, sep='.') },
    content = function(file) {
      h = 4.6
      message("fjjf")
      ggsave(file, plot = proteinBarPlot(), device = file_type, width=3*h, height=h)
    },
    contentType = paste("application/", file_type, sep = "")
  )
}

proteinBarPlot <- reactive({
  req(protein_table_processed())
  PT <- protein_table_processed()
  PT$NameX = PT$Name
  PT$NameX[is.na(PT$NameX)] = PT$ID[is.na(PT$NameX)]
  PT$ID <- PT$NameX
  minzscore = input$protein_barplot_minzscore
  topk = input$protein_barplot_maxitems
  yaxis = input$protein_barplot_yaxis
  coloring = input$protein_barplot_coloring
  #  yaxistxt_main = "Protein Phosphorylation"
  show_significant_only = input$protein_barplot_significant_only
  barplot(PT, minzscore, topk, yaxis, coloring, show_significant_only)
})

output$protein_barplot_plot <- renderPlot({
  proteinBarPlot()
})

output$protein_barplot_downloadPlotPNG <- proteinDownloadPlotDLHandler(
  proteinBarPlot(), file_name = "protein-barplot", file_type = "png")

output$protein_barplot_downloadPlotPDF <- proteinDownloadPlotDLHandler(
  proteinBarPlot(), file_name = "protein-barplot", file_type = "pdf")