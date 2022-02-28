siteDownloadPlotDLHandler <- function(plot, file_name, file_type){
  downloadHandler(
    filename = function() { paste(file_name, file_type, sep='.') },
    content = function(file) {
      h = 4.6
      message("fjjf")
      ggsave(file, plot = siteBarPlot(), device = file_type, width=3*h, height=h)
    },
    contentType = paste("application/", file_type, sep = "")
  )
}

siteBarPlot <- reactive({
  req(site_table_processed())
  ST <- site_table_processed()
  ST$NameX = ST$ProteinName
  ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
  ST$ID <- str_c(ST$NameX, ST$Position, sep = "-")
  minzscore = input$site_barplot_minzscore
  topk = input$site_barplot_maxitems
  yaxis = input$site_barplot_yaxis
  coloring = input$site_barplot_coloring
  #  yaxistxt_main = "Site Phosphorylation"
  show_significant_only = input$site_barplot_significant_only
  barplot(ST, minzscore, topk, yaxis, coloring, show_significant_only)
})

output$site_barplot_plot <- renderPlot({
  siteBarPlot()
})

output$site_barplot_downloadPlotPNG <- siteDownloadPlotDLHandler(
  siteBarPlot(), file_name = "site-barplot", file_type = "png")

output$site_barplot_downloadPlotPDF <- siteDownloadPlotDLHandler(
  siteBarPlot(), file_name = "site-barplot", file_type = "pdf")