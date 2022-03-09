siteDownloadHeatmapDLHandler <- function(plot, file_name, file_type){
  downloadHandler(
    filename = function() { paste(file_name, file_type, sep='.') },
    content = function(file) {
      h = 4.6
      ggsave(file, plot = siteHeatmap(), device = file_type, width=3*h, height=h)
    },
    contentType = paste("application/", file_type, sep = "")
  )
}

output$site_heatmap_select_group_ui <- renderUI({
  validate(
    need(metadata_ready(), "")
  )
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  tags$div(
    multiChoicePicker("site_heatmap_select_group", "Grouping:", groups, isInline = "F", multiple = T, max_opts = 1)
  )
})

output$site_heatmap_downloadPlotPNG <- siteDownloadHeatmapDLHandler(
  siteHeatmap(), file_name = "site-heatmap", file_type = "png")

output$site_heatmap_downloadPlotPDF <- siteDownloadHeatmapDLHandler(
  siteHeatmap(), file_name = "site-heatmap", file_type = "pdf")

siteHeatmap <- reactive({
  req(processed_data_bysample())
  ds <- processed_data_bysample()
  
  STx <- site_table_processed()
  STx$NameX = STx$ProteinName
  STx$NameX[is.na(STx$NameX)] = STx$Protein[is.na(STx$NameX)]
  STx$ID <- str_c(STx$NameX, STx$Position, sep = "-")
  
  ST <- ds$ST
  ST$NameX = ST$ProteinName
  ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
  ST$ID <- str_c(ST$NameX, ST$Position, sep = "-")
  
  minzscore = input$site_heatmap_minzscore
  topk = input$site_heatmap_maxitems
  show_significant_only = input$site_heatmap_significant_only
  intensity_fc_style = input$site_heatmap_intensity_fc_style
  groupings = input$site_heatmap_select_group
  
  message(input$site_heatmap_coloring)
  message(class(input$site_heatmap_coloring))
  heatmapMain(ST, STx, ds, minzscore, topk, 
              show_significant_only, intensity_fc_style, "sites",
              groupings = groupings)
})

output$site_heatmap <- renderPlot({
  siteHeatmap()
})