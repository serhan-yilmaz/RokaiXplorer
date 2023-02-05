cache$cached_site_heatmap_select_group = reactiveVal("")

output$site_heatmap_select_group_ui <- renderUI({
  validate(
    need(metadata_ready(), "")
  )
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  selected = fo_restore_if_applicable(groups, isolate(foGetCacheValue("cached_site_heatmap_select_group")))
  
  tags$div(
    multiChoicePicker("site_heatmap_select_group", "Grouping:", groups, isInline = "F", multiple = T, max_opts = 1, selected = selected)
  )
})

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
  
  heatmapMain(ST, STx, ds, minzscore, topk, 
              show_significant_only, intensity_fc_style, "sites",
              groupings = groupings)
})

output$site_heatmap <- renderPlot({
  siteHeatmap()
})

output$site_heatmap_downloadPlotPNG <- downloadPlotDLHandler(
  siteHeatmap, file_name = "phosphosite-heatmap", file_type = "png")

output$site_heatmap_downloadPlotPDF <- downloadPlotDLHandler(
  siteHeatmap, file_name = "phosphosite-heatmap", file_type = "pdf")
