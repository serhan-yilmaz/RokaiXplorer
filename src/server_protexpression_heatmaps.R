cache$cached_protexpression_heatmap_select_group = reactiveVal("")

output$protexpression_heatmap_select_group_ui <- renderUI({
  validate(
    need(metadata_ready(), "")
  )
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  selected = fo_restore_if_applicable(groups, isolate(foGetCacheValue("cached_protexpression_heatmap_select_group")))
  
  tags$div(
    multiChoicePicker("protexpression_heatmap_select_group", "Grouping:", groups, isInline = "F", multiple = T, max_opts = 1, selected = selected)
  )
})

protExpressionHeatmap <- reactive({
  req(processed_expression_data_bysample())
  ds <- processed_expression_data_bysample()
  
  STx <- protexpression_table_processed()
  STx$NameX = STx$ProteinName
  STx$NameX[is.na(STx$NameX)] = STx$Protein[is.na(STx$NameX)]
  STx$ID <- STx$NameX
  # STx$ID <- str_c(STx$NameX, STx$Position, sep = "-")
  
  ST <- ds$ST
  ST$NameX = ST$ProteinName
  ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
  ST$ID <- ST$NameX
  
  minzscore = input$protexpression_heatmap_minzscore
  topk = input$protexpression_heatmap_maxitems
  show_significant_only = input$protexpression_heatmap_significant_only
  intensity_fc_style = input$protexpression_heatmap_intensity_fc_style
  groupings = input$protexpression_heatmap_select_group
  
  heatmapMain(ST, STx, ds, minzscore, topk, 
              show_significant_only, intensity_fc_style, "proteins",
              groupings = groupings)
})

output$protexpression_heatmap <- renderPlot({
  protExpressionHeatmap()
})

output$protexpression_heatmap_downloadPlotPNG <- downloadPlotDLHandler(
  protExpressionHeatmap, file_name = "expression-heatmap", file_type = "png")

output$protexpression_heatmap_downloadPlotPDF <- downloadPlotDLHandler(
  protExpressionHeatmap, file_name = "expression-heatmap", file_type = "pdf")