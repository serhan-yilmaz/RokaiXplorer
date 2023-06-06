## Protein heatmaps

cache$cached_protein_heatmap_select_group = reactiveVal("")

output$protein_heatmap_select_group_ui <- renderUI({
  validate(
    need(metadata_ready(), "")
  )
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  # browser()
  selected = fo_restore_if_applicable(groups, isolate(foGetCacheValue("cached_protein_heatmap_select_group")))
  
  tags$div(
    multiChoicePicker("protein_heatmap_select_group", "Grouping:", groups, isInline = "F", multiple = T, max_opts = 1, selected = selected)
  )
})

# stop("ajjd")

proteinHeatmap <- reactive({
  req(processed_protein_data_bysample())
  ds <- processed_protein_data_bysample()
  
  # stop("xfjjkfd")
  
  PTx <- protein_table_processed()
  PTx$NameX = PTx$Name
  PTx$NameX[is.na(PTx$NameX)] = PTx$ID[is.na(PTx$NameX)]
  PTx$ID <- PTx$NameX
  
  PT <- ds$PT
  PT$NameX = PT$Name
  PT$NameX[is.na(PT$NameX)] = PT$ID[is.na(PT$NameX)]
  PT$ID <- PT$NameX
  
  minzscore = input$protein_heatmap_minzscore
  topk = input$protein_heatmap_maxitems
  show_significant_only = input$protein_heatmap_significant_only
  intensity_fc_style = input$protein_heatmap_intensity_fc_style
  groupings = input$protein_heatmap_select_group
  heatmapMain(PT, PTx, ds, minzscore, topk, 
              show_significant_only, intensity_fc_style, "proteins",
              groupings = groupings)
})

output$protein_heatmap <- renderPlot({
  proteinHeatmap()
})


output$protein_heatmap_downloadPlotPNG <- downloadPlotDLHandler(
  proteinHeatmap, file_name = "protein-heatmap", file_type = "png")

output$protein_heatmap_downloadPlotPDF <- downloadPlotDLHandler(
  proteinHeatmap, file_name = "protein-heatmap", file_type = "pdf")