## Protein heatmaps

cache$cached_kinase_heatmap_select_group = reactiveVal("")

output$kinase_heatmap_select_group_ui <- renderUI({
  validate(
    need(metadata_ready(), "")
  )
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  # browser()
  selected = fo_restore_if_applicable(groups, isolate(foGetCacheValue("cached_kinase_heatmap_select_group")))
  
  tags$div(
    multiChoicePicker("kinase_heatmap_select_group", "Grouping:", groups, isInline = "F", multiple = T, max_opts = 1, selected = selected)
  )
})

kinaseHeatmap <- reactive({
  req(processed_kinase_data_bysample())
  ds <- processed_kinase_data_bysample()
  
  PTx <- kinase_table_processed()
  PTx$ID <- PTx$KinaseName
  PTx$Phos <- PTx$Activity
  PTx = PTx[!is.na(PTx$ZScore), ]
  minsubs = input$kinase_heatmap_minsubs
  PTx = PTx[PTx$NumSubs >= minsubs, ]
  
  # PTx$NameX = PTx$Name
  # PTx$NameX[is.na(PTx$NameX)] = PTx$ID[is.na(PTx$NameX)]
  # PTx$ID <- PTx$NameX
  
  PT <- ds$KT
  PT$ID <- PT$KinaseName
  PT$Phos <- PT$Activity

  # browser()
  
  
  minzscore = input$kinase_heatmap_minzscore
  topk = input$kinase_heatmap_maxitems
  show_significant_only = input$kinase_heatmap_significant_only
  intensity_fc_style = input$kinase_heatmap_intensity_fc_style
  groupings = input$kinase_heatmap_select_group
  heatmapMain(PT, PTx, ds, minzscore, topk, 
              show_significant_only, intensity_fc_style, "kinases",
              groupings = groupings)
})

output$kinase_heatmap <- renderPlot({
  kinaseHeatmap()
})

kinase_heatmap_plot <- reactive({
  return(kinaseHeatmap()$plot)
})

kinase_heatmap_plot_data <- reactive({
  return(kinaseHeatmap()$plotdata)
})

output$kinase_heatmap <- renderPlot({
  kinase_heatmap_plot()
})


output$kinase_heatmap_downloadPlotPNG <- downloadPlotDLHandler(
  kinase_heatmap_plot, file_name = "kinase-heatmap", file_type = "png")

output$kinase_heatmap_downloadPlotPDF <- downloadPlotDLHandler(
  kinase_heatmap_plot, file_name = "kinase-heatmap", file_type = "pdf")

output$kinase_heatmap_downloadPlotDataExcel <- downloadExcelFileHandler(
  kinase_heatmap_plot_data, 
  file_name = "heatmap_data", 
  sheet_name = "Plot Data"
)