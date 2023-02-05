## Depends barplot_samplewise

## Site samplewise barplot
modal_site_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  validate(need(input$mbox_site_plot_samples_case_control, ""))
  mds <- modal_box_selection_mapped()
  validate(need(mds$isSite, ""))
  req(processed_data_bysample_unfiltered())
  
  ds <- processed_data_bysample_unfiltered()
  groupings = input$mbox_site_plot_select_group
  case_control_option = input$mbox_site_plot_samples_case_control
  showSampleNames = input$mbox_site_plot_show_samples
  barplot_samplewise(ds, ds$ST, mds, groupings, "Site", case_control_option, 
                     showSampleNames = showSampleNames)
})

output$modal_site_samplewise_barplot <- renderPlot({
  modal_site_samplewise_barplot()
})

## Protein samplewise barplot
modal_protein_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  validate(need(input$mbox_site_plot_samples_case_control, ""))
  mds <- modal_box_selection_mapped()
  validate(need(mds$isProtein, ""))
  req(processed_protein_data_bysample_unfiltered())
  
  ds <- processed_protein_data_bysample_unfiltered()
  groupings = input$mbox_site_plot_select_group
  case_control_option = input$mbox_site_plot_samples_case_control
  showSampleNames = input$mbox_site_plot_show_samples
  barplot_samplewise(ds, ds$PT, mds, groupings, "Protein", case_control_option, 
                     showSampleNames = showSampleNames)
})


## Protein expression samplewise barplot
modal_protexpression_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  validate(need(input$mbox_site_plot_samples_case_control, ""))
  mds <- modal_box_selection_mapped()
  validate(need(mds$isProtein, ""))
  req(processed_expression_data_bysample_unfiltered())
  
  ds <- processed_expression_data_bysample_unfiltered()
  
  # browser()
  
  ds$ST$Identifier = ds$ST$ProteinName
  
  groupings = input$mbox_site_plot_select_group
  case_control_option = input$mbox_site_plot_samples_case_control
  showSampleNames = input$mbox_site_plot_show_samples
  barplot_samplewise(ds, ds$ST, mds, groupings, "Protein", case_control_option, 
                     showSampleNames = showSampleNames)
})

output$modal_protein_samplewise_barplot <- renderPlot({
  if(is_protein_modalbox_plot_showing_expression()){
    modal_protexpression_samplewise_barplot()
  } else {
    modal_protein_samplewise_barplot()
  }
})

## Kinase samplewise barplot
modal_kinase_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  validate(need(input$mbox_site_plot_samples_case_control, ""))
  mds <- modal_box_selection_mapped()
  validate(need(mds$isKinase, ""))
  req(processed_kinase_data_bysample_unfiltered())
  
  ds <- processed_kinase_data_bysample_unfiltered()
  groupings = input$mbox_site_plot_select_group
  case_control_option = input$mbox_site_plot_samples_case_control
  showSampleNames = input$mbox_site_plot_show_samples
  barplot_samplewise(ds, ds$KT, mds, groupings, "Kinase", case_control_option, 
                     showSampleNames = showSampleNames)
})

output$modal_kinase_samplewise_barplot <- renderPlot({
  modal_kinase_samplewise_barplot()
})

## Goterm samplewise barplot
modal_goenrichment_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  # validate(need(input$mbox_site_plot_show_samples, ""))
  mds <- modal_box_selection_mapped()
  validate(need(mds$isGOTerm, ""))
  req(processed_go_enrichment_bysample_unfiltered())
  
  ds <- processed_go_enrichment_bysample_unfiltered()
  groupings = input$mbox_site_plot_select_group
  case_control_option = "Case samples"
  showSampleNames = input$mbox_site_plot_show_samples
  barplot_samplewise(ds, ds$GO, mds, groupings, "GOTerm", case_control_option, 
                     showSampleNames = showSampleNames)
})

output$modal_goenrichment_samplewise_barplot <- renderPlot({
  modal_goenrichment_samplewise_barplot()
})



# modal_current_samplewise_barplot <- reactive({
#   req(modal_box_selection())
#   selection <- modal_box_selection()
#   if(selection$isSite == TRUE){
#     return(modal_site_samplewise_barplot)
#   }
#   if(selection$isProtein == TRUE){
#     return(modal_protein_samplewise_barplot)
#   }
#   if(selection$isKinase == TRUE){
#     return(modal_kinase_samplewise_barplot)
#   }
#   return(NULL)
# })

modal_current_samplewise_barplot <- reactive({
  req(modal_box_selection())
  selection <- modal_box_selection()
  if(selection$isSite == TRUE){
    return(modal_site_samplewise_barplot)
  }
  if(selection$isProtein == TRUE){
    return(modal_protein_samplewise_barplot)
  }
  if(selection$isKinase == TRUE){
    return(modal_kinase_samplewise_barplot)
  }
  if(selection$isGOTerm == TRUE){
    return(modal_goenrichment_samplewise_barplot)
  }
  return(NULL)
})

modal_barplot_filename <- reactive({
  req(modal_box_selection())
  selection <- modal_box_selection()
  paste(selection$main_identifier, "barplot", sep = "-");
})

modalBoxDownloadPlotDLHandler <- function(file_type){
  downloadPlotDLHandler(
    modal_current_samplewise_barplot(), 
    file_name = modal_barplot_filename, 
    file_type = file_type, 
    w_mult = 1.5,
    h_mult = 1
  )
}

output$modalbox_barplot_downloadPlotPNG <- modalBoxDownloadPlotDLHandler("png")
output$modalbox_barplot_downloadPlotPDF <- modalBoxDownloadPlotDLHandler("pdf")


