## Depends barplot_samplewise

## Site samplewise barplot
modal_site_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  mds <- modal_box_selection_mapped()
  validate(need(mds$isSite, ""))
  req(processed_data_bysample_unfiltered())
  
  ds <- processed_data_bysample_unfiltered()
  groupings = input$mbox_site_plot_select_group
  barplot_samplewise(ds, ds$ST, mds, groupings, "Site")
})

output$modal_site_samplewise_barplot <- renderPlot({
  modal_site_samplewise_barplot()
})

## Protein samplewise barplot
modal_protein_samplewise_barplot <- reactive({
  req(modal_box_selection_mapped())
  mds <- modal_box_selection_mapped()
  validate(need(mds$isProtein, ""))
  req(processed_protein_data_bysample_unfiltered())
  
  ds <- processed_protein_data_bysample_unfiltered()
  groupings = input$mbox_site_plot_select_group
  barplot_samplewise(ds, ds$PT, mds, groupings, "Protein")
})

output$modal_protein_samplewise_barplot <- renderPlot({
  modal_protein_samplewise_barplot()
})