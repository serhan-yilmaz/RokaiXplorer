observeEvent(input$buttonSampleData, {
  shinyWidgets::updatePickerInput(session, "refproteome", selected = "Uniprot Mouse");
  if(input$mainTabset == "About"){
    updateTabsetPanel(session, "mainTabset", "Site")
  }
  network_value("uniprot.mouse")
  myvalue("sample")
  reset('file1')
  reset('file2')
  upload_data_ready(FALSE)
  upload_metadata_ready(FALSE)
  main_logging("Sample Data")

})