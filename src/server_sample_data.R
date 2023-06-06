observeEvent(input$buttonSampleData, {
  foLoadSampleData()
  main_logging("Sample Data")
  a <- guide$get_next()
  
  if(!is.null(a) && guide$get_next()$highlighted == "optionbox_filter_by_subgroup_wrapper"){
    #message("abcd")
    delay(100, guide$move_forward())
  }
  
})

# stop("abcdsd")

foLoadSampleData <- function(){
  shinyWidgets::updatePickerInput(session, "refproteome", selected = "Uniprot Mouse");
  if(input$mainTabset == "About"){
    updateTabsetPanel(session, "mainTabset", "Phosphosite")
  }
  network_value("uniprot.mouse")
  myvalue("sample")
  reset('file1')
  reset('file2')
  upload_data_ready(FALSE)
  upload_expression_data_ready(FALSE)
  upload_metadata_ready(FALSE)
  a = current_dataset()
  b = current_metadata()
}