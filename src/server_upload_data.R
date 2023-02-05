observeEvent(input$file1, {
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  # session$sendCustomMessage('upload_attempted_message', paste(upload_name(), network_value(), sep="-"))
  # main_logging(paste("Upload Attempted", sep = ""))
  upload_dataset()
  req(upload_dataset())
  network_value(refProteomeValue())
  # if(input$mainTabset == "About"){
  #     updateTabsetPanel(session, "mainTabset", "Plot")
  # }
  # session$sendCustomMessage('upload_sucess_message', paste(upload_name(), network_value(), sep="-"))
  # main_logging(paste("Upload Data - ", upload_name(), "-", network_value(), sep = ""))
})


observeEvent(input$file1_expression, {
  inFile <- input$file1_expression
  if (is.null(inFile))
    return(NULL)
  upload_expression_dataset()
  req(upload_expression_dataset())
  network_value(refProteomeValue())
})

observeEvent(input$file2, {
  inFile <- input$file2
  if (is.null(inFile))
    return(NULL)
  upload_metadata()
  req(upload_metadata())
})

upload_dataset <- reactive({
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  fileInfo <- input$file1
  ext = file_ext(inFile$datapath)
  switch(ext, 
         "csv" = x <- read.csv(inFile$datapath),
         validate(
           need(FALSE, "Invalid file type.")
         )
  )
  if(isolate(input$mainTabset) == "About"){
    updateTabsetPanel(session, "mainTabset", "Phosphosite")
  }
  
  myvalue("upload")
  upload_data_ready(TRUE)
  upload_name(fileInfo$name)
  message(cat("Dataset is uploaded: ", fileInfo$name))
  
  validate(
    need(x$Protein, "File format error: Protein column is missing."),
    need(x$Position, "File format error: Position column is missing."),
    # need(x$Quantification, "File format error: Quantification column is missing.")
  )
  main_logging(paste("Upload Data - ", upload_name(), "-", network_value(), sep = ""))
  
  # validate(
  #     need(class(x$Quantification) == "numeric", "File format error: Quantification column must be numeric.")
  # )
  return(x)
})

upload_expression_dataset <- reactive({
  inFile <- input$file1_expression
  if (is.null(inFile))
    return(NULL)
  fileInfo <- input$file1_expression
  ext = file_ext(inFile$datapath)
  switch(ext, 
         "csv" = x <- read.csv(inFile$datapath),
         validate(
           need(FALSE, "Invalid file type.")
         )
  )
  
  if(isolate(input$mainTabset) == "About"){
    updateTabsetPanel(session, "mainTabset", "Expression")
  }
  
  myvalue("upload")
  upload_expression_data_ready(TRUE)
  message(cat("Expression Dataset is uploaded: ", fileInfo$name))
  
  validate(
    need(x$Protein, "File format error: Protein column is missing."),
    need(is.na(match("Position", colnames(x))), "File format error: Expression data should not contain a position column."),
  )
  main_logging(paste("Upload Expression Data - ", fileInfo$name, "-", network_value(), sep = ""))
  return(x)
})

upload_metadata <- reactive({
  inFile <- input$file2
  if (is.null(inFile))
    return(NULL)
  fileInfo <- input$file2
  ext = file_ext(inFile$datapath)
  switch(ext, 
         "csv" = x <- read.csv(inFile$datapath),
         validate(
           need(FALSE, "Invalid file type.")
         )
  )
  if(isolate(input$mainTabset) == "About"){
    updateTabsetPanel(session, "mainTabset", "Phosphosite")
  }
  
  myvalue("upload")
  metadata_ready(FALSE)
  upload_metadata_ready(TRUE)
  upload_name_metadata(fileInfo$name)
  message(cat("Metadata is uploaded: ", fileInfo$name))
  
  validate(
    need(x$RowName, "File format error: RowName column is missing."),
    #   need(x$Position, "File format error: Position column is missing."),
    # need(x$Quantification, "File format error: Quantification column is missing.")
  )
  
  main_logging(paste("Upload Metadata - ", upload_name_metadata(), "-", network_value(), sep = ""))

  # validate(
  #     need(class(x$Quantification) == "numeric", "File format error: Quantification column must be numeric.")
  # )
  return(x)
})