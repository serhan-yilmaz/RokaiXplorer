upload_data_helper <- function(el, helper_file = "input_data_format", 
                               tooltip = "Phosphosite quantifications. Click to learn the format."){
  tooltip_id = paste0(helper_file, "_tooltip_icon")
  tags$span(
    style = "display:inline;", 
    helper(el, 
           id = tooltip_id,
           type = "markdown", 
           content = helper_file),
    tippy_this(tooltip_id, paste0("<span style='font-size:14px; margin: 0px;'>", tooltip, "<span>"), allowHTML = TRUE)
  )
}

upload_data_combined_helper <- function(el, helper_file = "input_data_format_combined"){
  tags$span(
    style = "display:inline;", 
    helper(el, 
           id = "upload_data_tooltip_icon",
           type = "markdown", 
           content = helper_file),
    tippy_this("upload_data_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Click to learn the file format.<span>", allowHTML = TRUE)
  )
}

deploymentDataDownloadDiv <- function(use_expression_data = FALSE){
  if(use_expression_data){
    expression_download <- downloadButton('buttonDownloadDeploymentExpressionData', 'Expression Data')
    helper_file = "input_data_format_combined_withexpression"
  } else {
    expression_download <- tags$span()
    helper_file = "input_data_format_combined"
  }
  
  upload_data_combined_helper(tags$div(
    style = "margin-bottom:8px", 
    tags$b("Download Data: ", style = "margin-right: 10px;"),
    tags$div(
      style = "margin-top: 2px; display:inline-block;", 
      #  style = "border-style: inset; padding: 2px;", 
      tags$b(style = "margin-left: 2px; margin-right: 2px;"), 
      # tags$div(style = "margin-top: 4px;", 
      # tags$b("Download Sample Data: ", style = "margin-right: 10px;"),
      tags$div(style = "display:inline-block;", 
               style = "margin-top: 3px;;",
               style = "border-style: inset; padding: 3px; border-width:1px;", 
               #   border-width: 3px;
               downloadButton('buttonDownloadDeploymentData', 'Phosphorylation Data'),
               expression_download, 
               downloadButton('buttonDownloadDeploymentMetadata', 'Metadata'),
      )
      #  )
    )
  ), helper_file = helper_file)
}

sampleDataDiv <- tags$div(
  class = "inline-block", id = "sample_data_div", 
  tags$b("Sample Data: ", style = "margin-right: 10px;"),
  tags$span(
    style = "margin-top: 2px;", 
    #  style = "border-style: inset; padding: 2px;", 
    tags$span(id = "load_sample_data_div", actionButton("buttonSampleData", "Load Sample Data")),
    tags$b(style = "margin-left: 2px; margin-right: 2px;"), 
    # tags$div(style = "margin-top: 4px;", 
    # tags$b("Download Sample Data: ", style = "margin-right: 10px;"),
    tags$div(style = "display:inline-block;", 
             style = "margin-top: 4px;;",
             style = "border-style: inset; padding: 3px; border-width:1px;", 
             #   border-width: 3px;
             downloadButton('buttonDownloadSampleData', 'Phosphorylation Data'),
             downloadButton('buttonDownloadSampleExpressionData', 'Expression Data'),
             downloadButton('buttonDownloadSampleMetaData', 'Metadata'),
    )
    #  )
  )
)

dataInputDiv <- tags$div(

  tags$div(style = "margin: 0px; margin-top: 8px;", id = "data_input_div", 
           optionBox(title = "Data Input", collapsed = F, id = "optionbox_data_input", 
                     sampleDataDiv, 
                     tags$hr(style = "margin-top:12px; margin-bottom:12px;"), 
                     tags$div(id = "upload_data_div", 
                     tags$div(id = "upload_data_div2", 
                     upload_data_helper(
                       tags$div(
                         id = "phospho_data_div", 
                         fileInput("file1", "Upload Phosphorylation Data:", accept = c(".csv")),
                         tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                         tags$style(".checkbox {margin-bottom: 0px;}"),
                       )
                     ),
                     upload_data_helper(tags$div(id = "expression_data_div", 
                       style = "margin-top: 0px; margin-bottom: -8px;",
                       fileInput("file1_expression", "(Optional) Upload Expression Data:", accept = c(".csv")),
                       tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                       tags$style(".checkbox {margin-bottom: 0px;}"),
                     ), helper_file = "input_expression_data_format", tooltip = "Protein expression data. Click to learn the format."),
                     # tippy_this("upload_data_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Phosphosite quantifications. Click to learn the format.<span>", allowHTML = TRUE), 
                     tags$span(helper(
                       tags$div(
                         id = "metadata_upload_div", 
                         style = "margin-top: 0px;", 
                         fileInput("file2", "Upload Metadata:", accept = c(".csv")),
                         tags$style(".shiny-input-container {margin-bottom: 0px} #file2_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                         tags$style(".checkbox {margin-bottom: 0px;}"),
                       ), type = "markdown", id = "upload_metadata_tooltip_icon",
                       content = "input_metadata_format"
                     )),
                     tippy_this("upload_metadata_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Metadata on samples. Click to learn the format.<span>", allowHTML = TRUE), 
                     tags$div(style = "margin-top: 0px;",
                              multiChoicePicker("refproteome", "Reference Proteome:", c("Uniprot Human", "Uniprot Mouse", "Uniprot Rat"), selected = "Uniprot Mouse"),
                     )
                     ))
           )
           #tags$hr(style = "margin: 8px 0px 8px 0px;")
  )
)