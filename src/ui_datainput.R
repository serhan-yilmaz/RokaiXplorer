upload_data_helper <- function(el){
  tags$span(
    style = "display:inline;", 
    helper(el, 
           id = "upload_data_tooltip_icon",
           type = "markdown", 
           content = "input_data_format"),
    tippy_this("upload_data_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Phosphosite quantifications. Click to learn the format.<span>", allowHTML = TRUE)
  )
}

upload_data_combined_helper <- function(el){
  tags$span(
    style = "display:inline;", 
    helper(el, 
           id = "upload_data_tooltip_icon",
           type = "markdown", 
           content = "input_data_format_combined"),
    tippy_this("upload_data_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Click to learn the file format.<span>", allowHTML = TRUE)
  )
}

deploymentDataDownloadDiv <- upload_data_combined_helper(tags$div(
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
             downloadButton('buttonDownloadDeploymentData', 'Data'),
             downloadButton('buttonDownloadDeploymentMetadata', 'Metadata'),
    )
    #  )
  )
))

dataInputDiv <- tags$div(
  tags$div(
    class = "inline-block", id = "sample_data_div", 
    tags$b("Sample Data: ", style = "margin-right: 10px;"),
    tags$div(
      style = "margin-top: 2px;", 
      #  style = "border-style: inset; padding: 2px;", 
      actionButton("buttonSampleData", "Load Sample Data"),
      tags$b(style = "margin-left: 2px; margin-right: 2px;"), 
      # tags$div(style = "margin-top: 4px;", 
      # tags$b("Download Sample Data: ", style = "margin-right: 10px;"),
      tags$div(style = "display:inline-block;", 
               style = "margin-top: 3px;;",
               style = "border-style: inset; padding: 3px; border-width:1px;", 
               #   border-width: 3px;
               downloadButton('buttonDownloadSampleData', 'Data'),
               downloadButton('buttonDownloadSampleMetaData', 'Metadata'),
      )
      #  )
    )
  ), 
  tags$div(style = "margin: 0px; margin-top: 8px;", id = "upload_data_div", 
           optionBox(title = "Upload Data", collapsed = F, 
                     upload_data_helper(
                       tags$div(
                         fileInput("file1", "Upload Data:", accept = c(".csv")),
                         tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                         tags$style(".checkbox {margin-bottom: 0px;}"),
                       )
                     ),
                     # tippy_this("upload_data_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Phosphosite quantifications. Click to learn the format.<span>", allowHTML = TRUE), 
                     helper(
                       tags$div(
                         style = "margin-top: 0px;", 
                         fileInput("file2", "Upload Metadata:", accept = c(".csv")),
                         tags$style(".shiny-input-container {margin-bottom: 0px} #file2_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                         tags$style(".checkbox {margin-bottom: 0px;}"),
                       ), type = "markdown", id = "upload_metadata_tooltip_icon",
                       content = "input_metadata_format"
                     ),
                     tippy_this("upload_metadata_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Metadata on samples. Click to learn the format.<span>", allowHTML = TRUE), 
                     tags$div(style = "margin-top: 0px;",
                              multiChoicePicker("refproteome", "Reference Proteome:", c("Uniprot Human", "Uniprot Mouse"), selected = "Uniprot Mouse"),
                     )
           )
           #tags$hr(style = "margin: 8px 0px 8px 0px;")
  )
)