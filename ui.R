library(shiny)
library(magrittr)
library(DT)
library(cicerone)

# For javascript
library(shinyjs)

# For notifications
library(shinytoastr)

# For tooltips
library(shinyBS) 
library(shinyhelper)
library(tippy)

version_text <- function(){"v0.1.2"}
version_style <- function(){"font-size: 14px; color:#93A3A3;"}
version_style_additional <- function(){
    "-webkit-user-select: none;
  -khtml-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  -o-user-select: none;
  user-select: none;"
}

multiChoicePicker <- function(id, label, choices, selected = choices[1], isInline = "T") {
    switch(isInline, 
           "T" = R <- tags$div(
               class = "inline-block", id = paste(id, "_div", sep = ""), 
               style = "justify-content: space-between;", 
               tags$b(label),
               shinyWidgets::pickerInput(id, "", choices, selected = selected, width = "fit", inline = T)
           ),
           "F" = R <- tags$div(
               id = paste(id, "_div", sep = ""), 
               tags$b(label),
               #selectInput(id, label, choices, selected = selected, width = "auto")
               shinyWidgets::pickerInput(id, "", choices, selected = selected, width = "fit", inline = F)
           )
    )
    return (R)
}

barplot_ui <- function(identifier, yaxis_mainoption){
    #"site_barplot"
    tags$div(
        shinycssloaders::withSpinner(plotOutput(paste(identifier, "plot", sep = "_"), height = "340px")), 
        fluidRow(
            column(width = 6, style = "padding: 8px;", fluidRow(id = paste(identifier, "sliders_div", sep = "_"), 
                                                                column(width = 6, style = "padding: 8px;", sliderInput(paste(identifier, "maxitems", sep = "_"), "Number of items shown", 5, 50, 20, step = 1, width = "220px")), 
                                                                column(width= 6, style = "padding: 8px;", sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. absolute z-score", 0, 4, 2, step = 0.05, width = "220px"))
            )),
            column(width = 3, style = "padding: 8px; padding-left: 16px;", 
                   multiChoicePicker(paste(identifier, "yaxis", sep = "_"), "Plot Y-Axis:", c(yaxis_mainoption, "Z-Score"), isInline = "F"),
                   tags$div(
                       style = "margin-top: 4px;", 
                       multiChoicePicker(paste(identifier, "coloring", sep = "_"), "Coloring:", c("Z-Score", "Significance"), isInline = "F")
                   )
                   ),
            column(width = 3, style = "padding: 8px;", tags$div(downloadButton(paste(identifier, "downloadPlotPNG", sep = "_"), 'Download PNG'),
                                                                tags$br(), 
                                                                downloadButton(paste(identifier, "downloadPlotPDF", sep = "_"), 'Download PDF')
            )
            )
        )
    )
}

# input_data_modal_content <- function (){
#     c("The site quantification data should be a csv file having the following columns:", 
#       paste("- <b>Protein (first column):</b>", "The Uniprot protein identifier. "),
#       paste("- <b>Position (second column):</b>", "The position of the site on the protein."),
#       paste("- <b>Quantifications (multiple columns):</b>", "The phosphorylation intensity of the site for the corresponding sample. The intensities are not expected to be in log scale."),
#       "<br> <p>Please see the provided sample data file to see an example.</p> Note that, in addition, you will also need to upload a metadata file to specify which samples are in case or control groups.")
# }

shinyUI(fluidPage(
    useToastr(),
    useShinyjs(),
    # Application title
    title = "RokaiXplorer",
    #titlePanel("RokaiXplorer"),
    

    # Sidebar with a slider input for number of bins
    verticalLayout(
        div(
            style = "margin-bottom:0px; padding-bottom:0px;",
            div(
                style = "position: relative; width: 100%",
                img(src='rokaiXplorer_logo.png', align = "left", style = "height: 53px; margin-bottom:10px; margin-top: 10px;"),
                
                tags$p(version_text(), style = paste(version_style(), version_style_additional(), "position: absolute; top: 35px; left:287px; width: 70%;", sep = ""))
                #tags$p("v2.0.0", style = "color:#A3A3A3;")
            )
            # style = "margin-bottom:10px; margin-top: 20px; padding-bottom:0px;",
            # tags$h2("RokaiXplorer", style = "display: inline;"),
            # tags$text(version_text(), style = paste(version_style(), "margin-left: 4px;", sep = "")), 
           # tags$text(version_text(), style = paste(version_style(), version_style_additional(), "margin-left: 4px;", sep = "")), 
        ), 
        fluidRow(
            id = "main_layout_div", 
            column(width = 4,
                   tags$form(class = "well", style = "margin-bottom:8px;", id = "main_control_div", 
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
                 tags$div(style = "margin: 0px; margin-top: 4px;", id = "upload_data_div", 
                          helper(
                              tags$div(
                                  fileInput("file1", "Upload Data:", accept = c(".csv")),
                                  tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                                  tags$style(".checkbox {margin-bottom: 0px;}"),
                              ), type = "markdown", id = "upload_data_tooltip_icon",
                              content = "input_data_format"
                          ),
                          tippy_this("upload_data_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Site quantifications. Click to learn the format.<span>", allowHTML = TRUE), 
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
                          #tags$hr(style = "margin: 8px 0px 8px 0px;")
                 ),
                 helper(tags$div(
                     style = "margin-top: 8px; ", 
                     tags$b("Select Subgroup: "), 
                     tags$div(
                         style = "min-height:30px; max-height:194px; overflow-y:auto; padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
                         uiOutput("subgroup_controls")
                     )
                 ), type = "inline", id = "select_subgroup_tooltip_icon"),
                 tippy_this("select_subgroup_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Select a subgroup to focus on a subset of samples. Groups are specified in metadata. <span>", allowHTML = TRUE), 
                 helper(tags$div(
                         style = "margin-top: 8px; ", 
                         tags$b("Investigate Group Differences: "), 
                         tags$div(
                             style = "min-height:30px; padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
                             uiOutput("group_difference_controls")
                         )
                ), type = "inline", id = "group_differences_tooltip_icon"),
                tippy_this("group_differences_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Select two subgroups to perform a differential analysis. (A vs B) <span>", allowHTML = TRUE), 
                
                   )
            ),
            # mainPanel(
            column(width = 8,
             tabsetPanel(id = "mainTabset",
              tabPanel("Site", tabsetPanel(id = "siteTabset", 
                  tabPanel("Table", 
                        tags$div(id = "site_table_div", 
                            shinycssloaders::withSpinner(DT::dataTableOutput("siteTable"))
                        )
                  ),
                  tabPanel(
                      "Volcano Plot",
                      shinycssloaders::withSpinner(plotOutput("sitelevel_volcano"))
                  ),
                  tabPanel(
                      "Bar Plot",
                      barplot_ui("site_barplot", "Site Phosphorylation")
                  )
                  
              )),
              tabPanel("Protein", tabsetPanel(id = "proteinTabset", 
                    tabPanel("Table", 
                       tags$div(id = "protein_table_div", 
                                shinycssloaders::withSpinner(DT::dataTableOutput("proteinTable"))
                       )
                    ),
                    tabPanel(
                        "Volcano Plot",
                        shinycssloaders::withSpinner(plotOutput("proteinlevel_volcano"))
                    ),
                    tabPanel(
                        "Bar Plot",
                        barplot_ui("protein_barplot", "Protein Phosphorylation")
                    )
              )),
              tabPanel("Diagnostics", tabsetPanel(id = "diagnosticsTabset", 
                  tabPanel("Histogram",
                      shinycssloaders::withSpinner(plotOutput("histogram_sitecentering"))
                  ))
             ))
                   
            )
        )
    )
))
