library(shiny)
library(magrittr)
library(DT)
library(cicerone)
library(visNetwork)

# For javascript
library(shinyjs)

# For notifications
library(shinytoastr)

# For tooltips
library(shinyBS) 
library(shinyhelper)
library(tippy)

#library(plotly)

version_text <- function(){"v0.3.1"}
version_style <- function(){"font-size: 14px; color:#93A3A3;"}
version_style_additional <- function(){
    "-webkit-user-select: none;
  -khtml-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  -o-user-select: none;
  user-select: none;"
}

source("ui_util.R")

barplot_ui <- function(identifier){
    #"site_barplot"
    item_txt = strsplit(identifier, "_",)[[1]][1]
    tags$div(
        shinycssloaders::withSpinner(plotOutput(paste(identifier, "plot", sep = "_"), height = "360px")), 
        fluidRow(
            column(width = 6, style = "padding: 8px;", 
                   fluidRow(id = paste(identifier, "sliders_div", sep = "_"), 
                    
                    column(width = 6, style = "padding: 8px;", 
                             foMaxItemsHelper(
                                  sliderInput(paste(identifier, "maxitems", sep = "_"), "Number of items shown", 5, 50, 20, step = 1, width = "220px")
                                , identifier),
                             ),
                    column(width= 6, style = "padding: 8px;", 
                              sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. absolute z-score", 0, 4, 2, step = 0.05, width = "220px"),
                             tags$div(
                               style = "margin-top: 8px;", 
                               tags$b(paste("Significant ", item_txt, "s only", sep = "")), 
                               shinyWidgets::materialSwitch(inputId = paste(identifier, "significant_only", sep = "_"), label = "", status = "warning", value = F, inline = T)
                             )
                           )
            )),
            column(width = 3, style = "padding: 8px; padding-left: 16px;", 
                   multiChoicePicker(paste(identifier, "yaxis", sep = "_"), "Plot Y-Axis:", c("Log2-FC", "Z-Score"), isInline = "F"),
                   tags$div(
                       style = "margin-top: 4px;", 
                       #uiOutput("site_heatmap_select_group_ui"), 
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

heatmap_ui <- function(identifier){
  #"site_barplot"
  item_txt = strsplit(identifier, "_",)[[1]][1]
  tags$div(
    shinycssloaders::withSpinner(plotOutput(paste(identifier, "", sep = ""), height = "360px")), 
    fluidRow(
      column(width = 6, style = "padding: 8px;", 
             fluidRow(id = paste(identifier, "sliders_div", sep = "_"), 
                      
                      column(width = 6, style = "padding: 8px;", 
                             foMaxItemsHelper(
                               sliderInput(paste(identifier, "maxitems", sep = "_"), "Number of items shown", 10, 100, 40, step = 1, width = "220px")
                               , identifier),
                      ),
                      column(width= 6, style = "padding: 8px;", 
                             sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. Samplewise Magnitude", 0, 2, 0.5, step = 0.02, width = "220px"),
                             tags$div(
                               style = "margin-top: 8px;", 
                               tags$b(paste("Significant ", item_txt, "s only", sep = "")), 
                               shinyWidgets::materialSwitch(inputId = paste(identifier, "significant_only", sep = "_"), label = "", status = "warning", value = T, inline = T)
                             )
                      )
             )),
      column(width = 3, style = "padding: 8px; padding-left: 16px; padding-top: 12px;",
             multiChoicePicker(paste(identifier, "intensity_fc_style", sep = "_"), "Show:", c("Case samples", "Both case and control"), isInline = "F"),
             tags$div(
               style = "margin-top: 6px;",
               uiOutput(paste(identifier, "select_group_ui", sep = "_")), 
             #  multiChoicePicker(paste(identifier, "coloring", sep = "_"), "Coloring:", c("Z-Score", "Significance"), isInline = "F", multiple = T)
             )
      ),
      column(width = 3, style = "padding: 8px; padding-top: 12px;", tags$div(downloadButton(paste(identifier, "downloadPlotPNG", sep = "_"), 'Download PNG'),
                                                          tags$br(), 
                                                          downloadButton(paste(identifier, "downloadPlotPDF", sep = "_"), 'Download PDF')
      )
      )
    )
  )
}


# network_ks_ui <- function(identifier, yaxis_mainoption, defaultSingleKinases = F){
#   tags$div(
#     shinycssloaders::withSpinner(visNetworkOutput(identifier, height = "340px")), 
#     fluidRow(
#       column(width = 6, style = "padding: 8px;", fluidRow(id = paste(identifier, "sliders_div", sep = "_"), 
#                                                           column(width = 6, style = "padding: 8px;", sliderInput(paste(identifier, "maxitems", sep = "_"), "Number of items shown", 5, 50, 20, step = 1, width = "220px")), 
#                                                           column(width= 6, style = "padding: 8px;", sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. absolute z-score", 0, 4, 2, step = 0.05, width = "220px"))
#       )),
#        column(width = 3, style = "padding: 8px; padding-left: 16px;", 
#               tags$div(
#                 style = "margin-top: 8px;", 
#                 tags$b("Single Kinases"), 
#                 shinyWidgets::materialSwitch(inputId = paste(identifier, "single_kinases", sep = "_"), label = "", status = "danger", value = defaultSingleKinases, inline = T)
#                 )
#        )
#     )
#   )
# }


network_ks_ui <- function(identifier, defaultSingleKinases = F){
  item_txt = strsplit(identifier, "_",)[[1]][1]
  tags$div(
    fluidRow(
      column(width = 9, shinycssloaders::withSpinner(visNetworkOutput(identifier, height = "480px"))), 
    
      column(width = 3, style = "padding: 8px; padding-right:24px;", 
             fluidRow(id = paste(identifier, "sliders_div", sep = "_"), 
                      foMaxItemsHelper(sliderInput(paste(identifier, "maxitems", sep = "_"), "Number of items shown", 5, 50, 20, step = 1, width = "220px")
                                       , identifier),
            sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. absolute z-score", 0, 4, 2, step = 0.05, width = "220px"),
            tags$div(
              style = "margin-top: 8px;", 
              tags$b(paste("Significant ", item_txt, "s only", sep = "")), 
              shinyWidgets::materialSwitch(inputId = paste(identifier, "significant_only", sep = "_"), label = "", status = "warning", value = F, inline = T)
            ),
             tags$div(
               style = "margin-top: 13px;", 
               tags$b("Single Kinases"), 
               shinyWidgets::materialSwitch(inputId = paste(identifier, "single_kinases", sep = "_"), label = "", status = "danger", value = defaultSingleKinases, inline = T)
             )
      ))
    )
  )
}

volcanoplot_ui <- function(identifier){
  tags$div(
    fluidRow(
      column(width = 9, shinycssloaders::withSpinner(plotOutput(identifier, height = "445px"))), 
      column(width = 3, style = "padding: 8px;", 
             fluidRow(id = paste(identifier, "sliders_div", sep = "_"), 
                      sliderInput(paste(identifier, "maxfdr", sep = "_"), "Max. FDR", 0.01, 0.25, 0.1, step = 0.01, width = "220px"),
                      #sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. absolute z-score", 0, 4, 2, step = 0.05, width = "220px"),
                      sliderInput(paste(identifier, "minlogfc", sep = "_"), "Min. absolute log2-FC", 0, 1, 0.32, step = 0.01, width = "220px"), 
                      tags$div(style = "margin-top:8px;",
                      textOutput(paste(identifier, "summary", sep = "_"))
                      )
                      # tags$div(
                      #   style = "margin-top: 8px;",
                      #   tags$b("Single Kinases"),
                      #   shinyWidgets::materialSwitch(inputId = paste(identifier, "single_kinases", sep = "_"), label = "", status = "danger", value = F, inline = T)
                      # )
             ))
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
                 ), type = "markdown", id = "select_subgroup_tooltip_icon", content = "select_subgroup_helper"),
                 tippy_this("select_subgroup_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Select a subgroup to focus on a subset of samples. Groups are specified in metadata. <span>", allowHTML = TRUE), 
                 helper(tags$div(
                         style = "margin-top: 8px; ", 
                         tags$b("Investigate Group Differences: "), 
                         tags$div(
                             style = "min-height:30px; padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
                             uiOutput("group_difference_controls")
                         )
                ), type = "markdown", id = "group_differences_tooltip_icon", content = "group_difference_helper"),
                tippy_this("group_differences_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Select two subgroups to perform a differential analysis. (A vs B) <span>", allowHTML = TRUE), 
                
                   )
            ),
            # mainPanel(
            column(width = 8,
             tabsetPanel(id = "mainTabset",
              tabPanel("Site", tabsetPanel(id = "siteTabset", 
                  tabPanel(
                      "Volcano Plot",
                      volcanoplot_ui("sitelevel_volcano")
                  ),
                  tabPanel(
                    "Bar Plot",
                    barplot_ui("site_barplot")
                  ), 
                  tabPanel(
                    "Heatmap",
                    heatmap_ui("site_heatmap")
                  ), 
                  tabPanel("Table", 
                        tags$div(id = "site_table_div", 
                            shinycssloaders::withSpinner(DT::dataTableOutput("siteTable"))
                        )
                  ),
                  tabPanel(
                    "Network", 
                    network_ks_ui("site_kinase_network", defaultSingleKinases = T)
                  )
                  
              )),
              tabPanel("Protein", tabsetPanel(id = "proteinTabset", 
                    tabPanel(
                      "Volcano Plot",
                      volcanoplot_ui("proteinlevel_volcano")
                    ),
                    tabPanel(
                      "Bar Plot",
                      barplot_ui("protein_barplot")
                    ),
                    tabPanel(
                      "Heatmap",
                      heatmap_ui("protein_heatmap")
                    ),
                    tabPanel("Table", 
                       tags$div(id = "protein_table_div", 
                                shinycssloaders::withSpinner(DT::dataTableOutput("proteinTable"))
                       )
                    ),
                    tabPanel(
                      "Network", 
                      network_ks_ui("protein_kinase_network")
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