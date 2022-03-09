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

source("current_version.R")

source("src/ui_util.R")
source("src/ui_styling.R", local = TRUE)
source("src/ui_plots_main.R", local = TRUE)


shinyUI(fluidPage(
    useToastr(),
    useShinyjs(),
    
    # Application title
    title = "RokaiXplorer",
    
    tags$head(
        tags$link(rel="shortcut icon", href="favicon.png"),
        tags$meta(name="description", content="RokaiXplorer"),
        #includeHTML(("www/google-analytics.html")),
        tags$script(on_ready)
    ),

    verticalLayout(
        div(
            style = "margin-bottom:0px; padding-bottom:0px;",
            div(
                style = "position: relative; width: 100%",
                img(src='rokaiXplorer_logo.png', align = "left", style = "height: 53px; margin-bottom:10px; margin-top: 10px;"),
                
                tags$p(version_text(), style = paste(version_style(), version_style_additional(), "position: absolute; top: 35px; left:287px; width: 70%;", sep = ""))
            )
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
                             tags$div(style = "min-height:450px;",
                                shinycssloaders::withSpinner(DT::dataTableOutput("siteTable")),
                             ),
                            "Double click on a row to inspect it in detail."
                        )
                  ),
                  tabPanel(
                    "Interactive Network", 
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
                                tags$div(style = "min-height:450px;",
                                  shinycssloaders::withSpinner(DT::dataTableOutput("proteinTable")),
                                ),
                                "Double click on a row to inspect it in detail."
                       )
                    ),
                    tabPanel(
                      "Interactive Network", 
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
