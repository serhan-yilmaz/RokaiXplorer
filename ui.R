library(shiny)
library(magrittr)
library(DT)
library(cicerone)
library(visNetwork)
library(shinydashboard)
library(shinyWidgets)
library(knitr)

# For javascript
library(shinyjs)

# For notifications
library(shinytoastr)

# For tooltips
library(shinyBS) 
library(shinyhelper)
library(tippy)

library(plotly)

source("current_version.R")

source("src/common_util.R")
source("src/ui_util.R")
source("src/ui_styling.R", local = TRUE)
source("src/ui_plots_main.R", local = TRUE)
source("src/ui_datainput.R", local = TRUE)

application_title = "RokaiXplorer"

deployment_options <- readDeploymentOptions()
DEPLOYMENT_MODE_ENABLED = deployment_options$deployment_mode

filter_by_collapsed = T
if(DEPLOYMENT_MODE_ENABLED){
  application_title = deployment_options$application_title
  if(deployment_options$allow_data_download){
    dataInputDiv <- deploymentDataDownloadDiv
  } else {
    dataInputDiv <- ""
  }
  filter_by_collapsed = F
  script_on_start_hide_contact = tags$script(on_start_hide_contact)
  subtitle <- deployment_options$subtitle
  if(!is.empty(subtitle)){
    subtitle_div <- tags$text(style = "font-size:18px; font-weight:normal; color: #888;", subtitle)
  } else {
    subtitle_div = ""
  }
  app_title_div <- tags$div(
    style = "margin-bottom:10px;", 
    tags$h1(style = "font-weight:bold; color: #555; margin-bottom:0px;", paste0(application_title, "")),
    subtitle_div
    )
  # app_title_div <- rokaiLogo
} else {
  RokaiXplorer_banner <- ""
  script_on_start_hide_contact = ""
  app_title_div <- rokaiLogo
}

shinyUI(fluidPage(
    useToastr(),
    useShinyjs(),
    shinyjs:::extendShinyjs(text = jscode_collapse, functions = c("collapse")),
    useShinydashboard(),
    # Application title
    title = application_title,
    
    tags$head(
        tags$link(rel="shortcut icon", href="favicon.png"),
        tags$meta(name="description", content=application_title),
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$script(on_ready),
        script_on_start_hide_contact,
        # tags$script(on_start_collapse)
    ),
    
    # tags$script("")
    
#     tags$script("$('*').on('plotly_click', function(evt) {
#     console.log(evt);
# });"),

    verticalLayout(
        # tags$h4("", style ="margin-bottom:0px;"), 
        # tags$h1(style = "font-weight:bold; color: #555;", paste0(application_title, "-App")),
        app_title_div, 
        fluidRow(
            id = "main_layout_div", 
            column(width = 4,
                   tags$form(class = "well", style = "margin-bottom:8px;", id = "main_control_div", 
                 dataInputDiv,
                 optionBox(id = "optionbox_filter_by_subgroup", title = "Filter the samples", collapsed = filter_by_collapsed,
                 helper(tags$div(
                     style = "margin-top: 8px; ", 
                     tags$b("Select a variable to filter: "), 
                     tags$div(
                         style = "min-height:30px; max-height:194px; overflow-y:auto; padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
                         uiOutput("subgroup_controls")
                     )
                 ), type = "markdown", id = "select_subgroup_tooltip_icon", content = "select_subgroup_helper"),
                 tippy_this("select_subgroup_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Select a subgroup to focus on a subset of samples. Groups are specified in metadata. <span>", allowHTML = TRUE), 
                 ),
                 optionBox(id = "optionbox_subgroup_differences", title = "Identify Subgroup Differences", collapsed = T, 
                 helper(tags$div(
                         style = "margin-top: 8px; ", 
                         tags$b("Investigate Subgroup Differences: "), 
                         tags$div(
                             style = "min-height:30px; padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
                             uiOutput("group_difference_controls")
                         )
                ), type = "markdown", id = "group_differences_tooltip_icon", content = "group_difference_helper"),
                tippy_this("group_differences_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Select two subgroups to perform a differential analysis. (A vs B) <span>", allowHTML = TRUE), 
                 ),
                optionBox(id = "config_optionbox", title = "Import/Export Config", status = "success", 
                          textInput("config_name", "Configuration name (optional):", value = ""),
                          tags$div(
                            style = "margin-bottom: 6px;", 
                            downloadButton('download_config', 'Download Config'),
                            #tags$div(style = "margin-left: auto; margin-right: auto; width:0px;"),
                            tags$div(style = "display: inline-block; float: right;",
                                     actionButton("generate_token_button", "Generate token")
                            )
                          ),
                          tags$div(style = "font-size:16px; float:right; margin-rcight: 4px;",
                                   tags$a(id = "config_link_element", ""),
                          ),
                          textInput("config_token", "Enter token:", value = ""),
                          tags$div(
                            tags$div(style = "display: inline-block; float:right;",
                                     tipify(actionButton("restore_token_button", "Restore config"),
                                            "Restores the configuration from the input token")
                            )
                          ),
                          tags$br(),
                          tags$div(
                            tipify(
                              fileInput("upload_config", "Upload Config:", accept = c(".json")),
                              "Restores the configuration from the uploaded config file"),
                            tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px }"),
                            # tags$style(".checkbox {margin-bottom: 0px;}"),
                          ),
                )
                   ),
                RokaiXplorer_banner, 
            ),
            # mainPanel(
            column(width = 8,
             tabsetPanel(id = "mainTabset",
              source(file = "src/ui_about_tab.R", local=TRUE)$value,
              tabPanel("Phosphosite", tabsetPanel(id = "siteTabset", 
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
                            "Double click on a row to inspect it in detail.",
                        )
                  ),
                  tabPanel(
                    "Interactive Network", 
                    network_ks_ui("site_kinase_network", defaultSingleKinases = T)
                  ),
                  tabPanel("Enrichment", 
                           tags$div(id = "site_enrichment_table_div", 
                                    tags$div(style = "min-height:450px;",
                                             shinycssloaders::withSpinner(DT::dataTableOutput("siteEnrichmentTable")),
                                    ),
                                    "Double click on a row to inspect it in detail.",
                           )
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
              tabPanel("Kinase", tabsetPanel(id = "proteinTabset", 
                    tabPanel(
                     "Volcano Plot",
                     volcanoplot_ui("kinaselevel_volcano", minlogfc = 0.2)
                    ),
                    tabPanel(
                      "Bar Plot",
                      barplot_ui("kinase_barplot", minzscore = 1.25, minzscore_max = 3, showminsubs = T, yaxis_option = "Activity")
                    ),
                    tabPanel(
                      "Heatmap",
                      heatmap_ui("kinase_heatmap", significant_only = F, showminsubs = T)
                    ),
                    tabPanel("Table", 
                             tags$div(id = "kinase_table_div", 
                                      tags$div(style = "min-height:450px;",
                                               shinycssloaders::withSpinner(DT::dataTableOutput("kinaseTable"))
                                      ),
                                      fluidRow(
                                        column(width = 6, "Double click on a row to inspect it in detail."),
                                        column(width = 6, tags$div(
                                          style = "display:flex; justify-content: flex-end;", 
                                          sliderInput(paste("kinase_table", "minsubs", sep = "_"), "Min. number of substrates", 1, 10, 1, step = 1, width = "220px"))),
                                      ),
                             )
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
