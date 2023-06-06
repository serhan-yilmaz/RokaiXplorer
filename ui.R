library(shiny)
library(magrittr)
library(DT)
library(cicerone)
library(visNetwork)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(knitr)
library(cicerone)

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
source("src/ui_styling.R")
source("src/ui_plots_main.R")
source("src/ui_datainput.R")

application_title = "RokaiXplorer"

deployment_options <- readDeploymentOptions()
DEPLOYMENT_MODE_ENABLED = deployment_options$deployment_mode

filter_by_collapsed = T
if(DEPLOYMENT_MODE_ENABLED){
  application_title = deployment_options$application_title
  if(deployment_options$allow_data_download){
    dataInputDiv <- deploymentDataDownloadDiv(deployment_options$use_expression_data)
  } else {
    dataInputDiv <- ""
  }
  filter_by_collapsed = F
  script_on_start_hide_contact = tags$script(on_start_hide_contact)
  subtitle <- deployment_options$application_subtitle
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
  # subtitle = "Interactive analysis of proteomics and phospho-proteomics data"
  subtitle = "Discover insights from proteomics experiments with ease"
  subtitle_div <- tags$text(style = "font-size:18px; font-weight:normal; color: #777;", subtitle)
  
  app_title_div <- tags$div(
    style = "margin-bottom:10px; display: flex; flex-direction: column;", 
    tags$div(rokaiLogo),
    tags$div(subtitle_div),
  )
}

go_cats <- c("Biological Process", "Cellular Component", "Molecular Function")
enrichment_cats <- list("GO Terms" = go_cats)
enrichment_sel_cats <- c(go_cats)
enrichment_datasource = c("Phosphosites", "Phosphoproteins", "Protein Expression")
# enrichment_datasource = c("Phosphosites")

# enrichment_analysis_optionbox <- tags$div(
#   style = "margin-top:8px;", 
#   optionBox(id = "optionbox_enrichment_analysis_options", title = "Analysis Options", collapsed = F, status = "primary", collapsible = F,
#    multiChoicePicker("enrichment_statisticaltest", "Statistical Test:", c("Chi-squared Test"), selected = "Phosphosites", isInline = "F", multiple = F, max_opts = 99, width = "auto", style = "display:flex;flex-direction: column; margin-bottom:6px;", picker_inline = F, class_names = "abc", tooltip = "The statistical test to apply to determine the p-values."),
#    fancyCheckbox("enrichment_apply_yates_correction", "Apply Yates's correction", default = T, tooltip = "If enabled, Yates`s correction for continuity will be applied on the chi-squared test."),
#    multiChoicePicker("enrichment_enrichmentscore", "Enrichment Score:", c("Log2 Risk Ratio (Bayes Estimate)"), selected = "Log2 Risk Ratio (Bayes Estimate)", isInline = "F", multiple = F, max_opts = 99, width = "auto", style = "display:flex;flex-direction: column; margin-bottom:6px;", picker_inline = F, class_names = "abc", tooltip = "Statistic to quantify the magnitude of the enrichment."),
#   )
# )

enrichment_analysis_additional <- tags$div(
  multiChoicePicker("enrichment_statisticaltest", "Statistical Test:", c("Chi-squared Test"), isInline = "F", multiple = F, max_opts = 99, width = "auto", style = "display:flex;flex-direction: column; margin-bottom:6px;", picker_inline = F, class_names = "abc", tooltip = "The statistical test to apply to determine the p-values."),
  fancyCheckbox("enrichment_apply_yates_correction", "Apply Yates's correction", default = T, tooltip = "If enabled, Yates`s correction for continuity will be applied on the chi-squared test."),
  multiChoicePicker("enrichment_enrichmentscore", "Enrichment Score:", c("Log2 Risk Ratio (Bayes Estimate)"), selected = "Log2 Risk Ratio (Bayes Estimate)", isInline = "F", multiple = F, max_opts = 99, width = "auto", style = "display:flex;flex-direction: column; margin-bottom:6px;", picker_inline = F, class_names = "abc", tooltip = "Statistic to quantify the magnitude of the enrichment."),
)

kinase_analysis_additional = tags$div(
  multiChoicePicker("ksNetwork", "Kinase Substrate Dataset:", c("PhosphoSitePlus", "PSP+Signor"), "PSP+Signor", style = "margin-bottom: 2px;"),
  multiChoicePicker("rokaiNetwork", "RoKAI Network:", c("KinaseSubstrate", "KS+PPI", "KS+PPI+SD", "KS+PPI+SD+CoEv"), "KS+PPI+SD+CoEv", style = "margin-top:4px; margin-bottom: 2px;"),
  # multiChoicePicker("kinase_statisticaltest", "Statistical Test:", c("Weighted T-Test"), selected = "Z-Test", width = "auto", style = "margin-top:4px; margin-bottom: 2px;", picker_inline = T, class_names = "abc", tooltip = "The statistical test to apply to determine the p-values."),
  # multiChoicePicker("kinase_activityscore", "Activity Score:", c("Scaled", "Original"), selected = "Scaled", width = "auto", style = "margin-top:4px; margin-bottom: 2px;", picker_inline = T, class_names = "abc", tooltip = "Statistic to quantify the magnitude of kinase or phosphatase activity."),
  checkboxInput("rokaiEnabled", "Use sites in functional neighborhood", TRUE),
  asliderInput(paste("kinaselevel_minsubs", sep = "_"), "Min. number of substrates", 1, 5, 3, step = 1, width = "240px", tooltip = "Kinases filtered by this cutoff will be removed from the analysis and will not appear in plots or tables.")
)

shinyUI(fluidPage(
    useToastr(),
    useShinyjs(),
    shinyjs:::extendShinyjs(text = jscode_collapse, functions = c("collapse")),
    useShinydashboard(),
    use_cicerone(),
    # Application title
    title = application_title,
    
    tags$head(
        tags$link(rel="shortcut icon", href="favicon.png"),
        tags$meta(name="description", content=application_title),
        # tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$script(on_ready),
        script_on_start_hide_contact,
        # tags$script(on_start_collapse)
    ),
    
    tags$head(
      includeCSS("www/style.css")
    ),

    verticalLayout(
        # tags$h4("", style ="margin-bottom:0px;"), 
        # tags$h1(style = "font-weight:bold; color: #555;", paste0(application_title, "-App")),
        app_title_div, 
        fluidRow(
            id = "main_layout_div", 
            column(width = 4, id = "main_control_div", 
                   tags$form(class = "well", style = "margin-bottom:8px;", id = "main_control_div2", 
                 dataInputDiv,
                 tags$div(id = "optionbox_filter_by_subgroup_wrapper2", 
                 optionBox(id = "optionbox_filter_by_subgroup", title = "Filter the samples", collapsed = filter_by_collapsed,
                 helper(tags$div(
                     id = "subgroup_controls_div", 
                     style = "margin-top: 8px; ", 
                     tags$b("Select a variable to filter: "), 
                     tags$div(
                         style = "min-height:30px; max-height:194px; overflow-y:auto; padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
                         uiOutput("subgroup_controls")
                     )
                 ), type = "markdown", id = "select_subgroup_tooltip_icon", content = "select_subgroup_helper"),
                 tippy_this("select_subgroup_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Select a subgroup to focus on a subset of samples. Groups are specified in metadata. <span>", allowHTML = TRUE), 
                 )),
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
                optionBox(id = "optionbox_options", title = "Analysis Options", collapsed = T,
                  fancyCheckbox("options_var_across_samples", "Estimate variance across samples", default = T, tooltip = "If enabled, variance will be measured across the samples and t-test will be performed to assess the significance of differential expression/phosphorylation. Otherwise, variance will be measured across the phosphosites/proteins based on the fold changes and z-test will be performed. Recommended", tooltip_width = "270px"), 
                  fancyCheckbox("options_moderated_ttest", "Apply moderated t-test", default = T, tooltip = "If enabled, a moderated t-test will be performed if applicable."), 
                  asliderInput("options_minsamples", "Min. number of samples", 2, 5, 2, step = 1, tooltip = "Select minimum required number of samples to perform a t-test. Phosphosites/proteins with less number of quantifications will be filtered (only used if t-test is enabled)"),
                  multiChoicePicker("options_var_stabilization", "Variable Stabilization:", c("Centering", "Do not apply"), selected = "Centering", isInline = "F", multiple = F, width = "auto", style = "margin-bottom:6px;", picker_inline = T, class_names = "abc", tooltip = "If centering is selected, quantifications for all samples will be centered by substrating the mean across all phosphosites/proteins. "),
                  fancyCheckbox("options_center_foldchanges", "Center the fold changes", default = T, tooltip = "If enabled, log fold changes will be centered by substrating the mean across all phosphosites/proteins."), 
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
            column(width = 8, id = "main_output_div",
                   tags$div(id = "main_output_div2", 
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
              )),
              tabPanel("Phosphoprotein", tabsetPanel(id = "proteinTabset", 
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
              tabPanel("Expression", tabsetPanel(id = "protExpressionTabset", 
                  tabPanel(
                    "Volcano Plot",
                    volcanoplot_ui("protexpression_volcano")
                  ),
                  tabPanel(
                    "Bar Plot",
                    barplot_ui("protexpression_barplot")
                  ),
                  tabPanel(
                    "Heatmap",
                    heatmap_ui("protexpression_heatmap")
                  ),
                  tabPanel("Table", 
                           tags$div(id = "protexpression_table_div", 
                                    tags$div(style = "min-height:450px;",
                                             shinycssloaders::withSpinner(DT::dataTableOutput("protExpressionTable")),
                                    ),
                                    "Double click on a row to inspect it in detail.",
                           )
                  ),
                  # tabPanel(
                  #   "Interactive Network", 
                  #   network_ks_ui("protexpression_kinase_network", defaultSingleKinases = T)
                  # ),
              )),
              tabPanel("Kinase", tabsetPanel(id = "kinaseTabset", 
                    tabPanel(
                     "Volcano Plot",
                     volcanoplot_ui("kinaselevel_volcano", include_fc_slider = F, additional = kinase_analysis_additional, plot_width = 7, show_legend = F, slider_width = "240px")
                    ),
                    tabPanel(
                      "Bar Plot",
                        barplot_ui("kinase_barplot", minzscore = 1, minzscore_max = 2, showminsubs = T, yaxis_option = "Activity", num_items_shown = 10, num_items_shown_max = 30)
                    ),
                    tabPanel(
                      "Heatmap",
                      heatmap_ui("kinase_heatmap", significant_only = F, showminsubs = T, minsamplewise_magnitude = 0, usezscore = T)
                    ),
                    tabPanel("Table", 
                             tags$div(id = "kinase_table_div", 
                                      tags$div(style = "min-height:450px;",
                                               dropdown_options_alt(
                                                 tags$div(
                                                   sliderInput(paste("kinase_table", "minsubs", sep = "_"), "Min. number of substrates", 1, 10, 1, step = 1, width = "220px")
                                                 ),
                                                 title = "Filtering Options", tooltip = "Click to display options.", width = "250px"
                                               ),
                                               shinycssloaders::withSpinner(DT::dataTableOutput("kinaseTable"))
                                      ),
                                      fluidRow(
                                        column(width = 6, "Double click on a row to inspect it in detail."),
                                        # column(width = 6, 
                                        # tags$div(
                                        #   style = "display:flex; justify-content: flex-end;", 
                                        #   sliderInput(paste("kinase_table", "minsubs", sep = "_"), "Min. number of substrates", 1, 10, 1, step = 1, width = "220px")
                                        # ),
                                        #   ),
                                      ),
                             )
                    ),
                    tabPanel("Targets",
                             tags$div(id = "kinase_targets_table_div", 
                                      tags$div(style = "min-height:450px;",
                                               # dropdown_options_alt(
                                               #   tags$div(
                                               #     sliderInput(paste("kinase_table", "minsubs", sep = "_"), "Min. number of substrates", 1, 10, 1, step = 1, width = "220px")
                                               #   ),
                                               #   title = "Filtering Options", tooltip = "Click to display options.", width = "250px"
                                               # ),
                                               shinycssloaders::withSpinner(DT::dataTableOutput("kinaseTargetsTable"))
                                      ),
                                      fluidRow(
                                        column(width = 6, "Double click on a row to inspect it in detail."),
                                      ),
                             )
                    )
              )),
              tabPanel("Enrichment", tabsetPanel(id = "enrichmentTabset", 
                tabPanel("Settings",
                         tags$div(id = "enrichment_settings_div", style = "padding-top: 8px;",
                         fluidRow(
                         column(6, 
                        optionBox(id = "optionbox_enrichment_inclusion_criteria", title = "Inclusion Criteria (Enrichment Terms)", collapsed = F, status = "primary", collapsible = F, 
                          multiChoicePicker("enrichment_categories", "Include the following:", enrichment_cats, selected = enrichment_sel_cats, isInline = "F", multiple = T, max_opts = 99, width = "auto", style = "display:flex;flex-direction: column; margin-bottom:6px;", picker_inline = F, class_names = "abc", tooltip = "Select which terms to include in the analysis"),
                          asliderInput(paste("enrichment", "mintargets", sep = "_"), "Min. number of observed proteins", 1, 10, 3, step = 1, tooltip = "Select minimum required number of identified proteins for a term to be included in the analysis"),
                          asliderInput(paste("enrichment", "minobservedratio", sep = "_"), "Min. observation ratio", 0, 50, 10, step = 1, post = "%", tooltip = "Select a minimum ratio of number of identified / number of total proteins in the set"),
                          fancyCheckbox("enrichment_filterbyoverlap", "Filter by overlap", default = T, tooltip = "If enabled, highly similar terms will be excluded from the analysis based on the overlap of identified proteins in their sets", style = "max-width:200px;"), 
                          asliderInput(paste("enrichment", "maxoverlap", sep = "_"), "Max. overlap percentage: ", 50, 100, 70, step = 5, post = "%", tooltip = "Select a threshold to filter out highly similar terms as measured by jaccard index"),
                          tags$p("Number of terms: ", uiOutput("enrichment_display_numpathways", inline = TRUE)), 
                         ),
                         ),
                         column(6, 
                                optionBox(id = "optionbox_enrichment_background_set_proteins", title = "Background Set (Proteins)", collapsed = F, status = "primary", collapsible = F, 
                                          multiChoicePicker("enrichment_datasource", "Datasource:", enrichment_datasource, selected = "Phosphosites", isInline = "F", multiple = F, max_opts = 99, width = "auto", style = "display:flex;flex-direction: column; margin-bottom:6px;", picker_inline = F, class_names = "abc", tooltip = "Select a datasource to determine the background set to perform the enrichment analysis on"),
                                          fancyCheckbox("enrichment_fdrcorrection", "Apply FDR correction", default = F, tooltip = "If enabled, the P-value cutoff is applied after accounting for FDR with BH procedure"),
                                          asliderInput(paste("enrichment", "maxpvalue", sep = "_"), "Max. P-value Cutoff", 0.01, 0.25, 0.1, step = 0.01, tooltip = "Cutoff on the P-values or FDR"),
                                          asliderInput(paste("enrichment", "minlogfc", sep = "_"), "Min. Absolute Log2FC Cutoff", 0, 2, 1, step = 0.05, tooltip = "Select a cutoff based on the absolute value of log2 fold changes."),
                                          multiChoicePicker("enrichment_logfcdirection", "Direction:", c("Positive and Negative", "Positive Only", "Negative Only"), selected = "Positive and Negative", isInline = "F", multiple = F, max_opts = 99, width = "auto", style = "display:flex;flex-direction: column; margin-bottom:6px;", picker_inline = F, class_names = "abc", tooltip = "Select the desired direction for the log fold changes"),
                                          tags$p("Number of proteins: ", uiOutput("enrichment_display_numproteins", inline = TRUE))
                                  ),
                                ),
                         )
                         ), 
                ),
                tabPanel(
                  "Volcano Plot",
                  volcanoplot_ui("pathwaylevel_volcano", include_fc_slider = F, plot_width = 8, additional = enrichment_analysis_additional)
                ),
                tabPanel("Table", 
                       tags$div(id = "enrichment_table_div", 
                                tags$div(style = "min-height:450px;",
                                         dropdown_options_alt(
                                           tags$div(
                                             multiChoicePicker("enrichment_table_categories", "Category:", enrichment_cats, selected = enrichment_sel_cats, isInline = "F", multiple = T, max_opts = 99, width = "220px", style = "display:flex;flex-direction: column;", style_label = "margin-bottom:-15px;"), 
                                             asliderInput(paste("enrichment", "minhits", sep = "_"), "Min. Number of Hits", 0, 5, 1, step = 1, style_div = "margin-top: 4px;"),
                                             fancyCheckbox("enrichment_table_significantonly", "Significant Only", default = F),
                                           ),
                                           title = "Filtering Options", tooltip = "Click to display options.", width = "250px"
                                         ),
                                         shinycssloaders::withSpinner(DT::dataTableOutput("siteEnrichmentTable")),
                                ),
                                "Double click on a row to inspect it in detail.",
                       )
                ),
                tabPanel("Targets",
                         tags$div(id = "pathway_targets_table_div", 
                                  tags$div(style = "min-height:450px;",
                                           dropdown_options_alt(
                                             tags$div(
                                               multiChoicePicker("enrichment_targets_table_categories", "Category:", enrichment_cats, selected = enrichment_sel_cats, isInline = "F", multiple = T, max_opts = 99, width = "220px", style = "display:flex;flex-direction: column;", style_label = "margin-bottom:-15px;"), 
                                               asliderInput(paste("enrichment_targets", "minhits", sep = "_"), "Min. Number of Hits", 0, 5, 1, step = 1, style_div = "margin-top: 4px;"),
                                               fancyCheckbox("enrichment_targets_table_significantonly", "Significant Only", default = T),
                                               fancyCheckbox("enrichment_targets_table_hitsonly", "Show Hits Only", default = F),
                                             ),
                                           title = "Filtering Options", tooltip = "Click to display options.", width = "250px"
                                           ),
                                           shinycssloaders::withSpinner(DT::dataTableOutput("pathwayTargetsTable"))
                                  ),
                                  fluidRow(
                                    column(width = 6, "Double click on a row to inspect it in detail."),
                                  ),
                         )
                )
              )),
               tabPanel("Report Generator", tags$div(id = "report_generator_div", style = "padding:4px;", 
                tags$span(id = "report_generator_options", generate_scenario_area(list(
                multiChoicePicker("report_data_source", "Data Source:", c("Phosphosites", "Phosphoproteins", "Protein Expression", "Kinases", "Enrichment"), isInline = "F", multiple = F, width = "auto", style = "display:flex;flex-direction: column; margin-bottom:6px;", picker_inline = F, class_names = "abc", tooltip = "Select the type of the analysis to be performed."),
                uiOutput("report_group_options")
                ), nRow = 3)), 
                tags$span(id = "report_generator_buttons", 
                actionButton('prepare_report', "Run"),
                shinyjs::disabled(downloadButton('test_report_generator', 'Report (Excel)')),
                ),
              ))
             #  tabPanel("Diagnostics", tabsetPanel(id = "diagnosticsTabset", 
             #      tabPanel("Histogram",
             #          shinycssloaders::withSpinner(plotOutput("histogram_sitecentering"))
             #      ))
             # )
             )
            ))
        ),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading ", tags$span(class = "loadingbar-custom"), id="loadmessage")),
    )
))
