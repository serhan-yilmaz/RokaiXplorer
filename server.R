
source("install_dependencies.R", local = TRUE)
install_dependencies()

# timestart = Sys.time()
library(stringr)
library(tools)
library(Matrix)
library(shiny)
library(DT)
library(reshape) 
library(ggplot2)
library(cicerone)
library(shinyjs)
library(shinytoastr)
library(shinylogs)
library(visNetwork)
#library(webshot)
library(preprocessCore)
#library(patchwork)
library(withr)
library(htmlwidgets)
library(rjson)
library(clipr)
library(writexl)
library(pracma)
library(limma, include.only = 'squeezeVar')
library(statmod)
library(plotly)
library(cicerone)
# library(hrbrthemes)
library(viridis)
library(ggthemes)

# timeend = Sys.time() - timestart
# message(timeend)

# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("preprocessCore")

##
#library(BiocManager)
#options(repos = BiocManager::repositories())

##
#webshot::install_phantomjs()

options(shiny.sanitize.errors = FALSE)

library(ids)

#library(plotly)

source("current_version.R")

source("src/common_util.R")
source("src/ui_util.R")
source("src/compute_pvalues.R")
source("src/t2zstatistic.R")
source("src/rokai_kinase_weights.R")
source("src/rokai_inference.R")
source("src/rokai_core.R")
source("src/rokai_circuit.R")
source("src/rokai_weights.R")

options(shiny.sanitize.errors = FALSE)

deployment_options <- readDeploymentOptions()
DEPLOYMENT_MODE_ENABLED = deployment_options$deployment_mode

## Update Sample Data
folder = "data/"
Tsample <- read.csv(paste(folder, "rokaiXplorer_sample_data.csv", sep=""))
Tsample_snippet <- read.csv(paste(folder, "rokaiXplorer_sample_data_snippet.csv", sep=""))
Tsample_expression_snippet <- read.csv(paste(folder, "rokaiXplorer_sample_expression_data_snippet.csv", sep=""))
Tsample_metadata <- read.csv(paste(folder, "rokaiXplorer_sample_metadata.csv", sep=""))
Tsample_expression <- read.csv(paste(folder, "rokaiXplorer_sample_expression_data.csv", sep=""))

tryCatch({
  library(rjson)
  email_credentials = fromJSON(file = "email/credentials.json")
  options(EMAIL_AVAILABLE = TRUE)
}, 
  error = function(e){options(EMAIL_AVAILABLE = FALSE)},
  warning = function(e){}
)

EMAIL_AVAILABLE = getOption("EMAIL_AVAILABLE")
if(is.null(EMAIL_AVAILABLE)){
  EMAIL_AVAILABLE = FALSE
}

if(DEPLOYMENT_MODE_ENABLED){
  Tdeployment_data <- read.csv(deployment_options$data_file_path)
  Tdeployment_metadata <- read.csv(deployment_options$metadata_file_path)
  if(deployment_options$use_expression_data){
    Tdeployment_expression_data <- read.csv(deployment_options$expression_data_file_path)
  } else {
    Tdeployment_expression_data <- NULL
  }
  
  config_file <- list(
    datapath = deployment_options$config_file_path
  )
  switch(deployment_options$reference_proteome, 
         "Uniprot Human" = config_reference_proteome <- "uniprot.human",
         "Uniprot Mouse" = config_reference_proteome <- "uniprot.mouse",
         "Uniprot Rat" = config_reference_proteome <- "uniprot.rat",
         stop("Invalid reference proteome.")) 
}

by_instance_logs_enabled = FALSE

shinyServer(function(input, output, session) {

    observe_helpers(withMathJax = TRUE, help_dir = "helpfiles")
  
    if(!DEPLOYMENT_MODE_ENABLED && by_instance_logs_enabled){
      track_usage(storage_mode = store_json(path = "logs/by_instance/"))
    }
    
    cache = list()
    
    session_id <- reactiveVal(random_id(n = 1, byte = 8))
    network_value <- reactiveVal("uniprot.human")
    upload_name <- reactiveVal("")
    upload_name_metadata <- reactiveVal("")
    myvalue <- reactiveVal("")
    initialized <- reactiveVal(TRUE)
    upload_data_ready <- reactiveVal(FALSE)
    upload_expression_data_ready <- reactiveVal(FALSE)
    upload_metadata_ready <- reactiveVal(FALSE)
    metadata_ready <- reactiveVal(FALSE)
    analyze_group_differences <- reactiveVal(FALSE)
    
    if(DEPLOYMENT_MODE_ENABLED){
      myvalue("deploymentdata")
    }
    
    source(file = "src/server_00_misc_functions.R", local=TRUE)
    source(file = "src/server_00_matlab_helpers.R", local=TRUE)
    source(file = "src/server_00_optionset.R", local=TRUE)
    source(file = "src/server_00_logging_main.R", local=TRUE)
    source(file = "src/server_00_ui_links.R", local=TRUE)
    source(file = "src/server_00_leave_feedback.R", local=TRUE)
    source(file = "src/server_00_util.R", local=TRUE)
    source(file = "src/server_00_interactive_tutorial.R", local=TRUE)
    
    
    observeEvent(input$initialized, {
        if(!dir.exists("logs/")){
          dir.create("logs/")
        }
        main_logging("Session Initialized")
        if(DEPLOYMENT_MODE_ENABLED){
          network_value(refProteomeValue())
          foRestoreConfiguration("", fromtoken = F, 
                                 file = config_file, silent = TRUE,
                                 ignore_input_not_found_error = TRUE)
          current_metadata()
          delay(50, preprocessed_dataset())
        }
        foReadURI(session)
    })
    
    refProteomeValue <- reactive({
      if(DEPLOYMENT_MODE_ENABLED){
        return(config_reference_proteome)
      }
      switch(input$refproteome, 
             "Uniprot Human" = "uniprot.human",
             "Uniprot Mouse" = "uniprot.mouse",
             "Uniprot Rat" = "uniprot.rat")
    })
    
    # Comment the following to change mapping beyond data upload
    observeEvent(input$refproteome, {
        network_value(refProteomeValue())
    })
    
    source(file = "src/server_reactive_network.R", local=TRUE)
    source(file = "src/server_sample_data.R", local=TRUE)
    source(file = "src/server_upload_data.R", local=TRUE)

    source(file = "src/server_reactive_dataset.R", local=TRUE)
    source(file = "src/server_parse_current_dataset.R", local=TRUE)
    
    source(file = "src/server_select_subgroups.R", local=TRUE)
    source(file = "src/server_select_group_differences.R", local=TRUE)
    
    foFilterDataset <- function(ds, validSamples){
      ds$Ts <- ds$Ts[, validSamples]
      return(ds)
    }
    
    filtered_dataset <- reactive({
        req(subgroup_samples())
        req(current_dataset_mapped())
        validSamples <- subgroup_samples()
        ds <- current_dataset_mapped()
        ds = foFilterDataset(ds, validSamples)
        return(ds)
    })
    
    foFilterMetadata <- function(x, validSamples){
      x$nSample <- nnzero(validSamples)
      x$caseSamples <- x$caseSamples[validSamples]
      x$Tsample_metadata <- x$Tsample_metadata[, validSamples]
      if(analyze_group_differences()){
        gd <- selected_group_differences()
        x$samplesA <- gd$samplesA[validSamples]
        x$samplesB <- gd$samplesB[validSamples]
      }
      return(x)
    }
    
    filtered_metadata <- reactive({
        req(subgroup_samples())
        validSamples <- subgroup_samples()
        x <- current_metadata()
        x = foFilterMetadata(x, validSamples)
        return(x)
    })

    source(file = "src/server_process_data_main.R", local=TRUE)
    ## The following are for heatmap and modal box inspection
    source(file = "src/server_process_data_bysample.R", local=TRUE)
    source(file = "src/server_process_proteindata_bysample.R", local=TRUE)
    source(file = "src/server_process_kinasedata_bysample.R", local=TRUE)
    source(file = "src/server_process_goenrichment_bysample.R", local=TRUE)
    
    source(file = "src/server_process_site_tables.R", local=TRUE)
    source(file = "src/server_process_protein_tables.R", local=TRUE)
    source(file = "src/server_process_kinase_tables.R", local=TRUE)
    source(file = "src/server_process_go_enrichment_tables.R", local=TRUE)
    source(file = "src/server_process_kinase_targets_table.R", local=TRUE)
    source(file = "src/server_process_pathway_targets_table.R", local=TRUE)
    
    source(file = "src/server_barplot_main.R", local=TRUE)
    source(file = "src/server_heatmap_main.R", local=TRUE)
    source(file = "src/server_barplot_samplewise.R", local=TRUE)
    source(file = "src/server_kinase_network_functions.R", local=TRUE)
    
    source(file = "src/server_volcanoplots.R", local=TRUE)
    source(file = "src/server_diagnostics.R", local=TRUE)
    source(file = "src/server_table_outputs.R", local=TRUE)
    source(file = "src/server_kinase_networks.R", local=TRUE)
    
    source(file = "src/server_site_barplots.R", local=TRUE)
    source(file = "src/server_protein_barplots.R", local=TRUE)
    source(file = "src/server_kinase_barplots.R", local=TRUE)
    
    source(file = "src/server_site_heatmaps.R", local=TRUE)
    source(file = "src/server_protein_heatmaps.R", local=TRUE)
    source(file = "src/server_protexpression_heatmaps.R", local=TRUE)
    source(file = "src/server_kinase_heatmaps.R", local=TRUE)
    
    source(file = "src/server_modalbox_main.R", local=TRUE)
    source(file = "src/server_modalbox_barplots.R", local=TRUE)
    source(file = "src/server_modalbox_tables.R", local=TRUE)
    
    source(file = "src/server_table_outputs_enrichment.R", local=TRUE)
    source(file = "src/server_table_outputs_targets.R", local=TRUE)
    
    source(file = "src/server_report_generator.R", local=TRUE)
    
    output$buttonDownloadSampleData <- downloadCSVFileHandler(
      Tsample_snippet, 'sample_data_snippet.csv');
    output$buttonDownloadSampleExpressionData <- downloadCSVFileHandler(
      Tsample_expression_snippet, 'sample_expression_data_snippet.csv');
    output$buttonDownloadSampleMetaData <- downloadCSVFileHandler(
      Tsample_metadata, 'sample_metadata.csv');
    output$buttonDownloadDeploymentData <- downloadCSVFileHandler(
      reactive_dataset, 'data.csv');
    output$buttonDownloadDeploymentExpressionData <- downloadCSVFileHandler(
      reactive_expression_dataset, 'expression_data.csv');
    output$buttonDownloadDeploymentMetadata <- downloadCSVFileHandler(
      reactive_metadata, 'metadata.csv');
    
})
