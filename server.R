
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
library(plotly)

# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("preprocessCore")

##
#library(BiocManager)
#options(repos = BiocManager::repositories())

##
#webshot::install_phantomjs()

library(ids)

#library(plotly)

source("current_version.R")
source("src/compute_pvalues.R")
source("src/ui_util.R")

## Update Sample Data
folder = "data/"
Tsample <- read.csv(paste(folder, "rokaiXplorer_sample_data.csv", sep=""))
Tsample_snippet <- read.csv(paste(folder, "rokaiXplorer_sample_data_snippet.csv", sep=""))
Tsample_metadata <- read.csv(paste(folder, "rokaiXplorer_sample_metadata.csv", sep=""))

shinyServer(function(input, output, session) {

    observe_helpers(withMathJax = TRUE, help_dir = "helpfiles")
    track_usage(storage_mode = store_json(path = "logs/by_instance/"))
    
    session_id <- reactiveVal(random_id(n = 1, byte = 8))
    network_value <- reactiveVal("uniprot.human")
    upload_name <- reactiveVal("")
    upload_name_metadata <- reactiveVal("")
    myvalue <- reactiveVal("")
    initialized <- reactiveVal(TRUE)
    upload_data_ready <- reactiveVal(FALSE)
    upload_metadata_ready <- reactiveVal(FALSE)
    metadata_ready <- reactiveVal(FALSE)
    analyze_group_differences <- reactiveVal(FALSE)
    
    source(file = "src/server_logging_main.R", local=TRUE)
    source(file = "src/server_ui_links.R", local=TRUE)
    source(file = "src/server_leave_feedback.R", local=TRUE)
    
    observeEvent(input$initialized, {
        main_logging("Session Initialized")
    })
    
    refProteomeValue <- reactive({
        switch(input$refproteome, 
               "Uniprot Human" = "uniprot.human",
               "Uniprot Mouse" = "uniprot.mouse")
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
    
    filtered_dataset <- reactive({
        req(subgroup_samples())
        req(current_dataset_mapped())
        validSamples <- subgroup_samples()
        ds <- current_dataset_mapped()
        ds$Ts <- ds$Ts[, validSamples]
        return(ds)
    })
    
    filtered_metadata <- reactive({
        req(subgroup_samples())
        validSamples <- subgroup_samples()
        x <- current_metadata()
        x$nSample <- nnzero(validSamples)
        x$caseSamples <- x$caseSamples[validSamples]
        x$Tsample_metadata <- x$Tsample_metadata[, validSamples]
        if(analyze_group_differences()){
            gd <- selected_group_differences()
            x$samplesA <- gd$samplesA[validSamples]
            x$samplesB <- gd$samplesB[validSamples]
        }
        
        return(x)
    })

    source(file = "src/server_process_data_main.R", local=TRUE)

    ## The following are for heatmap and modal box inspection
    source(file = "src/server_process_data_bysample.R", local=TRUE)
    source(file = "src/server_process_proteindata_bysample.R", local=TRUE)
    
    source(file = "src/server_process_site_tables.R", local=TRUE)
    source(file = "src/server_process_protein_tables.R", local=TRUE)
    
    source(file = "src/server_volcanoplots.R", local=TRUE)
    source(file = "src/server_diagnostics.R", local=TRUE)
    source(file = "src/server_table_outputs.R", local=TRUE)
    
    source(file = "src/server_barplot_main.R", local=TRUE)
    source(file = "src/server_site_barplots.R", local=TRUE)
    source(file = "src/server_protein_barplots.R", local=TRUE)

    source(file = "src/server_heatmap_main.R", local=TRUE)
    source(file = "src/server_site_heatmaps.R", local=TRUE)
    source(file = "src/server_protein_heatmaps.R", local=TRUE)
    
    source(file = "src/server_kinase_network_functions.R", local=TRUE)
    source(file = "src/server_kinase_networks.R", local=TRUE)
    
    source(file = "src/server_modalbox_main.R", local=TRUE)
    source(file = "src/server_barplot_samplewise.R", local=TRUE)
    source(file = "src/server_modalbox_barplots.R", local=TRUE)
    source(file = "src/server_modalbox_tables.R", local=TRUE)
    
    output$buttonDownloadSampleData <- downloadHandler(
        filename = function() { paste('sample_data_snippet.csv', sep='') },
        content = function(file) {
            write.csv(Tsample_snippet, file = file, row.names = FALSE, quote=FALSE)
        }
    )
    
    output$buttonDownloadSampleMetaData <- downloadHandler(
        filename = function() { paste('sample_metadata.csv', sep='') },
        content = function(file) {
            write.csv(Tsample_metadata, file = file, row.names = FALSE, quote=FALSE)
        }
    )

})
