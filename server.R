
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
library(preprocessCore)

# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# 
# BiocManager::install("preprocessCore")

##
#library(BiocManager)
#options(repos = BiocManager::repositories())


#library(igraph)

library(ids)

#library(plotly)

source("compute_pvalues.R")
source("ui_util.R")

## Update Sample Data
folder = "data/"
Tsample <- read.csv(paste(folder, "rokaiXplorer_sample_data.csv", sep=""))
Tsample_metadata <- read.csv(paste(folder, "rokaiXplorer_sample_metadata.csv", sep=""))

foList <- function(...){
    x <- list(...)
    outList <- list()
    previous = NULL
    for(i in seq(1, length(x), 1)){
        if((i %% 2) == 0){
            outList[[previous]] <- x[[i]]
        }
        previous = x[[i]]
    }
    return(outList)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    observe_helpers(withMathJax = TRUE, help_dir = "helpfiles")
    
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
    
    observeEvent(input$buttonSampleData, {
        shinyWidgets::updatePickerInput(session, "refproteome", selected = "Uniprot Mouse");
        network_value("uniprot.mouse")
        myvalue("sample")
        reset('file1')
        reset('file2')
        upload_data_ready(FALSE)
        upload_metadata_ready(FALSE)
        #main_logging("Sample Data")
        # if(input$mainTabset == "About"){
        #     updateTabsetPanel(session, "mainTabset", "Plot")
        # }
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
    
    reactive_network <- reactive({
        req(initialized())
        switch (network_value(),
                "uniprot.human" = fname <- "rokai_network_data_uniprotkb.rds",
                "uniprot.mouse" = fname <- "rokai_network_data_uniprotkb_mouse.rds",
                validate(
                    need(FALSE, "Invalid network state.")
                )
        )
        NetworkData <- readRDS(paste("data/", fname, sep =""));
        proteins = unique(NetworkData$Site$Protein)
        indices = match(proteins, NetworkData$Site$Protein)
        names <- NetworkData$Site$Gene[indices]
        NetworkData$Protein <- data.frame(ID = proteins, Name = names)
        NetworkData$Site$Identifier <- str_c(NetworkData$Site$Gene, NetworkData$Site$Position, sep = "-")
        
        indices = match(NetworkData$Site$Protein, proteins)
        NetworkData$Wsite2protein <- sparseMatrix(
            i = 1:nrow(NetworkData$Site),
            j = indices,
            x = T,
            dims = c(nrow(NetworkData$Site), nrow(NetworkData$Protein))
        )
        
        indices = match(NetworkData$Kinase$KinaseID, proteins)
        rowindices = 1:nrow(NetworkData$Kinase)
        rowindices = rowindices[!is.na(indices)]
        indices = indices[!is.na(indices)]
        NetworkData$Wkinase2protein <- sparseMatrix(
            i = rowindices,
            j = indices,
            x = T,
            dims = c(nrow(NetworkData$Kinase), nrow(NetworkData$Protein))
        )
        NetworkData$Wkinase2site <- as(NetworkData$Wkinase2protein %*% t(NetworkData$Wsite2protein), "lgCMatrix")
        
        NetworkData$Wkin2protein <- as(NetworkData$Wkin2site %*% (NetworkData$Wsite2protein), "lgCMatrix")
        
        return (NetworkData)
    })
    
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
    
    observeEvent(input$file2, {
        inFile <- input$file2
        if (is.null(inFile))
            return(NULL)
        upload_metadata()
        req(upload_metadata())
    })
    
    upload_dataset <- reactive({
        library(tools)
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
        myvalue("upload")
        upload_data_ready(TRUE)
        upload_name(fileInfo$name)
        message(cat("Dataset is uploaded: ", fileInfo$name))
        
        validate(
            need(x$Protein, "File format error: Protein column is missing."),
            need(x$Position, "File format error: Position column is missing."),
            # need(x$Quantification, "File format error: Quantification column is missing.")
        )
        
        # validate(
        #     need(class(x$Quantification) == "numeric", "File format error: Quantification column must be numeric.")
        # )
        return(x)
    })
    
    upload_metadata <- reactive({
        library(tools)
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
        # validate(
        #     need(class(x$Quantification) == "numeric", "File format error: Quantification column must be numeric.")
        # )
        return(x)
    })
    
    reactive_dataset <- reactive({
        req(initialized())
        if(myvalue() == "upload"){
            validate(
                need(upload_data_ready(), "Waiting for data upload...")
            )
        }
        switch (myvalue(),
                "sample" = D <- Tsample,
                "upload" = D <- upload_dataset(),
                validate(
                    need(FALSE, "Waiting for data...")
                )
        )
        return (D)
    })
    
    reactive_metadata <- reactive({
        req(initialized())
        metadata_ready(FALSE)
        if(myvalue() == "upload"){
            validate(
                need(upload_metadata_ready(), "Waiting for metadata upload...")
            )
        }
        switch (myvalue(),
                "sample" = D <- Tsample_metadata,
                "upload" = D <- upload_metadata(),
                validate(
                    need(FALSE, "Waiting for data...")
                )
        )
        return (D)
    })
    
    current_dataset <- reactive({
        req(reactive_dataset())
        library(tidyverse)
        Tx <- reactive_dataset()
        Ts <- as.matrix(Tx %>% select(3:ncol(Tx)))
        ST <- (Tx %>% select(1:2))
        #T$ID = paste(T$Protein, T$Position, sep="_")
        return (list("Ts" = Ts, "ST" = ST))
    })
    
    current_metadata <- reactive({
        req(reactive_metadata())
        library(tidyverse)
        T_metadata <- reactive_metadata()
        
        colNames <- colnames(T_metadata)
        hasRowNames <- colNames[1] == "RowName"
        validate(
            need(hasRowNames, "The first column of the metadata should be the row names.")
        )
        rownames(T_metadata) <- T_metadata$RowName
        T_metadata <- T_metadata %>% select(2:ncol(T_metadata))
        rowNames <- rownames(T_metadata)
        hasGroup <- rowNames[1] == "Group"
        
        validate(
            need(hasGroup, "The first row of the metadata should be 'Group' indicating the Case/Control status of the samples.")
        )
        
        #hasGroup <- sum(is.na(Tsample_metadata["Group", ])) == 0;
        
        #validate(
        #    need(hasGroup, "Metadata should include a row named Group indicating the Case/Control status of the samples.")
            # need(hasGroup, "Metadata should include a row named Group indicating the Case/Control status of the samples.")
       # )
        
        group <- tolower(as.character(T_metadata["Group", ]))
        group_vals <- tolower(unique(group))
        nCase = sum(group == "case")
        nControl = sum(group == "control")
        nSample = ncol(T_metadata)
        
        validate(
            need(nCase > 0, "There should be at least one case sample in metadata."),
            need(nControl > 0, "There should be at least one control sample in metadata."), 
            need((nCase + nControl), "The group of each sample should be either case or control.")
        )
        
        caseSamples <- group == "case"
        
        x <- list()
        x$nSample <- nSample
        x$caseSamples <- caseSamples
        x$Tsample_metadata <- T_metadata[2:nrow(T_metadata), ]
        metadata_ready(TRUE)
        return(x)
    })
    
    current_dataset_mapped <- reactive({
        req(current_dataset())
        req(metadata_ready())
        
        ds <- current_dataset()
        x <- current_metadata()
        
        validate(
            need(x$nSample == ncol(ds$Ts), 
                 "The number of samples in the data should be the same as in the metadata."),
        )
        validate(
            need(sum(is.na(match(colnames(x$Tsample_metadata), colnames(ds$Ts)))) == 0, 
                 "The samples in the data should match the metadata.")
        )
        
        req(reactive_network())
        NetworkData <- reactive_network()
        
        proteins = ds$ST$Protein
        indices = match(proteins, NetworkData$Protein$ID)
        ds$ST$ProteinName <- NetworkData$Protein$Name[indices]
        
        ## Check the sample match
        
        validate(
            need(nnzero(!is.na(indices))>0, "Input mapping failed. Please check if the correct reference proteome is selected.")
        )
        
        return(ds)
    })
    
    foSubgoupSelectInput <- function(i, T_metadata) {
        q <- T_metadata[i,]
        rowname <- rownames(q)
        values <- unique(as.character(q))
        values = sort(values, decreasing =F)
        #message(rowname)
        li <- list()
        li$All <- 1
        for (j in 1:length(values)) {
            li[[values[j]]] <- j + 1
        }
        
        #foList("Feature Request", 1, "Comment", 2, "Bug Report", 3)
        tags$div(
            style = "margin-bottom: 0px;",
        selectInput(paste("subgroup_select", i, sep="") , rowname[1], 
                    choices = li, 
                    selected = 1, selectize = F, width = 170)  
        )
        #Tsample_metadata[i,]
        }
    
    output$subgroup_controls <- renderUI({
        validate(
           need(metadata_ready(), "")
        )
        x <- current_metadata()
        # tags$div(
        #     style = "margin-top: 8px; ", 
        #     tags$b("Select Subgroup: "), 
        #     tags$div(
        #         style = "padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
        #         
        #     )
        # )
        tags$div(
            lapply(1:nrow(x$Tsample_metadata), 
                   function(i) {foSubgoupSelectInput(i, x$Tsample_metadata)})
        )
    })
    
    output$group_difference_controls <- renderUI({
        validate(
            need(metadata_ready(), "")
        )
        x <- current_metadata()
        groups <- rownames(x$Tsample_metadata)
        # tags$div(
        #     style = "margin-top: 8px; ", 
        #     tags$b("Investigate Group Differences: "), 
        #     tags$div(
        #         style = "padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
        #         selectInput("select_group_differences" , "Select Group:", 
        #                     choices = c("None", groups), 
        #                     selected = 1, selectize = F, width = 170),
        #         uiOutput("select_subgroups_for_difference"),
        #     )
        # )
        tags$div(
            selectInput("select_group_differences" , "Select Group:", 
                        choices = c("None", groups), 
                        selected = 1, selectize = F, width = 170),
            uiOutput("select_subgroups_for_difference")
        )
    })
    
    output$select_subgroups_for_difference <- renderUI({
        validate(
            need(metadata_ready(), ""),
            need(!is.null(input$select_group_differences), ""),
            need(!is.na(input$select_group_differences), ""),
            need(!(input$select_group_differences == "None"), ""),
        )
        selected_group <- as.character(input$select_group_differences)
       # message(selected_group)
        if(selected_group != "None"){
            x <- current_metadata()
            groups <- rownames(x$Tsample_metadata)
            possible_choices <- unique(as.character(x$Tsample_metadata[selected_group, ]))
        } else {
            possible_choices = "Select a group first."
        }
        selected_two = possible_choices[min(2, length(possible_choices))]
        tags$div(
            tags$div(style="display:inline-block;", 
            selectInput("select_subgroup_A" , "A:", 
                        choices = possible_choices, 
                        selected = 1, selectize = F, width = 100)
            ),
            tags$div(style="display:inline-block;", 
            selectInput("select_subgroup_B" , "B:", 
                        choices = possible_choices, 
                        selected = selected_two, selectize = F, width = 100)
            )
        )
    })
    
    subgroup_samples <- reactive({
        req(current_metadata())
        x <- current_metadata()
        validSamples = rep(TRUE, x$nSample)
        for (i in 1:nrow(x$Tsample_metadata)){
            q <- input[[paste("subgroup_select", i, sep="")]]
            if(is.null(q)){ validate(need(FALSE, "")); }
            qv <- as.numeric(q)
            if(qv > 1){
                subgroups = x$Tsample_metadata[i,]
                values <- unique(as.character(subgroups))
                values = sort(values, decreasing =F)
                #message((subgroups == values[qv - 1]))
                validSamples = validSamples & (subgroups == values[qv - 1])
            }
        }
        return(validSamples)
    })
    
    observeEvent(input$select_group_differences, {
    })
    
    observeEvent(c(input$select_group_differences, input$select_subgroup_A, input$select_subgroup_B), {
        q <- input$select_group_differences
        if(is.null(q) | (as.character(q) == "None")){
            analyze_group_differences(FALSE)
            return();
        }
        qA <- input$select_subgroup_A
        qA <- input$select_subgroup_A
        qB <- input$select_subgroup_B
        if(is.null(qA) | is.null(qB)){
            analyze_group_differences(FALSE)
            return();
        }
        analyze_group_differences(TRUE)
    })
    
    selected_group_differences <- reactive({
        req(metadata_ready())
        x <- current_metadata()
        defaultOutput <- list(investigateGroupDifferencesEnabled = FALSE)
        q <- input$select_group_differences
        if(is.null(q) | (as.character(q) == "None")){
            validate(need(FALSE, ""))
            return(defaultOutput)
        }
        qA <- input$select_subgroup_A
        qB <- input$select_subgroup_B
        if(is.null(qA) | is.null(qB)){
            validate(need(FALSE, ""))
            return(defaultOutput)
        }
        selected_group = as.character(x$Tsample_metadata[as.character(q), ])
        selected_A <- as.character(qA)
        selected_B <- as.character(qB)
        samplesA <- selected_group == selected_A
        samplesB <- selected_group == selected_B
        
        validate(
            need(selected_A != selected_B, "Selected subgroups A and B should be different.")
        )
        
        return(list(investigateGroupDifferencesEnabled = TRUE, 
                    samplesA = samplesA, samplesB = samplesB))
    })
    
    
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
    
    processed_dataset <- reactive({
        req(filtered_dataset())
        req(filtered_metadata())
        ds <- filtered_dataset()
        Ts <- ds$Ts
        ST <- ds$ST
        Tmeta <- filtered_metadata()
        caseSamples <- Tmeta$caseSamples
        
        Tcase <- as.matrix(log2(Ts[, caseSamples]))
        Tcontrol <- as.matrix(log2(Ts[, !caseSamples]))
        
        nCase = ncol(Tcase)
        nControl = ncol(Tcontrol)
        
        validate(
            need((nCase+nControl)>0, "There are no samples in the selected subgroup."), 
            need((nCase)>0, "There are no case samples in the selected subgroup."), 
            need((nControl)>0, "There are no control samples in the selected subgroup.")
        )
        
        ## Sample Mean Balancing
        
        
        w_s = 1
        
        if(analyze_group_differences()){
          #  gd <- selected_group_differences()
            TcaseA = as.matrix(Tcase[, Tmeta$samplesA[caseSamples]])
            TcaseB = as.matrix(Tcase[, Tmeta$samplesB[caseSamples]])
            TcontrolA = as.matrix(Tcontrol[, Tmeta$samplesA[!caseSamples]])
            TcontrolB = as.matrix(Tcontrol[, Tmeta$samplesB[!caseSamples]])
            
            # message(colnames(TcaseB))
            # message(colnames(TcontrolB))
            
            McaseA <- apply(TcaseA, 1, function(x) mean(x, na.rm=T))
            McaseB <- apply(TcaseB, 1, function(x) mean(x, na.rm=T))
            McontrolA <- apply(TcontrolA, 1, function(x) mean(x, na.rm=T))
            McontrolB <- apply(TcontrolB, 1, function(x) mean(x, na.rm=T))
            
            Q_A <- McaseA - McontrolA;
            Q_B <- McaseB - McontrolB;
            
            Q <- Q_A - Q_B
            
            NcaseA <- apply(TcaseA, 1, function(x) nnzero(!is.na(x)))
            NcaseB <- apply(TcaseB, 1, function(x) nnzero(!is.na(x)))
            NcontrolA <- apply(TcontrolA, 1, function(x) nnzero(!is.na(x)))
            NcontrolB <- apply(TcontrolB, 1, function(x) nnzero(!is.na(x)))
            
            nCaseA = ncol(TcaseA)
            nCaseB = ncol(TcaseB)
            nControlA = ncol(TcontrolA)
            nControlB = ncol(TcontrolB)
            
            if((nCaseA >= 2) & (nControlA >= 2)){
                ScaseA <- apply(TcaseA, 1, function(x) sd(x, na.rm=T))
                SEcaseA <- ScaseA / sqrt(NcaseA - w_s)
                ScontrolA <- apply(TcontrolA, 1, function(x) sd(x, na.rm=T))
                SEcontrolA <- ScontrolA / sqrt(NcontrolA - w_s)
                SE_A <- sqrt(SEcaseA^2 + SEcontrolA^2)
                validsA <- (NcaseA >= 2) & (NcontrolA >= 2)
            } else {
                SE_A <- rep(sd(Q_A, na.rm = T), length(Q_A))
                validsA <- (NcaseA >= 1) & (NcontrolA >= 1)
            }
            
            if((nCaseB >= 2) & (nControlB >= 2)){
                ScaseB <- apply(TcaseB, 1, function(x) sd(x, na.rm=T))
                SEcaseB <- ScaseB / sqrt(NcaseB - w_s)
                ScontrolB <- apply(TcontrolB, 1, function(x) sd(x, na.rm=T))
                SEcontrolB <- ScontrolB / sqrt(NcontrolB - w_s)
                SE_B <- sqrt(SEcaseB^2 + SEcontrolB^2)
                validsB <- (NcaseB >= 2) & (NcontrolB >= 2)
            } else {
                SE_B <- rep(sd(Q_B, na.rm = T), length(Q_B))
                validsB <- (NcaseB >= 1) & (NcontrolB >= 1)
            }
            
            SE <- sqrt(SE_A^2 + SE_B^2)
            
            validSites = !is.na(Q) & !is.na(SE) & validsA & validsB
            
        } else {
            Mcase <- apply(Tcase, 1, function(x) mean(x, na.rm=T))
            Mcontrol <- apply(Tcontrol, 1, function(x) mean(x, na.rm=T))
            
            Q <- Mcase - Mcontrol;
            
            Ncase <- apply(Tcase, 1, function(x) nnzero(!is.na(x)))
            Ncontrol <- apply(Tcontrol, 1, function(x) nnzero(!is.na(x)))
            
            if((nCase >= 2) & (nControl >= 2)){
                Scase <- apply(Tcase, 1, function(x) sd(x, na.rm=T))
                SEcase <- Scase / sqrt(Ncase - w_s)
                Scontrol <- apply(Tcontrol, 1, function(x) sd(x, na.rm=T))
                SEcontrol <- Scontrol / sqrt(Ncontrol - w_s)
                SE <- sqrt(SEcase^2 + SEcontrol^2)
                valids <- (Ncase >= 2) & (Ncontrol >= 2)
            } else {
                SE <- rep(sd(Q, na.rm = T), length(Q))
                valids <- (Ncase >= 1) & (Ncontrol >= 1)
            }
            validSites = (!is.na(Q)) & valids
        }
        
        Xv = Q[validSites]
        Sx = SE[validSites]
        ST = ST[validSites, ]
        
        #Scase = apply(Tcase, 2, function(x) sd(x, na.rm=T))
        return (list("Xv" = Xv, "Sx" = Sx, "ST"= ST,"validSites" = validSites))
    })
    
    processed_data_bysample <- reactive({
        req(filtered_dataset())
        req(filtered_metadata())
        ds <- filtered_dataset()
        Ts <- ds$Ts
        ST <- ds$ST
        Tmeta <- filtered_metadata()
        caseSamples <- Tmeta$caseSamples
        
        Tcase <- as.matrix(log2(Ts[, caseSamples]))
        Tcontrol <- as.matrix(log2(Ts[, !caseSamples]))
        
        nCase = ncol(Tcase)
        nControl = ncol(Tcontrol)
        
        validate(
            need((nCase+nControl)>0, "There are no samples in the selected subgroup."), 
            need((nCase)>0, "There are no case samples in the selected subgroup."), 
            need((nControl)>0, "There are no control samples in the selected subgroup.")
        )
        
        Mcontrol <- apply(Tcontrol, 1, function(x) mean(x, na.rm=T))
        
        Q <- Tcase - Mcontrol;
        SE <-apply(Q, 2, function(x) rep(sd(x, na.rm = T), length(x)))
        
        Ncase <- apply(Tcase, 1, function(x) nnzero(!is.na(x)))
        Ncontrol <- apply(Tcontrol, 1, function(x) nnzero(!is.na(x)))
        
        valids <- (Ncase >= 1) & (Ncontrol >= 1)
        validSites = valids
        
        Xv = Q[validSites, ]
        Sx = SE[validSites, ]
        ST = ST[validSites, ]
        Ts = Ts[validSites, ]
        Ts <- log2(Ts)
        
        return (list("Xv" = Xv, "Sx" = Sx, "ST"= ST, "Ts" = Ts, "validSites" = validSites, "Tmeta" = Tmeta))
    })
    
    processed_protein_data_bysample <- reactive({
        req(processed_data_bysample())
        ds <- processed_data_bysample()
        ST <- ds$ST
        Xv <- ds$Xv
        Sx <- ds$Sx
        Ts <- ds$Ts
        Tmeta = ds$Tmeta
        proteins = unique(ST$Protein)
        indices = match(proteins, ST$Protein)
        names <- ST$ProteinName[indices]
        Protein <- data.frame(ID = proteins, Name = names)
        
        indices = match(ST$Protein, Protein$ID)
        Wprotein2site <- sparseMatrix(
            i = indices,
            j = 1:nrow(ST), 
            x = TRUE,
            dims = c(nrow(Protein), nrow(ST))
        )
        
        A <- as.matrix((Wprotein2site %*% Xv) / rowSums(Wprotein2site))
        SE = as.matrix(sqrt((Wprotein2site^2)%*%(Sx^2)) / rowSums(Wprotein2site))
        Z = as.matrix(A / SE)
        Tp <- as.matrix((Wprotein2site %*% Ts) / rowSums(Wprotein2site))
        
        Navailable <- apply(A, 1, function(x) nnzero(!is.na(x)))
        
        
        return (list("Xv" = A, "Sx" = SE, "Ts" = Tp, "PT"= Protein, "Tmeta" = Tmeta))
    })
    
    
    preprocessed_dataset <- reactive({
        req(processed_dataset())
        ds <- processed_dataset()
        ds$Xv = ds$Xv - mean(ds$Xv, na.rm = T)
        return (ds)
    })
    
    
    site_table <- reactive({
        req(preprocessed_dataset())
    #    req(processed_protein_data_bysample())
    #    req(processed_data_bysample())
        ds <- preprocessed_dataset();
        
        validSites = ds$validSites
        Xv = ds$Xv
        Sx = ds$Sx
        Zx = Xv / Sx
        res = compute_pvalues(as.matrix(Zx))
        
        NetworkData <- reactive_network()
        ST = ds$ST[, c("Protein", "ProteinName", "Position")]
        ST$Phos = Xv
        ST$StdErr = Sx
        ST$ZScore = Zx
        ST$PValue = res$PValues
        ST$FDR = res$QValues
        ST$MagnitudeAdj <- abs(Xv) - 3 * Sx;
        
        validate(
            need(nrow(ST) > 0, "There are no sites identified in the selected subgroup. Please make sure there are no conflicts in the subgroup selection.")
        )
        
        return (ST)
    })
    
    site_table_processed <- reactive({
        req(site_table())
        ST <- site_table()
        max_fdr = input$sitelevel_volcano_maxfdr
        min_logfc = input$sitelevel_volcano_minlogfc
        ST$isSignificant = (ST$FDR <= max_fdr) & (abs(ST$Phos) >= min_logfc)
        return(ST)
    })
    
    protein_table <- reactive({
        req(site_table())
        #req(reactive_network())
        #NetworkData <- reactive_network()
        #Wprotein2site <- t(NetworkData$Wsite2protein)
        
        ST <- site_table();
        proteins = unique(ST$Protein)
        indices = match(proteins, ST$Protein)
        names <- ST$ProteinName[indices]
        Protein <- data.frame(ID = proteins, Name = names)
        
        #message(max(indices))
        #message(nrow(Protein))
        #message(nrow(ST))
        
        # validate(
        #    need(nrow(ST) > 0, "There are no proteins identified in the selected subgroup. Please make sure there are no concflicts in the subgroup selection.")
        # )
        
        indices = match(ST$Protein, Protein$ID)
        Wprotein2site <- sparseMatrix(
            i = indices,
            j = 1:nrow(ST), 
            x = TRUE,
            dims = c(nrow(Protein), nrow(ST))
        )
        
        A <- as.numeric((Wprotein2site %*% ST$Phos) / rowSums(Wprotein2site))
        SE = as.numeric(sqrt((Wprotein2site^2)%*%(ST$StdErr^2)) / rowSums(Wprotein2site))
        Z = as.numeric(A / SE)
        valids = !is.na(Z)
        
        PT = Protein[valids, c("ID", "Name")]
        PT$Phos = A[valids]
        PT$StdErr = SE[valids]
        PT$ZScore = Z[valids]
        res = compute_pvalues(as.matrix(PT$ZScore))
        PT$PValue = res$PValues
        PT$FDR = res$QValues
        PT$MagnitudeAdj <- abs(PT$Phos) - 3 * PT$StdErr;
        #PT$isSignificant = (PT$FDR <= 0.1) & (abs(PT$Phos) >= log2(1.25))
        
        return (PT)
    })
    
    protein_table_processed <- reactive({
        req(protein_table())
        PT <- protein_table()
        max_fdr = input$proteinlevel_volcano_maxfdr
        min_logfc = input$proteinlevel_volcano_minlogfc
        PT$isSignificant = (PT$FDR <= max_fdr) & (abs(PT$Phos) >= min_logfc)
        return(PT)
    })
    
    main_volcanoplot <- function(ST, min_logfc){
        ST$logP = pmin(-log10(ST$PValue), 10)
        
        thr = min(ST$logP[ST$isSignificant]);
        xMax = round(max(abs(ST$Phos)), digits = 0)
        
        defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
        
        # clr <- c('#0072BD','#E69F00')
        clr <- defaultcolors[1:2]
        ggplot(ST, aes(x=Phos, y=logP, color = isSignificant)) + 
            theme_bw() +
            geom_point(size=3.5) + 
            geom_hline(yintercept=(thr), linetype="dashed", color = '#555555') + 
            geom_vline(xintercept=c(-min_logfc, min_logfc), linetype="dashed", color = '#333333') + 
            
            theme(text = element_text(size = 18)) + 
            labs(x = "Log2-FC", y = "-log10(P-Value)") + 
            scale_color_manual(name = "", values=clr, labels = c("Not Significant", "Significant")) +
            ylim(0, 10) + 
            xlim(-xMax, xMax)
        #+ theme(legend.position="top") 
    }
    
    output$sitelevel_volcano <- renderPlot({
        req(site_table_processed())
        ST <- site_table_processed()
        minlogfc = input$sitelevel_volcano_minlogfc
        main_volcanoplot(ST, minlogfc)
    })
    
    output$proteinlevel_volcano <- renderPlot({
        req(protein_table_processed())
        PT <- protein_table_processed()
        minlogfc = input$proteinlevel_volcano_minlogfc
        main_volcanoplot(PT, minlogfc)
    })
    
    output$sitelevel_volcano_summary <- renderText({
        req(site_table_processed())
        ST <- site_table_processed()
        return(paste("Number of significant sites:", nnzero(ST$isSignificant)))
    })
    
    output$proteinlevel_volcano_summary <- renderText({
        req(protein_table_processed())
        PT <- protein_table_processed()
        return(paste("Number of significant proteins:", nnzero(PT$isSignificant)))
    })
    
    output$histogram_sitecentering <- renderPlot({
        req(processed_dataset())
        ds <- processed_dataset()
        ST = ds$ST
        ST$Phos = ds$Xv
        #hist(ST$Phos)
        m <- mean(ST$Phos, na.rm = T)
        ggplot(ST, aes(x=Phos)) +
            geom_histogram(color="black", fill="lightblue") +
            #theme_classic() +
            theme_bw() +
            theme(text = element_text(size = 16)) + 
           # theme(panel.grid = element_line(color = "#8ccde3",
          #                                  size = 0.75,
           #                                 linetype = 2)) + 
            labs(x = "Log2-FC", y = "Frequency") +
            ggtitle(paste("Mean: ", round(m, digit=2), sep = "")) + 
            theme(plot.title = element_text(hjust = 0.5)) 
    })
    
    barplot <- function(K, minzscore, topk, yaxis, coloring, show_significant_only){
        Ks <- K[!is.na(K$Phos),]
        Ks <- Ks[abs(Ks$ZScore) >= minzscore,]
        if(show_significant_only == T){
            Ks <- Ks[Ks$isSignificant, ]
        }
        
        #zstar = max(minzscore, 3)
        zstar = 3
        si <- order(abs(Ks$Phos) - zstar*Ks$StdErr, decreasing = TRUE)
        #si <- order(abs(Ks$ZScore), decreasing = TRUE)
        
        valids <- si[1:min(topk, length(si))]
        Ks <- Ks[valids, ]
        
        si <- order(Ks$Phos, decreasing = TRUE)
        Ks <- Ks[si,]
        
        c_limit = 4
        Ks$ColoringVar = Ks$ZScore
        Ks$ColoringVar = pmax( -1*c_limit, pmin(Ks$ColoringVar, c_limit))
        
        significanceColoring = FALSE
        if(coloring == "Significance"){
            Ks$ColoringVar <- !Ks$isSignificant
            #Ks$ColoringVar <- ifelse(!Ks$isSignificant, "NotSig", ifelse(Ks$Phos > 0, "SigPos", "SigNeg"))
            significanceColoring = TRUE
        }
        
        ## Add custom color scaling - If needed
        ## Add XLabel Coloring - If needed
        
        Ks$Sorting = -1*Ks$Phos
        Ks$Yaxis = Ks$Phos
        yaxisText = "Log2-FC"
       # yaxisText = yaxistxt_main
        showErrorBars = TRUE
        if(yaxis == "Z-Score"){
            Ks$Yaxis = Ks$ZScore
            Ks$Sorting = -1*Ks$ZScore
            yaxisText = "Z-Score"
            showErrorBars = FALSE
        }
        
        p <- ggplot(data=Ks, aes(x=reorder(ID, Sorting), y=Yaxis, fill = ColoringVar)) +
            geom_bar(stat="identity", width=0.8, col="#333333", size = 0.75) +
            theme_minimal() +
            theme(text = element_text(size=18),
                  axis.text.x = element_text(size = 16, angle=90, hjust=1, face = "bold"),
                  legend.key.height = unit(1.25, "cm"))
        
        if(showErrorBars){ # Show errorabars
            p <- p + geom_errorbar(aes(ymin=Phos-1.96*StdErr, ymax=Phos+1.96*StdErr), width=.5, size = 0.95)
        }
        
        #  defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
        #clr <- c('#F10000', '#CCCCCC')
        clr <- c('#E69F00', '#CCCCCC')
        #clr <- c('#F10000', '#0000F1', '#CCCCCC')
       # clr <- c('#CCCCCC', '#F10000', '#0000F1')
       # , labels = c("Not Significant", "Significant Neg", "Significant Pos")
        
        if(significanceColoring){
            p <- p + scale_fill_manual(name = "", values=clr, labels = c("Significant", "Not Significant"))
        } else {
            p <- p + scale_fill_distiller(palette = "RdYlBu", type = "div", limit = c_limit * c(-1, 1))
        }
        
        p <- p + labs(fill = "Z-Score", x = "", y = yaxisText)
        return (p)
    }
    
    siteDownloadPlotDLHandler <- function(plot, file_name, file_type){
        downloadHandler(
            filename = function() { paste(file_name, file_type, sep='.') },
            content = function(file) {
                h = 4.6
                message("fjjf")
                ggsave(file, plot = siteBarPlot(), device = file_type, width=3*h, height=h)
            },
            contentType = paste("application/", file_type, sep = "")
        )
    }
    
    proteinDownloadPlotDLHandler <- function(plot, file_name, file_type){
        downloadHandler(
            filename = function() { paste(file_name, file_type, sep='.') },
            content = function(file) {
                h = 4.6
                message("fjjf")
                ggsave(file, plot = proteinBarPlot(), device = file_type, width=3*h, height=h)
            },
            contentType = paste("application/", file_type, sep = "")
        )
    }
    
    siteBarPlot <- reactive({
        req(site_table_processed())
        ST <- site_table_processed()
        ST$NameX = ST$ProteinName
        ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
        ST$ID <- str_c(ST$NameX, ST$Position, sep = "-")
        minzscore = input$site_barplot_minzscore
        topk = input$site_barplot_maxitems
        yaxis = input$site_barplot_yaxis
        coloring = input$site_barplot_coloring
      #  yaxistxt_main = "Site Phosphorylation"
        show_significant_only = input$site_barplot_significant_only
        barplot(ST, minzscore, topk, yaxis, coloring, show_significant_only)
    })
    
    output$site_barplot_plot <- renderPlot({
        siteBarPlot()
    })
    
    output$site_barplot_downloadPlotPNG <- siteDownloadPlotDLHandler(
        siteBarPlot(), file_name = "site-barplot", file_type = "png")
    
    output$site_barplot_downloadPlotPDF <- siteDownloadPlotDLHandler(
        siteBarPlot(), file_name = "site-barplot", file_type = "pdf")
    
    heatmapMain <- function(ST, STx, ds, minzscore, topk, show_significant_only, intensity_fc_style, items_txt, groupings = c()){
        show_intensity = intensity_fc_style == "Both case and control"

        #Z = ds$Xv/ds$Sx
        Z = ds$Xv
        Ts <- ds$Ts
        Tmeta <- ds$Tmeta

        indices = match(ST$ID, STx$ID)
        valids = !is.na(indices)
        error_no_items_txt <- paste("There are no", items_txt, "to show for the specified options.")
        validate(
            need(nnzero(valids) > 1, error_no_items_txt)
        )
        Z <- Z[valids, ]
        ST <- ST[valids, ]
        Ts <- Ts[valids, ]
        indices <- indices[valids]

        STv = STx[indices, ]

        Ts = Ts - apply(Ts, 2, function(x) mean(x, na.rm=T))
        Ts = Ts - apply(Ts, 1, function(x) mean(x, na.rm=T))

        #Ts <- as.data.frame(normalize.quantiles(as.matrix(Ts)))
        #Z[is.na(Z)] = 0


        rownames(Z) <- ST$ID
        colnames(Ts) <- colnames(ds$Ts)
        rownames(Ts) <- ST$ID

        avgZ = apply(Z, 1, function(x) mean(x, na.rm=T))
        avgAbsZ = apply(Z, 1, function(x) mean(abs(x), na.rm=T))
        sumAbsZ = apply(Z, 1, function(x) sum(abs(x), na.rm=T)) / ncol(Z)
        #message(ncol(Z))
        #valids = (abs(sumAbsZ) >= (2 * ncol(Z))) & (abs(avgZ) >= 1)
        # valids = STv$isSignificant
        #valids = rep(T, nrow(STv), 1) & (abs(STv$ZScore) >= minzscore)
        valids = !is.na(sumAbsZ) & (abs(sumAbsZ) >= minzscore)
        #valids = !is.na(avgAbsZ) & (abs(avgAbsZ) >= minzscore)
        if(show_significant_only){
            valids = valids & (STv$isSignificant)
        }
        #& (abs(STv$ZScore) >= 3.5)

        validate(
            need(nnzero(valids) > 1, error_no_items_txt)
        )
        Z <- as.matrix(Z[valids, ])
        ST <- ST[valids, ]
        Ts <- Ts[valids, ]
        STv <- STv[valids, ]

        sort_score = abs(STv$Phos) - 3 * STv$StdErr
        #avgZ = apply(Z, 1, function(x) mean(x, na.rm=T))
        si <- order(sort_score, decreasing = TRUE)
        # topk = length(Z)
        valids <- si[1:min(topk, length(si))]

        Z <- as.matrix(Z[valids, ])
        ST <- ST[valids, ]
        STv <- STv[valids, ]
        Ts <- Ts[valids, ]

        valids <- order(STv$Phos, decreasing = TRUE)
        Z <- as.matrix(Z[valids, ])
        ST <- ST[valids, ]
        Ts <- Ts[valids, ]

        #caseSamples = !is.na(match(colnames(Ts), colnames(Z)))
      #  message(nnzero(caseSamples))
        
        #groupings = c("Timepoint", "Gender")
        if(show_intensity){
            Tq <- Ts
           # Tq$caseSamples = caseSamples
            data_melt <- melt(Ts)
            data_melt$caseSamples <- ifelse(!is.na(match(data_melt$X2, colnames(Z))), "Case", "Control")

            #data_melt$Grouping = t(Tmeta$Tsample_metadata["Gender", indices])
            
            fill_txt = "Log2-Intensity"
        } else {
            data_melt <- melt(Z)
            fill_txt = "Log2-FC"
        }
        indices = match(data_melt$X2, colnames(Tmeta$Tsample_metadata))
        nGrouping = length(groupings)
        for(iGroup in range(1, nGrouping)){
          grp_txt = paste("Grouping", iGroup, sep = "")
          data_melt[[grp_txt]] = t(Tmeta$Tsample_metadata[groupings[iGroup], indices])
        }
        
        
        # data_melt[data_melt < -4] = -4
        # data_melt[data_melt > 4] = 4

       # defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
        
        ggp <- ggplot(data_melt, aes(X1, X2)) +                           # Create heatmap with ggplot2
            geom_tile(aes(fill = value))
        if(show_intensity){
            #ggp = ggp + facet_grid(caseSamples+Grouping~., scales='free', switch = "y")
           # ggp = ggp + facet_grid(caseSamples~., scales='free', switch = "y")
            
            if(nGrouping <= 0){
              ggp = ggp + facet_grid(rows = vars(caseSamples), scales='free', switch = "y")
            }
            if(nGrouping == 1){
              ggp = ggp + facet_grid(rows = vars(caseSamples, Grouping1), scales='free', switch = "y")
            }
            if(nGrouping >= 2){
              ggp = ggp + facet_grid(rows = vars(caseSamples, Grouping1, Grouping2), scales='free', switch = "y")
            }
        } else {
          if(nGrouping == 1){
            ggp = ggp + facet_grid(rows = vars(Grouping1), scales='free', switch = "y")
          }
          if(nGrouping >= 2){
            ggp = ggp + facet_grid(rows = vars(Grouping1, Grouping2), scales='free', switch = "y")
          }
        }
        
        ggp = ggp + theme(strip.text.y = element_text(size = 16))
        ggp = ggp + theme(strip.background = element_rect(color = "#000000", fill = "#F9F9B7"))
        
        ggp = ggp + scale_fill_gradient2(name = fill_txt, low = "blue", mid = "white", high = "red", na.value = "#336633")
        #ggp = ggp + scale_fill_gradientn(colours = c("blue", "white", "red"))

        #ggp = ggp + scale_fill_gradient(low = "white", high = "red", na.value = "#336633")
        ggp = ggp + theme(axis.text.x=element_text(angle =- 90, vjust = 0.5, hjust = 0, size=15))
        ggp = ggp + theme(axis.text.y=element_text(vjust = 0, hjust = 0.5, size=11))
        ggp = ggp + labs(x = "", y = "")

        return(ggp)
    }
    
    siteDownloadHeatmapDLHandler <- function(plot, file_name, file_type){
        downloadHandler(
            filename = function() { paste(file_name, file_type, sep='.') },
            content = function(file) {
                h = 4.6
                message("fjjf")
                ggsave(file, plot = siteHeatmap(), device = file_type, width=3*h, height=h)
            },
            contentType = paste("application/", file_type, sep = "")
        )
    }
    
    output$site_heatmap_select_group_ui <- renderUI({
        validate(
            need(metadata_ready(), "")
        )
        x <- current_metadata()
        groups <- rownames(x$Tsample_metadata)
        tags$div(
            multiChoicePicker("site_heatmap_select_group", "Grouping:", groups, isInline = "F", multiple = T, max_opts = 1)
        )
    })
    
    output$site_heatmap_downloadPlotPNG <- siteDownloadHeatmapDLHandler(
        siteHeatmap(), file_name = "site-heatmap", file_type = "png")
    
    output$site_heatmap_downloadPlotPDF <- siteDownloadHeatmapDLHandler(
        siteHeatmap(), file_name = "site-heatmap", file_type = "pdf")
    
    siteHeatmap <- reactive({
        req(processed_data_bysample())
        ds <- processed_data_bysample()
        
        STx <- site_table_processed()
        STx$NameX = STx$ProteinName
        STx$NameX[is.na(STx$NameX)] = STx$Protein[is.na(STx$NameX)]
        STx$ID <- str_c(STx$NameX, STx$Position, sep = "-")
        
        ST <- ds$ST
        ST$NameX = ST$ProteinName
        ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
        ST$ID <- str_c(ST$NameX, ST$Position, sep = "-")
        
        minzscore = input$site_heatmap_minzscore
        topk = input$site_heatmap_maxitems
        show_significant_only = input$site_heatmap_significant_only
        intensity_fc_style = input$site_heatmap_intensity_fc_style
        groupings = input$site_heatmap_select_group
        
        message(input$site_heatmap_coloring)
        message(class(input$site_heatmap_coloring))
        heatmapMain(ST, STx, ds, minzscore, topk, 
                    show_significant_only, intensity_fc_style, "sites",
                    groupings = groupings)
    })
    
    output$site_heatmap <- renderPlot({
        siteHeatmap()
    })
    
    ## Protein heatmaps
    
    output$protein_heatmap_select_group_ui <- renderUI({
      validate(
        need(metadata_ready(), "")
      )
      x <- current_metadata()
      groups <- rownames(x$Tsample_metadata)
      tags$div(
        multiChoicePicker("protein_heatmap_select_group", "Grouping:", groups, isInline = "F", multiple = T, max_opts = 1)
      )
    })
    
    proteinHeatmap <- reactive({
        req(processed_protein_data_bysample())
        ds <- processed_protein_data_bysample()
        
        PTx <- protein_table_processed()
        PTx$NameX = PTx$Name
        PTx$NameX[is.na(PTx$NameX)] = PTx$ID[is.na(PTx$NameX)]
        PTx$ID <- PTx$NameX
        
        PT <- ds$PT
        PT$NameX = PT$Name
        PT$NameX[is.na(PT$NameX)] = PT$ID[is.na(PT$NameX)]
        PT$ID <- PT$NameX
        
        minzscore = input$protein_heatmap_minzscore
        topk = input$protein_heatmap_maxitems
        show_significant_only = input$protein_heatmap_significant_only
        intensity_fc_style = input$protein_heatmap_intensity_fc_style
        groupings = input$protein_heatmap_select_group
        heatmapMain(PT, PTx, ds, minzscore, topk, 
                    show_significant_only, intensity_fc_style, "proteins",
                    groupings = groupings)
    })
    
    output$protein_heatmap <- renderPlot({
        proteinHeatmap()
    })
    
    proteinDownloadHeatmapDLHandler <- function(plot, file_name, file_type){
        downloadHandler(
            filename = function() { paste(file_name, file_type, sep='.') },
            content = function(file) {
                h = 4.6
                message("fjjf")
                ggsave(file, plot = siteHeatmap(), device = file_type, width=3*h, height=h)
            },
            contentType = paste("application/", file_type, sep = "")
        )
    }
    
    output$protein_heatmap_downloadPlotPNG <- proteinDownloadHeatmapDLHandler(
        proteinHeatmap(), file_name = "protein-heatmap", file_type = "png")
    
    output$protein_heatmap_downloadPlotPDF <- proteinDownloadHeatmapDLHandler(
        proteinHeatmap(), file_name = "protein-heatmap", file_type = "pdf")
    
    
    ## Protein Bar Plots
    
    proteinBarPlot <- reactive({
        req(protein_table_processed())
        PT <- protein_table_processed()
        PT$NameX = PT$Name
        PT$NameX[is.na(PT$NameX)] = PT$ID[is.na(PT$NameX)]
        PT$ID <- PT$NameX
        minzscore = input$protein_barplot_minzscore
        topk = input$protein_barplot_maxitems
        yaxis = input$protein_barplot_yaxis
        coloring = input$protein_barplot_coloring
      #  yaxistxt_main = "Protein Phosphorylation"
        show_significant_only = input$protein_barplot_significant_only
        barplot(PT, minzscore, topk, yaxis, coloring, show_significant_only)
    })
    
    output$protein_barplot_plot <- renderPlot({
        proteinBarPlot()
    })
    
    output$protein_barplot_downloadPlotPNG <- proteinDownloadPlotDLHandler(
        proteinBarPlot(), file_name = "protein-barplot", file_type = "png")
    
    output$protein_barplot_downloadPlotPDF <- proteinDownloadPlotDLHandler(
        proteinBarPlot(), file_name = "protein-barplot", file_type = "pdf")
    
    
    foKinaseNetworkSubset <- function(ST, NetworkData, indices, Wkin2site, Wkin2onsite){
        ST = ST[!is.na(indices), ]
        indices = indices[!is.na(indices)]
        Wk2s = Wkin2site[, indices]
        Wk2os = Wkin2onsite[, indices]
        validKins = (rowSums(Wk2s) + rowSums(Wk2os)) > 0
        validSites = (colSums(Wk2s) + colSums(Wk2os)) > 0
        KT = NetworkData$Kinase[validKins, ]
        ST = ST[validSites, ]
        Wk2s = Wk2s[validKins, validSites]
        Wk2os = Wk2os[validKins, validSites]
        return(list(KT = KT, ST = ST, Wk2s = Wk2s, Wk2os = Wk2os))
    }
    
    foKinaseNetworkDraw <- function(K, KT, Wk2s, Wk2os, minzscore, topk, keepsinglekinases, items_txt, footer_txt, show_significant_only){
        thereAreNoItemsError = paste("There are no", items_txt, "that can pass the specified threshold.")
        valids = (abs(K$ZScore) >= minzscore)
        if(show_significant_only == T){
            valids = valids & K$isSignificant
        }
        validate(
            need(nnzero(valids) > 0, thereAreNoItemsError)
        )
        
        Ks <- K[valids,]
        Wk2s = Wk2s[, valids]
        Wk2os = Wk2os[, valids]
        
        zstar = 3
        si <- order(abs(Ks$Phos) - zstar*Ks$StdErr, decreasing = TRUE)
        valids <- si[1:min(topk, length(si))]
        Ks <- Ks[valids, ]
        Wk2s <- Wk2s[, valids]
        Wk2os <- Wk2os[, valids]
        
        #validKins = (rowSums(Wk2s) + rowSums(Wk2os)) > !keepsinglekinases
        validKins = (rowSums(Wk2s) > !keepsinglekinases) | (rowSums(Wk2os) > 0)
        
        validate(
            need(nnzero(validKins) > 0, "There are no kinases to show for the specified options.")
        )
        
        KT = KT[validKins, ]
        
        if(nnzero(validKins)  == 1){
            Wk2s = t(as.matrix(Wk2s[validKins, ]))
            Wk2os = t(as.matrix(Wk2os[validKins, ]))
        } else {
            Wk2s =(Wk2s[validKins, ])
            Wk2os = (Wk2os[validKins, ])
        }
        
        validSites = (colSums(Wk2s) + colSums(Wk2os)) > 0
        Ks = Ks[validSites, ]
        Wk2s = Wk2s[, validSites]
        Wk2os = Wk2os[, validSites]
        
        validate(
            need(nnzero(validSites) > 0, thereAreNoItemsError)
        )
        
        nItem = nrow(Ks)
        nKinase = nrow(KT)
        
        c_limit = 4
        coloringVar = Ks$ZScore
        #Ks$ColoringVar = pmax( -1*c_limit, pmin(Ks$ColoringVar, c_limit))
        
        aconst = 1.4;
        fq <- function(x) 1 - 2/(1+2^(x/aconst));
        qScaled = as.matrix(fq(coloringVar))
        n = nrow(qScaled)
        fc <- function(a,b,c) t(as.matrix(c(a,b,c)))
        mA = (as.matrix(pmax(qScaled, 0)) %*% fc(+0.04, -0.96, -0.96))
        mB = (as.matrix(abs(pmin(qScaled, 0))) %*% fc(-0.96, -0.96, +0.04))
        clx <- matrix(0.96, nrow = n, ncol = 3) + mA + mB
        
        color_hex = apply(clx, 1, function(x) rgb(x[1], x[2], x[3]))
        
        # color_hex = sapply(colors, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
        
        
        siteIds = nKinase + (1:nItem)
        kinaseIds = 1:nKinase
        phosConf = paste0(round(Ks$Phos, digits = 2), 
                          " [", round(Ks$Phos - 1.96*Ks$StdErr * sign(Ks$Phos), digits = 2), 
                          ", ", round(Ks$Phos + 1.96*Ks$StdErr * sign(Ks$Phos), digits = 2), "]", sep = "")
        
        item_txt = paste(toupper(substr(items_txt, 1, 1)), substr(items_txt, 2, nchar(items_txt) - 1), sep = "")
        
        line1 = c(rep("", nKinase), paste0("<br> ZScore: ", round(Ks$ZScore, digits = 3), sep = ""))
        line2 = c(rep("", nKinase), paste0("<br> log2-FC: ", phosConf, sep = ""))
        line3 = c(rep("", nKinase), paste0("<br> FDR: ", round(Ks$FDR, digits = 3), sep = ""))
        
        
        names = c(KT$KinaseName, Ks$ID)
        nodes = data.frame(id = c(kinaseIds, siteIds), label = names,
                           group = c(rep("Kinase", nKinase), rep(item_txt, nItem)),
                           color = c(rep("orange", nKinase), color_hex),
                           title = paste0("<b>", names, "</b>", line1, line2, line3))
        
        a <- which(Wk2s, arr.ind = T)
        nRowA = nrow(a)
        if(length(a) > 0){
            a[, 2] =  a[, 2] + nKinase
        } else {
            nRowA = 0
        }
        b <- which(t(Wk2os), arr.ind = T)
        nRowB = nrow(b)
        if(length(b) > 0){
            b[, 1] =  b[, 1] + nKinase
        } else {
            nRowB =0
        }
        C = rbind(a, b)
        
        edges = data.frame(from = C[, 1], to = C[, 2], 
                           width = c(rep(6, nRowA), rep(10, nRowB)),
                           color = c(rep("black", nRowA), rep("#EE9900", nRowB)),
                           arrows = c(rep("to", nRowA), rep("", nRowB))
        )
        
        ledges = data.frame(width = c(20, 30), 
                            color = c("black", "EE9900"),
                            label = c("Phosphorylates", "Is on the kinase"), 
                            arrows = c("", ""),
                            shadow = c(F, F))
        
        visNetwork(nodes, edges, width = "100%",
                   main = paste("Kinases connected to the identified", items_txt),
                   footer = footer_txt) %>%
            visEdges(shadow = FALSE,
                     arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
                     color = list(color = "black", highlight = "red")) %>%
            visGroups(groupname = "Kinase", color = "orange", shape = "rectangle", 
                      shadow = list(enabled = TRUE), font = list(size = 30)) %>% 
            visGroups(groupname = item_txt, color = "darkred", shape = "square", font = list(size = 26)) %>%
            visLegend(width = 0.13, position = "right") %>%
            visEdges(smooth = FALSE) %>%
            visInteraction(hideEdgesOnDrag = TRUE) %>%
            visInteraction(navigationButtons = TRUE) %>%
            visPhysics(stabilization = T) 
    }
    
    site_kinase_network <- reactive({
        req(site_table_processed())
        req(reactive_network())
        ST <- site_table_processed()
        ST$NameX = ST$ProteinName
        ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
        ST$ID <- str_c(ST$NameX, ST$Position, sep = "-")
        NetworkData <- reactive_network()
        ST = ST[!is.na(ST$Phos), ]
        indices = match(ST$ID, NetworkData$Site$Identifier)
        return(foKinaseNetworkSubset(ST, NetworkData, indices, 
                                     NetworkData$Wkin2site, 
                                     NetworkData$Wkinase2site))
    })
    
    output$site_kinase_network <- renderVisNetwork({
        req(site_kinase_network())
        ds <- site_kinase_network()
        minzscore = input$site_kinase_network_minzscore
        topk = input$site_kinase_network_maxitems
        keepsinglekinases = input$site_kinase_network_single_kinases
        show_significant_only = input$site_kinase_network_significant_only
        footer_txt = "Orange edges indicate the site is on the kinase."
        return(foKinaseNetworkDraw(ds$ST, ds$KT, ds$Wk2s, 
                                   ds$Wk2os, minzscore, topk, 
                                   keepsinglekinases, "sites", 
                                   footer_txt, show_significant_only))
    })
    
    protein_kinase_network <- reactive({
        req(protein_table_processed())
        req(reactive_network())
        PT <- protein_table_processed()
        PT$NameX = PT$Name
        PT$NameX[is.na(PT$NameX)] = PT$ID[is.na(PT$NameX)]
        PT$ID <- PT$NameX
        NetworkData <- reactive_network()
        PT = PT[!is.na(PT$Phos), ]
        indices = match(PT$Name, NetworkData$Protein$Name)
        return(foKinaseNetworkSubset(PT, NetworkData, indices, 
                                     NetworkData$Wkin2protein, 
                                     NetworkData$Wkinase2protein))
    })
    
    output$protein_kinase_network <- renderVisNetwork({
        req(protein_kinase_network())
        ds <- protein_kinase_network()
        minzscore = input$protein_kinase_network_minzscore
        topk = input$protein_kinase_network_maxitems
        keepsinglekinases = input$protein_kinase_network_single_kinases
        show_significant_only = input$protein_kinase_network_significant_only
        footer_txt = "Orange edges indicate that protein is a kinase. Black edges indicate the kinase phosphorylates a site on that protein. "
        return(foKinaseNetworkDraw(ds$ST, ds$KT, ds$Wk2s, 
                                   ds$Wk2os, minzscore, topk, 
                                   keepsinglekinases, "proteins", 
                                   footer_txt, show_significant_only))
    })
    
    output$siteTable <- DT::renderDataTable(server = FALSE, {
        req(site_table_processed())
        ST <- site_table_processed();
        ST$Phos = round(ST$Phos, digits = 3)
        ST$StdErr = round(ST$StdErr, digits = 3)
        ST$ZScore = round(ST$ZScore, digits = 3)
        ST$MagnitudeAdj = round(ST$MagnitudeAdj, digits = 3)
        
        si <- order(abs(ST$ZScore), decreasing = TRUE)
        ST <- ST[si,]
        
        fn = 'site_table'
        DT::datatable(ST, rownames= FALSE, extensions = 'Buttons', 
                      options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                     paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
            formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
    })
    
    output$proteinTable <- DT::renderDataTable(server = FALSE, {
        req(protein_table_processed())
        PT <- protein_table_processed();
        PT$Phos = round(PT$Phos, digits = 3)
        PT$StdErr = round(PT$StdErr, digits = 3)
        PT$ZScore = round(PT$ZScore, digits = 3)
        PT$MagnitudeAdj = round(PT$MagnitudeAdj, digits = 3)
        
        si <- order(abs(PT$ZScore), decreasing = TRUE)
        PT <- PT[si,]
        
        fn = 'site_table'
        DT::datatable(PT, rownames= FALSE, extensions = 'Buttons', 
                      options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                     paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
            formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
    })
    
    output$buttonDownloadSampleData <- downloadHandler(
        filename = function() { paste('sample_data.csv', sep='') },
        content = function(file) {
            write.csv(Tsample, file = file, row.names = FALSE, quote=FALSE)
            #write_csv(file, Tsample), 
        }
    )
    
    output$buttonDownloadSampleMetaData <- downloadHandler(
        filename = function() { paste('sample_metadata.csv', sep='') },
        content = function(file) {
            write.csv(Tsample_metadata, file = file, row.names = FALSE, quote=FALSE)
            #write_csv(file, Tsample), 
        }
    )

})
