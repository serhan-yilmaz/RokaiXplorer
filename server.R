
library(Matrix)
library(shiny)
library(DT)
library(ggplot2)
library(cicerone)
library(shinyjs)
library(shinytoastr)
library(shinylogs)

library(ids)

source("compute_pvalues.R")

## Update Sample Data
folder = "data/"
Tsample <- read.csv(paste(folder, "combined_alz_explorer_data.csv", sep=""))
Tsample_metadata <- read.csv(paste(folder, "combined_alz_explorer_metadata.csv", sep=""))

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
        # indices = match(NetworkData$Site$Protein, proteins)
        # NetworkData$Wsite2protein <- sparseMatrix(
        #     i = 1:nrow(NetworkData$Site),
        #     j = indices, 
        #     x = 1,
        #     dims = c(nrow(NetworkData$Site), nrow(NetworkData$Protein))
        # )
        
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
        
        tags$div(
            style = "margin-top: 8px; ", 
            tags$b("Select Subgroup: "), 
        tags$div(
            style = "padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
            lapply(1:nrow(x$Tsample_metadata), 
                   function(i) {foSubgoupSelectInput(i, x$Tsample_metadata)})
        )
        )
    })
    
    output$group_difference_controls <- renderUI({
        validate(
            need(metadata_ready(), "")
        )
        x <- current_metadata()
        groups <- rownames(x$Tsample_metadata)
        tags$div(
            style = "margin-top: 8px; ", 
            tags$b("Investigate Group Differences: "), 
            tags$div(
                style = "padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
                selectInput("select_group_differences" , "Select Group:", 
                            choices = c("None", groups), 
                            selected = 1, selectize = F, width = 170),
                uiOutput("select_subgroups_for_difference"),
            )
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
                SEcaseA <- ScaseA / sqrt(NcaseA)
                ScontrolA <- apply(TcontrolA, 1, function(x) sd(x, na.rm=T))
                SEcontrolA <- ScontrolA / sqrt(NcontrolA)
                SE_A <- sqrt(SEcaseA^2 + SEcontrolA^2)
                validsA <- (NcaseA >= 2) & (NcontrolA >= 2)
            } else {
                SE_A <- rep(sd(Q_A, na.rm = T), length(Q_A))
                validsA <- (NcaseA >= 1) & (NcontrolA >= 1)
            }
            
            if((nCaseB >= 2) & (nControlB >= 2)){
                ScaseB <- apply(TcaseB, 1, function(x) sd(x, na.rm=T))
                SEcaseB <- ScaseB / sqrt(NcaseB)
                ScontrolB <- apply(TcontrolB, 1, function(x) sd(x, na.rm=T))
                SEcontrolB <- ScontrolB / sqrt(NcontrolB)
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
                SEcase <- Scase / sqrt(Ncase)
                Scontrol <- apply(Tcontrol, 1, function(x) sd(x, na.rm=T))
                SEcontrol <- Scontrol / sqrt(Ncontrol)
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
    
    
    preprocessed_dataset <- reactive({
        req(processed_dataset())
        ds <- processed_dataset()
        ds$Xv = ds$Xv - mean(ds$Xv, na.rm = T)
        return (ds)
    })
    
    
    site_table <- reactive({
        req(preprocessed_dataset())
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
        
        validate(
            need(nrow(ST) > 0, "There are no sites identified in the selected subgroup. Please make sure there are no conflicts in the subgroup selection.")
        )
        
        return (ST)
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
        
        PT = Protein[valids, ]
        PT$Phos = A[valids]
        PT$StdErr = SE[valids]
        PT$ZScore = Z[valids]
        res = compute_pvalues(as.matrix(PT$ZScore))
        PT$PValue = res$PValues
        PT$FDR = res$QValues
        
        return (PT)
    })
    
    output$sitelevel_volcano <- renderPlot({
        req(site_table())
        ST <- site_table()
        ST$logP = pmin(-log10(ST$PValue), 10)
        ST$isSignificant= (ST$FDR <= 0.1) & (abs(ST$Phos) >= log2(1.25))
        
        thr = min(ST$logP[ST$isSignificant]);
        xMax = round(max(abs(ST$Phos)), digits = 0)
        
        defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
        
       # clr <- c('#0072BD','#E69F00')
        clr <- defaultcolors[1:2]
        ggplot(ST, aes(x=Phos, y=logP, color = isSignificant)) + 
            theme_bw() +
            geom_point(size=3) + 
            geom_hline(yintercept=(thr), linetype="dashed", color = '#555555') + 
            geom_vline(xintercept=c(-log2(1.25), log2(1.25)), linetype="dashed", color = '#333333') + 
            
            theme(text = element_text(size = 16)) + 
            labs(x = "Log2-FC", y = "-log10(P-Value)") + 
            scale_color_manual(name = "", values=clr, labels = c("Not Significant", "Significant")) +
            ylim(0, 10) + 
            xlim(-xMax, xMax)
            #+ theme(legend.position="top") 
    })
    
    output$proteinlevel_volcano <- renderPlot({
        req(protein_table())
        PT <- protein_table()
        PT$logP = pmin(-log10(PT$PValue), 10)
        PT$isSignificant = (PT$FDR <= 0.1) & (abs(PT$Phos) >= log2(1.25))
        
        thr = min(PT$logP[PT$isSignificant]);
        xMax = round(max(abs(PT$Phos)), digits = 0)
        
        defaultcolors <- c('#0072BD', '#D95319', '#EDB120', '#77AC30', '#4DBEEE')
        
        # clr <- c('#0072BD','#E69F00')
        clr <- defaultcolors[1:2]
        ggplot(PT, aes(x=Phos, y=logP, color = isSignificant)) + 
            theme_bw() +
            geom_point(size=3) + 
            geom_hline(yintercept=(thr), linetype="dashed", color = '#555555') + 
            geom_vline(xintercept=c(-log2(1.25), log2(1.25)), linetype="dashed", color = '#333333') + 
            
            theme(text = element_text(size = 16)) + 
            labs(x = "Log2-FC", y = "-log10(P-Value)") + 
            scale_color_manual(name = "", values=clr, labels = c("Not Significant", "Significant")) +
            ylim(0, 10) + 
            xlim(-xMax, xMax)
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
    
    
    output$siteTable <- DT::renderDataTable(server = FALSE, {
        req(site_table())
        ST <- site_table();
        ST$Phos = round(ST$Phos, digits = 3)
        ST$StdErr = round(ST$StdErr, digits = 3)
        ST$ZScore = round(ST$ZScore, digits = 3)
        
        si <- order(abs(ST$ZScore), decreasing = TRUE)
        ST <- ST[si,]
        
        fn = 'site_table'
        DT::datatable(ST, rownames= FALSE, extensions = 'Buttons', 
                      options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                     paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
            formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
    })
    
    output$proteinTable <- DT::renderDataTable(server = FALSE, {
        req(protein_table())
        PT <- protein_table();
        PT$Phos = round(PT$Phos, digits = 3)
        PT$StdErr = round(PT$StdErr, digits = 3)
        PT$ZScore = round(PT$ZScore, digits = 3)
        
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
