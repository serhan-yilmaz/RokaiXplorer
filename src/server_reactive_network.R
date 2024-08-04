reactive_network <- reactive({
  req(initialized())
  switch (network_value(),
          "uniprot.human" = fname <- "rokai_network_data_uniprotkb_human.rds",
          "uniprot.mouse" = fname <- "rokai_network_data_uniprotkb_mouse.rds",
          "uniprot.rat" = fname <- "rokai_network_data_uniprotkb_rat.rds",
          validate(
            need(FALSE, "Invalid network state.")
          )
  )
  switch(input$dataset_version_selection, 
         "Latest" = networkdata_version_folder <- "latest", 
         "2022" = networkdata_version_folder <- "2022", 
         validate(
           need(FALSE, "Invalid network data version")
         )
  )
  
  dataFolder = paste0("data/", networkdata_version_folder, "/")
  
  NetworkData <- readRDS(paste(dataFolder, fname, sep =""));
  proteins = unique(NetworkData$Site$Protein)
  indices = match(proteins, NetworkData$Site$Protein)
  names <- NetworkData$Site$Gene[indices]
  NetworkData$Protein <- data.frame(ID = proteins, Name = names)
  # pos = gsub('\\D+','', NetworkData$Site$Position)
  # NetworkData$Site$Identifier <- str_c(NetworkData$Site$Protein, NetworkData$Site$Position, sep = "-")
  # NetworkData$Site$Identifier <- str_c(NetworkData$Site$Gene, NetworkData$Site$Position, sep = "-")
  
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
  NetworkData$Kinase$Protein <- NetworkData$Protein$Name[indices]
  NetworkData$Kinase$Protein[is.na(indices)] = NetworkData$Kinase$Gene[is.na(indices)]
  
  indices = indices[!is.na(indices)]
  NetworkData$Wkinase2protein <- sparseMatrix(
    i = rowindices,
    j = indices,
    x = T,
    dims = c(nrow(NetworkData$Kinase), nrow(NetworkData$Protein))
  )
  NetworkData$Wkinase2site <- as(NetworkData$Wkinase2protein %*% t(NetworkData$Wsite2protein), "lgCMatrix")
  
  NetworkData$Wkin2protein <- as(NetworkData$Wkin2site %*% (NetworkData$Wsite2protein), "lgCMatrix")
  NetworkData$description <- list(
    "Wkin2site" = "Kinase-Substrate network at phosphosite level",
    "Wkin2protein" = "Kinase-Substrate network at protein level",
    "Wkinase2protein" = "Kinase -> Protein mapping (protein ids of kinases)",
    "Wkinase2site" = "Phosphosites that are on the kinase itself"
  )
  
  unmatched_proteins <- is.na(match(NetworkData$Protein$ID, NetworkData$UniprotGene$ID))
  Prot <- NetworkData$Protein[unmatched_proteins, ]
  colnames(Prot)[2] <- "Gene"
  rownames(Prot) <- NULL
  Prot <- FromUniprot <- FALSE
  NetworkData$UniprotGene$FromUniprot <- TRUE
  NetworkData$UniprotGene <- rbind(NetworkData$UniprotGene, Prot)

  GO_ENABLED = TRUE
  if(GO_ENABLED){
    switch (network_value(),
            "uniprot.human" = species <- "human",
            "uniprot.mouse" = species <- "mouse",
            "uniprot.rat" = species <- "rat",
            validate(
              need(FALSE, "Invalid network state.")
            )
    )
    GOData <- readRDS(paste0(dataFolder, "go_data.rds"));
    NetworkData$GO = GOData$go_ontology
    a_name <- paste("go_annotations", species, sep = "_")
    NetworkData$Gene2GO = GOData[[a_name]]
    go_indices = match(NetworkData$Gene2GO$GOTerm, NetworkData$GO$ID)
    gene_indices = match(NetworkData$Gene2GO$Gene_Name, NetworkData$UniprotGene$Gene)
    valids = !is.na(go_indices) & !is.na(gene_indices)
    go_indices = go_indices[valids]
    gene_indices = gene_indices[valids]
    NetworkData$Wuniprotgene2goterm <- sparseMatrix(
      i = gene_indices,
      j = go_indices,
      x = T,
      dims = c(nrow(NetworkData$UniprotGene), nrow(NetworkData$GO))
    )
  }
  NetworkData$net$version.go = GOData$date
  if(is.null(NetworkData$net$version.go)){
    NetworkData$net$version.go = '2023-01-19';
  }
  foUpdateVersions(NetworkData)
  
  return (NetworkData)
})

dataset_version_text <- function(dataset, date, link){
  id = paste(dataset, "txt", sep = "_");
  dataset <- paste(dataset, ":", sep = "")
  tags$tr(
    tags$td(tags$li(tags$a(dataset, href = link))), 
    tags$td(tags$text(date, id = id))
  )
}

output$dataset_versions_section <- renderUI({
  req(reactive_network())
  NetworkData <- reactive_network()
  tags$table(
    style="font-size: 16px; width: 100%; margin-left: 22px;",
    dataset_version_text("Uniprot", NetworkData$net$version.uniprot[1], "https://www.uniprot.org/"),
    dataset_version_text("PhosphoSitePlus", NetworkData$net$version.psp[1], "https://www.phosphosite.org/"),
    dataset_version_text("Signor", NetworkData$net$version.signor[1], "https://signor.uniroma2.it/"),
    dataset_version_text("STRING", NetworkData$net$version.string[1], "https://string-db.org/"),
    dataset_version_text("PTMcode", NetworkData$net$version.ptmcode[1], "https://ptmcode.embl.de/"),
    dataset_version_text("DEPOD", NetworkData$net$version.depod[1], "http://www.depod.org/"),
    dataset_version_text("GO", NetworkData$net$version.go[1], "http://geneontology.org/"),
  )
})

foUpdateVersions <- function(NetworkData){
  runjs(paste("document.getElementById('Uniprot_txt').innerHTML = '", NetworkData$net$version.uniprot[1], "';", sep = ""))
  runjs(paste("document.getElementById('PhosphoSitePlus_txt').innerHTML = '", NetworkData$net$version.psp[1], "';", sep = ""))
  runjs(paste("document.getElementById('Signor_txt').innerHTML = '", NetworkData$net$version.signor[1], "';", sep = ""))
  runjs(paste("document.getElementById('STRING_txt').innerHTML = '", NetworkData$net$version.string[1], "';", sep = ""))
  runjs(paste("document.getElementById('PTMcode_txt').innerHTML = '", NetworkData$net$version.ptmcode[1], "';", sep = ""))
  runjs(paste("document.getElementById('DEPOD_txt').innerHTML = '", NetworkData$net$version.depod[1], "';", sep = ""))
  runjs(paste("document.getElementById('GO_txt').innerHTML = '", NetworkData$net$version.go[1], "';", sep = ""))
}
