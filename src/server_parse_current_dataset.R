current_dataset <- reactive({
  req(reactive_dataset())
  library(tidyverse)
  Tx <- reactive_dataset()
  Ts <- as.matrix(Tx %>% select(3:ncol(Tx)))
  
  ## Convert to Numeric - Capture warning
  cnames = colnames(Ts)
  numcols = ncol(Ts)
  Ts <- matrix(as.numeric(Ts), ncol = numcols)
  colnames(Ts) <- cnames
  
  ST <- (Tx %>% select(1:2))
  
  # browser()
  
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
  if(nrow(T_metadata) >= 2){
    x$Tsample_metadata <- T_metadata[2:nrow(T_metadata), ]
  } else {
    x$Tsample_metadata <- T_metadata[0, ]
  }
  
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
  
  indices = match(proteins, NetworkData$UniprotGene$ID)
  ds$ST$ProteinName <- NetworkData$UniprotGene$Gene[indices]
  
  # indices = match(proteins, NetworkData$Protein$ID)
  # ds$ST$ProteinName <- NetworkData$Protein$Name[indices]
  
  nameX = ds$ST$ProteinName
  nameX[is.na(nameX)] = ds$ST$Protein[is.na(nameX)]
  ds$ST$Identifier <- str_c(nameX, ds$ST$Position, sep = "-")
  ds$ST$NetworkDataIndex <- match(ds$ST$Identifier, NetworkData$Site$Identifier)
  # browser()
  
  ## Check the sample match
  
  validate(
    need(nnzero(!is.na(indices))>0, "Input mapping failed. Please check if the correct reference proteome is selected.")
  )
  
  return(ds)
})