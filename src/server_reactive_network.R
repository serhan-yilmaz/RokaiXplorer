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
  
  return (NetworkData)
})