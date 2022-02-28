### Site/Protein Kinase Networks

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
  #footer_txt = "Orange edges indicate the site is on the kinase."
  footer_txt = "This network is interactive! You can drag & drop nodes to adjust the view and hover to see more information. Double click on a node to inspect it in detail. "
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
  #footer_txt = "Orange edges indicate that protein is a kinase. Black edges indicate the kinase phosphorylates a site on that protein. "
  footer_txt = "This network is interactive! You can drag & drop nodes to adjust the view and hover to see more information. Double click on a node to inspect it in detail. "
  return(foKinaseNetworkDraw(ds$ST, ds$KT, ds$Wk2s, 
                             ds$Wk2os, minzscore, topk, 
                             keepsinglekinases, "proteins", 
                             footer_txt, show_significant_only))
})