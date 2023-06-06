### Site/Protein Kinase Networks

site_kinase_network <- reactive({
  req(site_table_processed())
  req(reactive_network())
  ST <- site_table_processed()
  ST$NameX = ST$ProteinName
  ST$NameX[is.na(ST$NameX)] = ST$Protein[is.na(ST$NameX)]
  ST$ID <- str_c(ST$NameX, ST$Position, sep = "-")
  pos = gsub("[^0-9]", "", ST$Position)
  ST$ID2 <- str_c(ST$Protein, pos, sep = "_")
  NetworkData <- reactive_network()
  ST = ST[!is.na(ST$Phos), ]
  indices = match(ST$ID2, NetworkData$Site$Identifier)
  
  # indices = match(ST$ID, NetworkData$Site$Identifier)
  return(foKinaseNetworkSubset(ST, NetworkData, indices, 
                               NetworkData$Wkin2site, 
                               NetworkData$Wkinase2site))
})

siteKinaseNetwork <- reactive({
  ds <- site_kinase_network()
  minzscore = input$site_kinase_network_minzscore
  topk = input$site_kinase_network_maxitems
  keepsinglekinases = input$site_kinase_network_single_kinases
  show_significant_only = input$site_kinase_network_significant_only
  #footer_txt = "Orange edges indicate the site is on the kinase."
  footer_txt = "This network is interactive! You can drag & drop nodes to adjust the view and hover to see more information. Double click on a node to inspect it in detail. "
  return(foKinaseNetworkDraw(ds$ST, ds$KT, ds$Wk2s, 
                             ds$Wk2os, minzscore, topk, 
                             keepsinglekinases, "phosphosites", 
                             footer_txt, show_significant_only))
})

output$site_kinase_network <- renderVisNetwork({
  req(site_kinase_network())
  siteKinaseNetwork()
})

siteKSNetworkDownloadHandler <- function(file_name, file_type){
  downloadHandler(
    filename = function() { paste(file_name, "png", sep='.') },
    content = function(file) {
      h = 4.6
      toastr_info("Preparing the download. Please do not change tabs till the download is ready. This may take a few seconds...")
      
      html_name <- tempfile(fileext = ".html")
      siteKinaseNetwork() %>% 
        visOptions(height = "600px", width = "800px;") %>%
        visSave(html_name)
      webshot(html_name, zoom = 1, file = file, vheight = 800, vwidth = 800, delay = 0.2)
    },
    #contentType = paste("application/", file_type, sep = "")
  )
}

output$site_kinase_network_downloadPlotPNG <- siteKSNetworkDownloadHandler(
  file_name = "site-kinase-network", file_type = "png")

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