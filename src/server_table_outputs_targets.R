
output$kinaseTargetsTable <- DT::renderDataTable(server = FALSE, {
  req(kinase_targets_table())
  KS <- kinase_targets_table();
  # colnames(KS)[colnames(KS) == 'KinID'] <- 'UniprotID'
  
  KS$Phos = round(KS$Phos, digits = 3)
  KS$ZScore = round(KS$ZScore, digits = 3)
  
  si <- order(abs(KS$ZScore), decreasing = TRUE)
  KS <- KS[si,]
  
  callback_id = 'site_kinase_network_doubleclick'
  callback <- gsub('###CALLBACK_ID###', callback_id, c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[5] != null){ ", 
    " text1 = data[4].concat('-', data[5])", 
    " } else {",
    " text1 = data[1].concat('-', data[2])", 
    " }",
    " Shiny.setInputValue('###CALLBACK_ID###', text1.concat('_Phosphosite'), {priority: 'event'});",
    "})"
  ))
  
  tooltips <- list(
    "UniprotID" = "Uniprot ID of the kinase", 
    "Name" = "Name of the kinase", 
    "Gene" = "Gene symbol of the kinase", 
    "SubsID" = "Uniprot ID of the substrate", 
    "SubsProtein" = "Name of the substrate", 
    "Position" = "Position of the phosphosite on the protein",
    "DataSource" = "Shows where the information about the displayed interaction is obtained from",
    "Phos" = "Phosphorylation as log2 fold change", 
    "ZScore" = "Standardized log fold changes",
    "PValue" = "P-value",
    "FDR" = "False discovery rate",
    "EffectiveMag" = "Reliable portion of the log2 fold changes beyond 3 standard errors",
    "dummy" = ""
  )
  
  fn = 'kinase_targets'
  DT::datatable(KS, rownames= FALSE, extensions = 'Buttons', 
                callback=JS(callback),
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               initComplete = foAddTooltips(colnames(KS), tooltips),
                               paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
})



output$pathwayTargetsTable <- DT::renderDataTable(server = FALSE, {
  req(pathway_targets_table())
  KS <- pathway_targets_table();
  
  KS$ZScore = round(KS$ZScore, digits = 3)
  KS$HitRatio <- paste0(KS$numSignificant, " / ", KS$numIdentified)
  KS <- KS %>% relocate(HitRatio, .after = Category)
  
  
  KS <- KS[KS$numIdentified >= 1, ]
  KS <- KS[KS$numSignificant >= input$enrichment_targets_minhits, ]
  
  cats <- foEnrichmentCategories(input$enrichment_targets_table_categories)
  valids = !is.na(match(KS$Category, cats))
  KS <- KS[valids, ]
  
  if(input$enrichment_targets_table_significantonly == TRUE){
    KS = KS[KS$isSignificant, ]
  }
  
  if(input$enrichment_targets_table_hitsonly == TRUE){
    KS = KS[KS$IsHit, ]
  }
  
  si <- order(KS$IsHit, decreasing = TRUE)
  KS <- KS[si,]
  
  # si <- order(KS$ID, decreasing = FALSE)
  # KS <- KS[si,]
  
  si <- order(KS$LogRiskRatio, decreasing = TRUE)
  # si <- order(GT$FDR, decreasing = FALSE)
  KS <- KS[si,]
  
  si <- order(KS$EffectiveMag, decreasing = TRUE)
  KS <- KS[si,]
  
  si <- order(KS$isSignificant, decreasing = TRUE)
  KS <- KS[si,]
  
  # si <- order(KS$FDR, decreasing = FALSE)
  # KS <- KS[si,]
  
  if(nrow(KS) >= 2000){
    KS = KS[1:2000, ]
  }
  
  KS = subset(KS, select = -c(numSignificant, numIdentified, numProtein, ZScore, isSignificant, EffectiveMag, LogRiskRatio))
  
  callback_id = 'site_kinase_network_doubleclick'
  callback <- gsub('###CALLBACK_ID###', callback_id, c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;",
    " if(data[6] != null){ ",
    " text1 = data[6]", 
    " } else {",
    " text1 = data[0]",
    " }",
    " Shiny.setInputValue('###CALLBACK_ID###', text1.concat('_Protein'), {priority: 'event'});",
    "})"
  ))
  
  tooltips <- list(
    "ID" = "ID of the GO term", 
    "Name" = "Name of the GO term", 
    "Category" = "Category of the GO term", 
    "HitRatio" = "Number of hits / Number of identified proteins in the gene set", 
    "TargetID" = "Uniprot ID of the target protein (in the gene set)", 
    "TargetProtein" = "Name of the target protein",
    "IsHit" = "True or False, displays whether the protein in question is in the background set used for enrichment (i.e., passed the screening cutoffs)",
    "Phos" = "Phosphorylation as log2 fold change", 
    "ZScore" = "Standardized log fold changes",
    "PValue" = "P-value",
    "FDR" = "False discovery rate",
    "EffectiveMag" = "Reliable portion of the log2 fold changes beyond 3 standard errors",
    "dummy" = ""
  )
  
  fn = 'enrichment_targets'
  DT::datatable(KS, rownames= FALSE, extensions = 'Buttons', 
                callback=JS(callback),
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               autoWidth = TRUE, 
                               initComplete = foAddTooltips(colnames(KS), tooltips),
                               columnDefs = list(
                                 list(targets = 1, title = "Name", width = '200px')
                                 # list(targets = 1, title = "Name")
                               ),
                               paging = TRUE, searching = TRUE, pageLength = 7, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>%
    formatSignif('FDR', 3) 
})







