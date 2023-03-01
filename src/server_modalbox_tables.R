output$modal_kinase_sites_table <- DT::renderDataTable(server = FALSE, {
  req(site_table_processed())
  req(modal_box_selection_mapped())
  ds <- modal_box_selection_mapped()
  validate(need(ds$isKinase & ds$isMapped, ""))
  
  # browser()
  
  ST <- site_table_processed();
  net <- reactive_network()
  site_indices <- net$Wkin2site[ds$index, ]
  site_identifiers = as.character(net$Site$Identifier[site_indices])
  valid_sites = match(site_identifiers, ST$ID)
  valid_sites = valid_sites[!is.na(valid_sites)]
  ST = ST[valid_sites, ]
  
  tab <- createSiteDataTable(ST, maintable = F)
  
  # ST$Phos = round(ST$Phos, digits = 3)
  # ST$StdErr = round(ST$StdErr, digits = 3)
  # ST$ZScore = round(ST$ZScore, digits = 3)
  # ST$MagnitudeAdj = round(ST$MagnitudeAdj, digits = 3)
  # 
  # si <- order(abs(ST$ZScore), decreasing = TRUE)
  # ST <- ST[si,]
  # 
  # ST = subset(ST, select = -c(Identifier))
  # 
  # callback <- c(
  #   "table.on('dblclick','tr', function() {",
  #   " var data=table.row(this).data(); ",
  #   " let text1;", 
  #   " if(data[1] != null){ ", 
  #   " text1 = data[1].concat('-', data[2])", 
  #   " } else {",
  #   " text1 = data[0].concat('-', data[2])", 
  #   " }",
  #   " Shiny.setInputValue('site_kinase_network_doubleclickb', text1.concat('_Phosphosite'), {priority: 'event'});",
  #   "})"
  # )
  # 
  # fn = 'site_table'
  # tab <- DT::datatable(ST, rownames= FALSE, extensions = 'Buttons',
  #                      callback=JS(callback),
  #                      selection = "single",
  #                      options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
  #                                     initComplete = foAddTooltips(colnames(ST), siteTable_tooltips),
  #                                     #    extensions = "Select", 
  #                                     #                         callback = JS(callback),
  #                                     paging = TRUE, searching = TRUE, pageLength = 8, dom = 'frtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
  #   formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
  
  return(tab)
})

output$modal_protein_sites_table <- DT::renderDataTable(server = FALSE, {
  req(site_table_processed())
  req(modal_box_selection_mapped())
  ds <- modal_box_selection_mapped()
  validate(need(ds$isProtein & ds$isMapped, "This protein is not identified in the experiment. "))
  
  ST <- site_table_processed();
  protein_identifier = ds$table$ID
  valid_sites = !is.na(match(ST$Protein, protein_identifier))
  ST = ST[valid_sites, ]
  tab <- createSiteDataTable(ST, maintable = F)
  
  # ST$Phos = round(ST$Phos, digits = 3)
  # ST$StdErr = round(ST$StdErr, digits = 3)
  # ST$ZScore = round(ST$ZScore, digits = 3)
  # ST$MagnitudeAdj = round(ST$MagnitudeAdj, digits = 3)
  
  # net <- reactive_network()
  # site_indices <- net$Wkin2site[ds$index, ]
  # site_identifiers = as.character(net$Site$Identifier[site_indices])

  
  # si <- order(abs(ST$ZScore), decreasing = TRUE)
  # ST <- ST[si,]
  # 
  # ST = subset(ST, select = -c(Identifier))
  # 
  # callback <- c(
  #   "table.on('dblclick','tr', function() {",
  #   " var data=table.row(this).data(); ",
  #   " let text1;", 
  #   " if(data[1] != null){ ", 
  #   " text1 = data[1].concat('-', data[2])", 
  #   " } else {",
  #   " text1 = data[0].concat('-', data[2])", 
  #   " }",
  #   " Shiny.setInputValue('site_kinase_network_doubleclickb', text1.concat('_Phosphosite'), {priority: 'event'});",
  #   "})"
  # )
  # 
  # fn = 'site_table'
  # tab <- DT::datatable(ST, rownames= FALSE, extensions = 'Buttons',
  #                      callback=JS(callback),
  #                      selection = "single",
  #                      options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
  #                                     initComplete = foAddTooltips(colnames(ST), siteTable_tooltips),
  #                                     #    extensions = "Select", 
  #                                     #                         callback = JS(callback),
  #                                     paging = TRUE, searching = TRUE, pageLength = 8, dom = 'frtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
  #   formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
  
  return(tab)
})

output$modal_protein_goterms_table <- DT::renderDataTable(server = FALSE, {
  req(modal_box_selection_mapped())
  req(reactive_network())
  ds <- modal_box_selection_mapped()
  validate(need(ds$isProtein, ""))
  
  protein_gene_identifier = ds$identifier
  
  NetworkData <- reactive_network()
  
  valids = 	which(!is.na(match(NetworkData$Gene2GO$Gene_Name, protein_gene_identifier)))
  
  GO_Table = NetworkData$Gene2GO[valids, ]
  cat = GO_Table$Category
  GO_Table$Category[cat == 'c'] <- 'cellular_component'
  GO_Table$Category[cat == 'b'] <- 'biological_process'
  GO_Table$Category[cat == 'm'] <- 'molecular_function'
  # GO_Table$Category[cat == 'e'] <- 'external'
  colnames(GO_Table)[1] <- "Gene"
  
  indices = match(GO_Table$GOTerm, NetworkData$GO$ID)
  GO_Table$Name = tools::toTitleCase(NetworkData$GO$Name[indices])
  # GO_Table$Definition = strtrim(NetworkData$GO$Definition[indices], 25)
  # relation = GO_Table$Relation
  GO_Table$Relation = strtrim(GO_Table$Relation, 16)
  
  
  # browser()
  
  # PT = subset(PT, select = -c(Identifier, MagnitudeAdj, KinaseIndex))
  
  tooltips <- list(
    "ID" = "Uniprot ID of the protein", 
    "Name" = "Name of the protein", 
    # "InRef" = "Shows whether the phosphosite exists in the reference proteome",
    "Phos" = "Phosphorylation as log2 fold change", 
    "StdErr" = "Standard error for log2 fold change",
    "ZScore" = "Standardized log fold changes",
    "PValue" = "P-value",
    "FDR" = "False discovery rate",
    "EffectiveMag" = "Reliable portion of the log2 fold changes beyond 3 standard errors",
    "isSignificant" = "Is the phosphorylation significant", 
    # "EffectiveMag" = "log2FC - 3*StdErr",
    "dummy" = ""
  )
  
  callback <- c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[2] != null){ ", 
    " text1 = data[2]", 
    " } else {",
    " text1 = data[0]", 
    " }",
    " Shiny.setInputValue('site_kinase_network_doubleclickb', text1.concat('_GOTerm'), {priority: 'event'});",
    "})"
  )
  
  fn = 'protein_table'
  DT::datatable(GO_Table, rownames= FALSE, extensions = 'Buttons', 
                callback = JS(callback), 
                selection = "single",
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               columnDefs = list(
                                 list(targets = c(0), visible = FALSE)
                               ),
                               # initComplete = foAddTooltips(colnames(PT), tooltips),
                               paging = TRUE, searching = TRUE, pageLength = 7, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn), "colvis")))
})



output$modal_goterm_related_proteins_table <- DT::renderDataTable(server = FALSE, {
  req(modal_box_selection_mapped())
  req(reactive_network())
  ds <- modal_box_selection_mapped()
  validate(need(ds$isGOTerm, ""))
  
  NetworkData <- reactive_network()
  go_identifier = ds$identifier
  valids = 	which(!is.na(match(NetworkData$Gene2GO$GOTerm, go_identifier)))
  
  GO_Table = NetworkData$Gene2GO[valids, ]
  
  req(protein_table_processed())
  PT <- protein_table_processed();
  PT$Phos = round(PT$Phos, digits = 3)
  PT$StdErr = round(PT$StdErr, digits = 3)
  PT$ZScore = round(PT$ZScore, digits = 3)
  PT$MagnitudeAdj = round(PT$MagnitudeAdj, digits = 3)
  PT$EffectiveMag = pmax(PT$MagnitudeAdj, 0)
  
  indices = match(GO_Table$Gene_Name, PT$Name)
  GO_Table = GO_Table[!is.na(indices), ]
  indices = indices[!is.na(indices)]
  PT = PT[indices, ]
  
  si <- order(abs(PT$ZScore), decreasing = TRUE)
  PT <- PT[si,]
  si <- order(abs(PT$EffectiveMag), decreasing = TRUE)
  PT <- PT[si,]
  
  
  PT = subset(PT, select = -c(Identifier, MagnitudeAdj, KinaseIndex))
  
  PT$Relation = strtrim(GO_Table$Relation, 16)
  
  library(dplyr)
  PT <- PT %>% relocate(Relation, .before = Phos)
  
  tooltips <- foProteinTableTooltips()
  
  callback <- c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[1] != null){ ", 
    " text1 = data[1]", 
    " } else {",
    " text1 = data[0]", 
    " }",
    " Shiny.setInputValue('site_kinase_network_doubleclickb', text1.concat('_Protein'), {priority: 'event'});",
    "})"
  )
  
  fn = 'protein_table'
  DT::datatable(PT, rownames= FALSE, extensions = 'Buttons', 
                callback = JS(callback), 
                selection = "single",
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               initComplete = foAddTooltips(colnames(PT), tooltips),
                               paging = TRUE, searching = TRUE, pageLength = 7, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn), "colvis"))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
})
