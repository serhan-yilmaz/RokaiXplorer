output$modal_kinase_sites_table <- DT::renderDataTable(server = TRUE, {
  req(site_table_processed())
  req(modal_box_selection_mapped())
  ds <- modal_box_selection_mapped()
  validate(need(ds$isKinase & ds$isMapped, ""))
  
  ST <- site_table_processed();
  ST$Phos = round(ST$Phos, digits = 3)
  ST$StdErr = round(ST$StdErr, digits = 3)
  ST$ZScore = round(ST$ZScore, digits = 3)
  ST$MagnitudeAdj = round(ST$MagnitudeAdj, digits = 3)
  
  net <- reactive_network()
  site_indices <- net$Wkin2site[ds$index, ]
  site_identifiers = as.character(net$Site$Identifier[site_indices])
  valid_sites = match(site_identifiers, ST$Identifier)
  valid_sites = valid_sites[!is.na(valid_sites)]
  ST = ST[valid_sites, ]
  
  si <- order(abs(ST$ZScore), decreasing = TRUE)
  ST <- ST[si,]
  
  #  message(class(ST))
  ST = subset(ST, select = -c(Identifier))
  
  callback <- c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[1] != null){ ", 
    " text1 = data[1].concat('-', data[2])", 
    " } else {",
    " text1 = data[0].concat('-', data[2])", 
    " }",
    " Shiny.setInputValue('site_kinase_network_doubleclickb', text1.concat('_Site'), {priority: 'event'});",
    "})"
  )
  
  fn = 'site_table'
  tab <- DT::datatable(ST, rownames= FALSE, extensions = 'Buttons',
                       callback=JS(callback),
                       selection = "single",
                       options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                      #    extensions = "Select", 
                                      #                         callback = JS(callback),
                                      paging = TRUE, searching = TRUE, pageLength = 8, dom = 'frtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
  
  return(tab)
})

output$modal_protein_sites_table <- DT::renderDataTable(server = TRUE, {
  req(site_table_processed())
  req(modal_box_selection_mapped())
  ds <- modal_box_selection_mapped()
  validate(need(ds$isProtein & ds$isMapped, "This protein is not identified in the experiment. "))
  
  ST <- site_table_processed();
  ST$Phos = round(ST$Phos, digits = 3)
  ST$StdErr = round(ST$StdErr, digits = 3)
  ST$ZScore = round(ST$ZScore, digits = 3)
  ST$MagnitudeAdj = round(ST$MagnitudeAdj, digits = 3)
  
  # net <- reactive_network()
  # site_indices <- net$Wkin2site[ds$index, ]
  # site_identifiers = as.character(net$Site$Identifier[site_indices])
  protein_identifier = ds$table$ID
  valid_sites = !is.na(match(ST$Protein, protein_identifier))
  ST = ST[valid_sites, ]
  
  si <- order(abs(ST$ZScore), decreasing = TRUE)
  ST <- ST[si,]
  
  #  message(class(ST))
  ST = subset(ST, select = -c(Identifier))
  
  callback <- c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[1] != null){ ", 
    " text1 = data[1].concat('-', data[2])", 
    " } else {",
    " text1 = data[0].concat('-', data[2])", 
    " }",
    " Shiny.setInputValue('site_kinase_network_doubleclickb', text1.concat('_Site'), {priority: 'event'});",
    "})"
  )
  
  fn = 'site_table'
  tab <- DT::datatable(ST, rownames= FALSE, extensions = 'Buttons',
                       callback=JS(callback),
                       selection = "single",
                       options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                      #    extensions = "Select", 
                                      #                         callback = JS(callback),
                                      paging = TRUE, searching = TRUE, pageLength = 8, dom = 'frtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
  
  return(tab)
})