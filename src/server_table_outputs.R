output$siteTable <- DT::renderDataTable(server = FALSE, {
  req(site_table_processed())
  ST <- site_table_processed();
  ST$Phos = round(ST$Phos, digits = 3)
  ST$StdErr = round(ST$StdErr, digits = 3)
  ST$ZScore = round(ST$ZScore, digits = 3)
  ST$MagnitudeAdj = round(ST$MagnitudeAdj, digits = 3)
  
  si <- order(abs(ST$ZScore), decreasing = TRUE)
  ST <- ST[si,]
  
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
    " Shiny.setInputValue('site_kinase_network_doubleclick', text1.concat('_Site'), {priority: 'event'});",
    "})"
  )
  
  fn = 'site_table'
  tab <- DT::datatable(ST, rownames= FALSE, extensions = 'Buttons',
                       callback=JS(callback),
                       selection = "single",
                       options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                      #    extensions = "Select", 
                                      #                         callback = JS(callback),
                                      paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
  
  return(tab)
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
  
  PT = subset(PT, select = -c(Identifier))
  
  callback <- c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " Shiny.setInputValue('site_kinase_network_doubleclick', data[1].concat('_Protein'), {priority: 'event'});",
    "})"
  )
  
  fn = 'site_table'
  DT::datatable(PT, rownames= FALSE, extensions = 'Buttons', 
                callback = JS(callback), 
                selection = "single",
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
})


