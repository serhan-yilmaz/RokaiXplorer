
# reactive_enrichment_table_categories <- reactive({
#   return(foEnrichmentCategories(input$enrichment_table_categories))
# })

output$enrichment_display_numpathways <- renderUI({
  req(go_enrichment_table_processed())
  GT <- go_enrichment_table_processed();
  nValid = nnzero(GT$numIdentified >= 1)
  
  return(paste0(nValid, ' included in the analysis'))
})

output$enrichment_display_numproteins <- renderUI({
  req(enrichment_background_protein_set())
  out <- enrichment_background_protein_set();
  nSig = nnzero(out$proteinIsSignificant)
  nTotal = nnzero(out$proteinIsIdentified)
  
  return(paste0(nSig, '/', nTotal, ' passed the cutoffs'))
})

output$siteEnrichmentTable <- DT::renderDataTable(server = FALSE, {
  req(go_enrichment_table_processed_withtargets())
  GT <- go_enrichment_table_processed_withtargets();
  # GT$LogOdds = formatNumber(GT$LogOdds);
  # GT$StdErr = formatNumber(GT$StdErr)
  GT$ZScore = round(GT$ZScore, digits = 3)
  GT$ChiSqr = round(GT$ChiSqr, digits = 3)
  GT$MagnitudeAdj = round(GT$MagnitudeAdj, digits = 3)
  GT$EffectiveMag = pmax(GT$MagnitudeAdj, 0)
  
  # GT <- GT[!is.na(GT$ZScore),]
  # GT <- GT[!is.na(GT$LogOdds), ]
  GT <- GT[GT$numIdentified >= 1, ]
  GT <- GT[GT$numSignificant >= input$enrichment_minhits, ]
  
  # minsubs = input$kinase_table_minsubs
  # if(is.null(minsubs)){
  #   minsubs = 1
  # }
  # KT <- KT[KT$NumSubs >= minsubs,]
  
  si <- order(GT$LogRiskRatio, decreasing = TRUE)
  # si <- order(GT$FDR, decreasing = FALSE)
  GT <- GT[si,]
  si <- order(GT$EffectiveMag, decreasing = TRUE)
  GT <- GT[si,]
  
  si <- order(GT$isSignificant, decreasing = TRUE)
  GT <- GT[si,]
  
  GT = subset(GT, select = -c(MagnitudeAdj, numSignificant, numIdentified, numProtein, Index))
  colnames(GT)[3] <- "Category"
  
  cats <- foEnrichmentCategories(input$enrichment_table_categories)
  # cats <- reactive_enrichment_table_categories()
  valids = !is.na(match(GT$Category, cats))
  GT <- GT[valids, ]
  
  if(input$enrichment_table_significantonly == TRUE){
    GT = GT[GT$isSignificant, ]
  }
  
  GT <- GT %>% relocate(HitRatio, .after = Category)
  GT <- GT %>% relocate(ObsRatio, .after = HitRatio)
  
  GT <- formatNumericVariables(GT)
  # browser()
  # GT$Category <- lapply(GT$Category, foGetGoCategoryText)
  
  
  # colnames(GT)[1] <- "UniprotID"
  # colnames(GT)[2] <- "Name"
  
  tooltips <- list(
    "ID" = "ID of the GO term", 
    "Name" = "Name of the GO term", 
    "Category" = "Category of the GO term", 
    "HitRatio" = "Number of hits / Number of identified proteins in the gene set", 
    "ObsRatio" = "Number of identified / Number of total proteins in the gene set", 
    "LogOdds" = "Log2 of odds ratio", 
    "LogRiskRatio" = "Log2 of risk ratio obtained by Bayesian estimation", 
    "StdErr" = "Standard error for the log risk ratio",
    "ZScore" = "Standardized score for enrichment based on log risk ratio",
    "ChiSqr" = "Chi-squared statistic",
    "PValue" = "P-value",
    "FDR" = "False discovery rate",
    "EffectiveMag" = "Reliable portion of the enrichment score beyond 2 standard errors",
    "isSignificant" = "Is the enrichment significant", 
    # "EffectiveMag" = "log2FC - 3*StdErr",
    "dummy" = ""
  )
  
  callback <- c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[0] != null){ ", 
    " text1 = data[0]", 
    " } else {",
    " text1 = data[0]", 
    " }",
    " Shiny.setInputValue('site_kinase_network_doubleclick', text1.concat('_GOTerm'), {priority: 'event'});",
    "})"
  )
  
  # js_adjust_after_initialize <- JS('function() {this.api().columns().adjust().draw();}');
  
  hitsIndex = match('Hits', colnames(GT)) - 1
  allTargetsIndex = match('AllTargets', colnames(GT)) - 1
  
  fn = 'enrichment_table'
  DT::datatable(GT, rownames= FALSE, extensions = 'Buttons', 
                callback = JS(callback), 
                selection = "single",
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               autoWidth = TRUE, 
                               stateSave = TRUE,
                               stateLoadParams = JS('function (settings, data) {return false;}'),
                               columnDefs = list(
                                 list(targets = 1, title = "Name", width = '150px'),
                                 list(targets = hitsIndex, width = '300px'),
                                 list(targets = allTargetsIndex, visible = FALSE, width = '450px')
                               ),
                               # initComplete = js_adjust_after_initialize, 
                               initComplete = foAddTooltips(colnames(GT), tooltips),
                               columns = foRestoreStateIfAvailable("siteEnrichmentTable"),
                               paging = TRUE, searching = TRUE, pageLength = 7, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn), "colvis"))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
})

# observeEvent(output[["siteEnrichmentTable"]], {
#   js_adjust <- "var table = $('#siteEnrichmentTable').DataTable(); table.columns.adjust().draw();";
#   delay(100, shinyjs::runjs(js_adjust))
# })







