
formatNumber <- function(X, digits = 3){
  inner <- paste0("%.", digits, "f");
  out <- sprintf(inner, X);
}

formatNumericVariables <- function(GT, exclude = c("PValue", "FDR")){
  cols <- colnames(GT)
  nCol = ncol(GT)
  
  for(iCol in 1:nCol){
    Q = GT[[iCol]]
    cname <- cols[iCol]
    if(is.numeric(Q) && is.na(match(cname, exclude))){
      GT[[iCol]] <- formatNumber(Q);
    }
  }
  # GT$LogOdds = formatNumber(GT$LogOdds);
  # GT$StdErr = formatNumber(GT$StdErr);
  # GT$EffectiveMag = formatNumber(GT$EffectiveMag);
  return(GT)
}

output$siteEnrichmentTable <- DT::renderDataTable(server = FALSE, {
  req(go_enrichment_table_processed())
  GT <- go_enrichment_table_processed();
  # GT$LogOdds = formatNumber(GT$LogOdds);
  # GT$StdErr = formatNumber(GT$StdErr)
  GT$ZScore = round(GT$ZScore, digits = 3)
  GT$ChiSqr = round(GT$ChiSqr, digits = 3)
  GT$MagnitudeAdj = round(GT$MagnitudeAdj, digits = 3)
  GT$EffectiveMag = pmax(GT$MagnitudeAdj, 0)
  GT$Ratio <- paste0(GT$numSignificant, " / ", GT$numIdentified)
  
  # GT <- GT[!is.na(GT$ZScore),]
  # GT <- GT[!is.na(GT$LogOdds), ]
  GT <- GT[GT$numIdentified >= 1, ]
  GT <- GT[GT$numSignificant >= 1, ]
  
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
  
  GT = subset(GT, select = -c(MagnitudeAdj, numSignificant, numIdentified))
  colnames(GT)[3] <- "Category"
  
  GT <- GT %>% relocate(Ratio, .after = Category)
  
  GT <- formatNumericVariables(GT)
  # browser()
  # GT$Category <- lapply(GT$Category, foGetGoCategoryText)
  
  
  # colnames(GT)[1] <- "UniprotID"
  # colnames(GT)[2] <- "Name"
  
  tooltips <- list(
    "ID" = "ID of the GO term", 
    "Name" = "Name of the GO term", 
    "Category" = "Category of the GO term", 
    "Ratio" = "Number of significant / Number of identified proteins in the gene set", 
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
  
  fn = 'enrichment_table'
  DT::datatable(GT, rownames= FALSE, extensions = 'Buttons', 
                callback = JS(callback), 
                selection = "single",
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               autoWidth = TRUE, 
                               stateSave = TRUE,
                               stateLoadParams = JS('function (settings, data) {return false;}'),
                               columnDefs = list(
                                 list(targets = 1, title = "Name", width = '140px')
                                 # list(targets = 1, title = "Name")
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







