foRestoreStateIfAvailable <- function(id){
  state_id = paste(id, "state", sep = "_")
  state <- isolate(input[[state_id]]$columns)
}

foAddTooltips <- function(colnames=c(), tooltips = list()){
  if(length(colnames) == 0){
    return(NULL);
  }
  part1 <- 'function() {this.api().columns().header().to$().each(function() {';
  
  part3 <- '})}';
  
  # part_default = '$(this).attr("title", "title for " + $(this).text());';
  part_default = '';
  
  # template <- '$(this).attr("title", ###TITLE###);';
  template <- '$(this).attr("title", "###TOOLTIP###");';
  
  part2 = 'switch($(this).text()){';
  for(iCol in 1:length(colnames)){
    colname = colnames[[iCol]];
    tooltip = tooltips[[colname]]
    if(!is.null(tooltip) && !is.na(tooltip)){
      part2 = paste(part2, 'case', paste0('"', colname, '":'), sep = " ");
      txt <- gsub("###TOOLTIP###", tooltip, template)
      part2 = paste(part2, txt, sep = " ");
      part2 = paste(part2, 'break;', sep = " ");
    }
  }
  part2 = paste(part2, '}', sep = " ");
  
  JS(paste(part1, part_default, part2, part3, sep = " "));
}

foHideColumns = function(ST, cols){
  not_visible_targets = match(cols, colnames(ST))
  not_visible_targets = not_visible_targets[!is.na(not_visible_targets)]
  not_visible_targets = not_visible_targets - 1
  columnDefs = list(
    list(targets = not_visible_targets, visible = FALSE), 
    list(targets = '_all', visible = TRUE)
  )
}



createSiteDataTable <- function(ST, maintable = T){
  ST$Phos = round(ST$Phos, digits = 3)
  ST$StdErr = round(ST$StdErr, digits = 3)
  ST$ZScore = round(ST$ZScore, digits = 3)
  ST$MagnitudeAdj = round(ST$MagnitudeAdj, digits = 3)
  ST$EffectiveMag = pmax(ST$MagnitudeAdj, 0)
  # ST$InRef = ifelse(ST$InRef, "True", "False")
  
  si <- order(abs(ST$ZScore), decreasing = TRUE)
  ST <- ST[si,]
  si <- order(abs(ST$EffectiveMag), decreasing = TRUE)
  ST <- ST[si,]
  
  ST = subset(ST, select = -c(Identifier, MagnitudeAdj, NetworkDataIndex, ID))
  
  ST <- formatNumericVariables(ST)
  
  if(maintable == TRUE){
    stateSave = TRUE
    stateLoadParams = JS('function (settings, data) {return false;}')
    columns = foRestoreStateIfAvailable("siteTable")
    pageLength = 10
    dom = 'Bfrtip'
    callback_id = 'site_kinase_network_doubleclick'
  } else {
    stateSave = FALSE
    stateLoadParams = NULL
    columns = NULL
    pageLength = 8
    dom = 'frtip'
    callback_id = 'site_kinase_network_doubleclickb'
  }
  
  callback <- gsub('###CALLBACK_ID###', callback_id, c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[1] != null){ ", 
    " text1 = data[1].concat('-', data[2])", 
    " } else {",
    " text1 = data[0].concat('-', data[2])", 
    " }",
    " Shiny.setInputValue('###CALLBACK_ID###', text1.concat('_Phosphosite'), {priority: 'event'});",
    "})"
  ))
  
  tooltips <- list(
    "Protein" = "Uniprot ID of the protein", 
    "ProteinName" = "Name of the protein", 
    "InRef" = "Shows whether the phosphosite exists in the reference proteome",
    "Phos" = "Phosphorylation as log2 fold change", 
    "Position" = "Position of the phosphosite on the protein",
    "StdErr" = "Standard error for log2 fold change",
    "StdErrT" = "Standard error to be used in t-test alongside the degrees of freedom",
    "DF" = "Degrees of freedom",
    "InflationFactor" = "Estimates the additional variance due to the measurement of standard deviations in t-test. Equal to the ratio of StdErr/StdErrT",
    "TStat" = "T-statistic for the t-test",
    "ZScore" = "Standardized log fold changes",
    "PValue" = "P-value",
    "FDR" = "False discovery rate",
    "EffectiveMag" = "Reliable portion of the log2 fold changes beyond 3 standard errors",
    "isSignificant" = "Is the phosphorylation significant", 
    # "EffectiveMag" = "log2FC - 3*StdErr",
    "dummy" = ""
  )
  

  #   { targets: [0, 1], visible: true},
  #   { targets: '_all', visible: false }
  # ]
  
  fn = 'phosphosite_table'
  tab <- DT::datatable(ST, rownames= FALSE, extensions = 'Buttons',
                       callback=JS(callback),
                       selection = "single",
                       options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                      stateSave = stateSave, 
                                      stateLoadParams = stateLoadParams,
                                      columnDefs = foHideColumns(ST, c("InflationFactor", "StdErrT", "TStat")),
                                      columns = columns,
                                      initComplete = foAddTooltips(colnames(ST), tooltips),
                                      paging = TRUE, searching = TRUE, pageLength = pageLength, dom = dom, buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn), "colvis"))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3)
  return(tab)
} 

createProtExpressionDataTable <- function(ST, maintable = T){
  ST$Phos = round(ST$Phos, digits = 3)
  ST$StdErr = round(ST$StdErr, digits = 3)
  ST$ZScore = round(ST$ZScore, digits = 3)
  ST$MagnitudeAdj = round(ST$MagnitudeAdj, digits = 3)
  ST$EffectiveMag = pmax(ST$MagnitudeAdj, 0)
  # ST$InRef = ifelse(ST$InRef, "True", "False")
  
  si <- order(abs(ST$ZScore), decreasing = TRUE)
  ST <- ST[si,]
  si <- order(abs(ST$EffectiveMag), decreasing = TRUE)
  ST <- ST[si,]
  
  ST = subset(ST, select = -c(Identifier, MagnitudeAdj, Position, NetworkDataIndex, InRef))
  colnames(ST)[which(names(ST) == "Protein")] <- "ID"
  colnames(ST)[which(names(ST) == "ProteinName")] <- "Name"
  colnames(ST)[which(names(ST) == "Phos")] <- "Log2FC"
  
  ST <- formatNumericVariables(ST)
  
  if(maintable == TRUE){
    stateSave = TRUE
    stateLoadParams = JS('function (settings, data) {return false;}')
    columns = foRestoreStateIfAvailable("protExpressionTable")
    pageLength = 10
    dom = 'Bfrtip'
    callback_id = 'site_kinase_network_doubleclick'
  } else {
    stateSave = FALSE
    stateLoadParams = NULL
    columns = NULL
    pageLength = 8
    dom = 'frtip'
    callback_id = 'site_kinase_network_doubleclickb'
  }
  
  callback <- gsub('###CALLBACK_ID###', callback_id, c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[1] != null){ ", 
    " text1 = data[1]", 
    " } else {",
    " text1 = data[0]", 
    " }",
    " Shiny.setInputValue('###CALLBACK_ID###', text1.concat('_Protein'), {priority: 'event'});",
    "})"
  ))
  
  tooltips <- list(
    "ID" = "Uniprot ID of the protein", 
    "Name" = "Name of the protein", 
    "Protein" = "Uniprot ID of the protein", 
    "ProteinName" = "Name of the protein", 
    "Log2FC" = "Log2 fold change", 
    "StdErr" = "Standard error for log2 fold change",
    "StdErrT" = "Standard error to be used in t-test alongside the degrees of freedom",
    "DF" = "Degrees of freedom",
    "InflationFactor" = "Estimates the additional variance due to the measurement of standard deviations in t-test. Equal to the ratio of StdErr/StdErrT",
    "TStat" = "T-statistic for the t-test",
    "ZScore" = "Standardized log fold changes",
    "PValue" = "P-value",
    "FDR" = "False discovery rate",
    "EffectiveMag" = "Reliable portion of the log2 fold changes beyond 3 standard errors",
    "isSignificant" = "Is the expression significant", 
    # "EffectiveMag" = "log2FC - 3*StdErr",
    "dummy" = ""
  )
  

  #   { targets: [0, 1], visible: true},
  #   { targets: '_all', visible: false }
  # ]
  
  fn = 'phosphosite_table'
  tab <- DT::datatable(ST, rownames= FALSE, extensions = 'Buttons',
                       callback=JS(callback),
                       selection = "single",
                       options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                      stateSave = stateSave, 
                                      stateLoadParams = stateLoadParams,
                                      columnDefs = foHideColumns(ST, c("InflationFactor", "StdErrT", "TStat")),
                                      columns = columns,
                                      initComplete = foAddTooltips(colnames(ST), tooltips),
                                      paging = TRUE, searching = TRUE, pageLength = pageLength, dom = dom, buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn), "colvis"))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3)
  return(tab)
} 


output$siteTable <- DT::renderDataTable(server = FALSE, {
  req(site_table_processed())
  ST <- site_table_processed();
  tab <- createSiteDataTable(ST, maintable = T)
  
  return(tab)
})

output$protExpressionTable <- DT::renderDataTable(server = FALSE, {
  req(protexpression_table_processed())
  ST <- protexpression_table_processed();
  tab <- createProtExpressionDataTable(ST, maintable = T)
  
  return(tab)
})


# output$siteTable_state <- renderPrint(str(input$siteTable_state))

foProteinTableTooltips <- function(){
  tooltips <- list(
    "ID" = "Uniprot ID of the protein", 
    "Name" = "Name of the protein", 
    # "InRef" = "Shows whether the phosphosite exists in the reference proteome",
    "Phos" = "Mean phosphorylation of the sites on the protein as log2 fold change", 
    "StdErr" = "Standard error for log2 fold change",
    "DF" = "Degrees of freedom",
    "ZScore" = "Standardized log fold changes",
    "PValue" = "P-value",
    "FDR" = "False discovery rate",
    "EffectiveMag" = "Reliable portion of the log2 fold changes beyond 3 standard errors",
    "isSignificant" = "Is the mean phosphorylation significant", 
    "hasSignificantPSite" = "Does the protein have a significantly phosphorylated site on it", 
    "dummy" = ""
  )
}


output$proteinTable <- DT::renderDataTable(server = FALSE, {
  req(protein_table_processed())
  PT <- protein_table_processed();
  PT$Phos = round(PT$Phos, digits = 3)
  PT$StdErr = round(PT$StdErr, digits = 3)
  PT$ZScore = round(PT$ZScore, digits = 3)
  PT$MagnitudeAdj = round(PT$MagnitudeAdj, digits = 3)
  PT$EffectiveMag = pmax(PT$MagnitudeAdj, 0)
  
  si <- order(abs(PT$ZScore), decreasing = TRUE)
  PT <- PT[si,]
  si <- order(abs(PT$EffectiveMag), decreasing = TRUE)
  PT <- PT[si,]
  
  PT = subset(PT, select = -c(Identifier, MagnitudeAdj, KinaseIndex))
  tooltips <- foProteinTableTooltips()
  
  PT <- formatNumericVariables(PT)
  
  callback <- c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[1] != null){ ", 
    " text1 = data[1]", 
    " } else {",
    " text1 = data[0]", 
    " }",
    " Shiny.setInputValue('site_kinase_network_doubleclick', text1.concat('_Protein'), {priority: 'event'});",
    "})"
  )
  
  fn = 'protein_table'
  DT::datatable(PT, rownames= FALSE, extensions = 'Buttons', 
                callback = JS(callback), 
                selection = "single",
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               stateSave = TRUE, 
                               stateLoadParams = JS('function (settings, data) {return false;}'),
                               columns = foRestoreStateIfAvailable("proteinTable"),
                               initComplete = foAddTooltips(colnames(PT), tooltips),
                               paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn), "colvis"))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
})

output$kinaseTable <- DT::renderDataTable(server = FALSE, {
  req(kinase_table_processed_withtargets())
  KT <- kinase_table_processed_withtargets();
  KT$Activity = round(KT$Activity, digits = 3)
  KT$StdErr = round(KT$StdErr, digits = 3)
  KT$ZScore = round(KT$ZScore, digits = 3)
  KT$MagnitudeAdj = round(KT$MagnitudeAdj, digits = 3)
  KT$EffectiveMag = pmax(KT$MagnitudeAdj, 0)
  
  KT <- KT[!is.na(KT$Activity),]
  
  minsubs = input$kinase_table_minsubs
  if(is.null(minsubs)){
    minsubs = 1
  }
  KT <- KT[KT$NumSubs >= minsubs,]
  
  si <- order(abs(KT$ZScore), decreasing = TRUE)
  KT <- KT[si,]
  si <- order(abs(KT$EffectiveMag), decreasing = TRUE)
  KT <- KT[si,]
  
  KT = subset(KT, select = -c(MagnitudeAdj, Protein))
  colnames(KT)[1] <- "UniprotID"
  colnames(KT)[2] <- "Name"
  
  KT <- formatNumericVariables(KT, exclude = c("NumSubs"))
  
  tooltips <- list(
    "UniprotID" = "Uniprot ID of the kinase", 
    "Name" = "Name of the kinase", 
    "Gene" = "Gene symbol", 
    # "InRef" = "Shows whether the phosphosite exists in the reference proteome",
    "NumSubs" = "Number of substrates of the kinase with quantifications",
    "NumSubsRokai" = "Effective number of substrates of the kinase for the activity inference in RoKAI, including the phosphosites in functional neighborhood", 
    "Activity" = "Inferred activity of the kinase", 
    "StdErr" = "Standard error for the inferred activity",
    "DF" = "Degrees of freedom",
    "WeightSubs" = "Weight of the known substrates of a kinase in the activity inference",
    "WeightNeigh" = "Weight of the phosphosites in the functional neighborhood of a kinase in the inference",
    "PhosSubs" = "Mean phosphorylation of known substrates of a kinase (mean log2 fold change)",
    "PhosNeigh" = "Mean phosphorylation of the sites in the functional neighborhood of a kinase (mean log2 fold change)",
    "ZScoreSubs" = "Standardized score for the mean phosphorylation of known substrates of a kinase",
    "ZScoreNeigh" = "Standardized score for the the sites in the functional neighborhood of a kinase",
    "ZScore" = "Standardized score for activity",
    "PValue" = "P-value",
    "FDR" = "False discovery rate",
    "EffectiveMag" = "Reliable portion of the activity beyond 2 standard errors",
    "isSignificant" = "Is the activity significant", 
    # "EffectiveMag" = "log2FC - 3*StdErr",
    "dummy" = ""
  )
  
  callback <- c(
    "table.on('dblclick','tr', function() {",
    " var data=table.row(this).data(); ",
    " let text1;", 
    " if(data[1] != null){ ", 
    " text1 = data[1]", 
    " } else {",
    " text1 = data[0]", 
    " }",
    " Shiny.setInputValue('site_kinase_network_doubleclick', text1.concat('_Kinase'), {priority: 'event'});",
    "})"
  )
  
  targetsIndex = match('Targets', colnames(KT)) - 1
  
  columnDefs = foHideColumns(KT, c("NumSubsRokai", "WeightSubs", "PhosSubs", "ZScoreSubs", "WeightNeigh", "PhosNeigh", "ZScoreNeigh"))
  # browser()
  columnDefs = c(list(list(targets = targetsIndex, width = '450px')), columnDefs)
  # browser()
  
  fn = 'kinase_table'
  DT::datatable(KT, rownames= FALSE, extensions = 'Buttons', 
                callback = JS(callback), 
                selection = "single",
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               autoWidth = TRUE, 
                               stateSave = TRUE, 
                               stateLoadParams = JS('function (settings, data) {return false;}'),
                               # columnDefs = list(
                               #   list(targets = 0, title = "ID"),
                               #   list(targets = 1, title = "Name")
                               # ),
                               initComplete = foAddTooltips(colnames(KT), tooltips),
                               columnDefs = columnDefs,
                               columns = foRestoreStateIfAvailable("kinaseTable"),
                               paging = TRUE, searching = TRUE, pageLength = 8, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn), "colvis"))) %>% 
    formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
})


