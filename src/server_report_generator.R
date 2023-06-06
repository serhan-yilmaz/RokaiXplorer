
downloadExcelReportFileHandler <- function(wb, file_name){
  downloadHandler(
    filename = function() { paste(file_name, sep='') },
    content = function(file) {
      tryCatch({
      if(is.reactive(wb)){
        wb = wb()
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Preparing file for download", value = 0)
        progress$inc(0.99, detail = sprintf("%.0f%%", 100*0.99))
        saveWorkbook(wb, file, TRUE)
      }
      }, 
      error = function(e){message(paste0("An error occurred: ", e))}
      )
      # write.csv(data_file, file = file, row.names = FALSE, quote=FALSE)
    }
  )
}

cache$cached_report_worksheet_grouping = reactiveVal("")

output$report_group_options <- renderUI({
  validate(
    need(metadata_ready(), "Waiting for data...")
  )
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  selected_grouping = fo_restore_if_applicable(groups, isolate(cache$cached_report_worksheet_grouping()))
  
  tags$span(
    multiChoicePicker("report_worksheet_grouping", "Worksheet Grouping:", groups, 
                      selected = selected_grouping, 
                      isInline = "F", multiple = T, max_opts = 3, width = 170, 
                      style = "display:flex;flex-direction: column; margin-bottom:6px;",
                      class_names = "abc", 
                      picker_inline = F,
                      tooltip = "Reports the results in separate worksheets for each combination of the selected groups.")
  )
})

observeEvent(input$prepare_report, {
  a = report_workbook()
  if(is.nill(a)){
    shinyjs::disable("test_report_generator")
  } else {
    shinyjs::enable("test_report_generator")
  }
})

formatfdr <- function(x){
  x = round_m(x, digits = c(), sigdigits = 3)
  return(x)
}

report_styles <- reactive({
  library(openxlsx)
  out = list()
  
  # out$headerStyle = createStyle(fontColour = "#000001", fgFill = "#FFFFFF", textDecoration = "bold")
  out$headerStyle = createStyle(fontColour = "#000001", fgFill = "#B8CCE4", textDecoration = "bold")
  out$whiteStyle <- createStyle()
  out$negStyle <- createStyle(fontColour = "#2D32D7", bgFill = "#EBF6FF")
  out$posStyle = createStyle(fontColour = "#9C0A5F", bgFill = "#FFEBF6")
  out$infoStyle = createStyle(bgFill = "#F4F3EC")
  out$defaultStyle = createStyle(bgFill = "#F2F2F2")
  out$falseStyle = createStyle(bgFill = "#FEFEFE")
  out$trueStyle = createStyle(fontColour = "#9C0055", bgFill = "#FFC7CE")
  out$worksheetHeaderStyle = createStyle(halign = "center", valign = "bottom", fontSize = 11.5)
  return(out)
})

foExcelDefaultColoring <- function(wb, ST, sheet, nmax = 5000, startRow = 1){
  styles = report_styles()
  ncols = ncol(ST)
  rowsX = (startRow - 1) + (2:nmax)
  conditionalFormatting(wb, sheet,
                        cols = 1:ncols,
                        rows = rowsX, type = "Contains", rule = "", style = styles$defaultStyle
  )
  return(wb)
}


foExcelInfoFormatting <- function(wb, ST, sheet, infoColumns, nmax = 5000, startRow = 1){
  styles = report_styles()
  idx = match(infoColumns, colnames(ST))
  idx = idx[!is.na(idx)]
  rowsX = (startRow - 1) + (2:nmax)
  if(length(idx) > 0){
    for(iX in idx){
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = rowsX, type = "Contains", rule = "", style = styles$infoStyle
      )
    }
  }
  return(wb)
}

foExcelBinaryFormatting <- function(wb, ST, sheet, binaryColumns, nmax = 5000, startRow = 1){
  styles = report_styles()
  idx = match(binaryColumns, colnames(ST))
  idx = idx[!is.na(idx)]
  rowsX = (startRow - 1) + (2:nmax)
  if(length(idx) > 0){
    for(iX in idx){
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = rowsX, type = "Contains", rule = "TRUE", style = styles$trueStyle
      )
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = rowsX, type = "Contains", rule = "FALSE", style = styles$falseStyle
      )
    }
  }
  return(wb)
}

foExcelSignedFormatting <- function(wb, ST, sheet, signedColumns, nmax = 5000, startRow = 1){
  styles = report_styles()
  idx = match(signedColumns, colnames(ST))
  idx = idx[!is.na(idx)]
  rowsX = (startRow - 1) + (2:nmax)
  if(length(idx) > 0){
    for(iX in idx){
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = rowsX, rule = ">0", style = styles$posStyle
      )
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = rowsX, rule = "<0", style = styles$negStyle
      )
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = rowsX, type = "notContains", rule = "", style = styles$whiteStyle
      )
    }
  }
  return(wb)
}

formatPValues <- function(ST, cols = c("PValue", "FDR")){
  for(colname in cols){
    ST[[colname]] = formatfdr(ST[[colname]])
  }
  return(ST)
}

foSortTable <- function(ST, sorted_cols){
  idx = match(sorted_cols, colnames(ST))
  sorted_cols = sorted_cols[!is.na(idx)]
  ST = ST[, sorted_cols]
  return(ST)
}


foGetTableParameters <- function(analysis_level){
  out = list()
  out$to_numeric = c()
  out$absolute_sorting = TRUE
  out$fitlastcolumn = FALSE
  switch(analysis_level, 
         "Phosphosites" = {
           out$sorted_cols = c("Protein", "ProteinName", "Position", "InRef", "isSignificant", "ZScore", "TStat", "Phos", "StdErr", "DF", "PValue", "FDR")
           out$title = "Phosphosites"
         }, 
         "Phosphoproteins" = {
           out$sorted_cols = c("ID", "Name", "isSignificant", "ZScore", "TStat", "Phos", "StdErr", "DF", "PValue", "FDR")
           out$title = "Phosphoproteins"
         }, 
         "Protein Expression" = {
           out$sorted_cols = c("Protein", "ProteinName", "isSignificant", "ZScore", "TStat", "Phos", "StdErr", "DF", "PValue", "FDR")
           out$title = "Protein Expression"
         },
         "Kinases" = {
           out$sorted_cols = c("KinaseID", "KinaseName", "NumSubs", "isSignificant", "ZScore", "TStat", "Activity", "StdErr", "DF", "PValue", "FDR", "Targets")
           out$title = "Kinases"
           out$to_numeric = c("NumSubs", "ZScore", "Activity", "StdErr", "DF")
           out$fitlastcolumn = TRUE
         }, 
         "Enrichment" = {
           out$sorted_cols = c("ID", "Name", "Category", "HitRatio", "ObsRatio", "isSignificant", "ZScore", "LogRiskRatio", "ChiSqr", "PValue", "FDR", "Hits", "AllTargets")
           out$title = paste0("Enrichment", " (", input$enrichment_datasource, ")")
           out$to_numeric = c("ZScore")
           out$absolute_sorting = FALSE
           out$fitlastcolumn = TRUE
         }, 
         stop("Invalid data source for the report generator")
  )
  
  return(out)
}

formatCols <- function(ST, col_names, foFormatting){
  names = colnames(ST)
  idx = match(col_names, colnames(ST))
  idx = idx[!is.na(idx)]
  if(length(idx) > 0){
    for(iX in idx){
      col = names[iX]
      ST[[col]] = foFormatting(ST[[col]])
    }
  }
  return(ST)
}

formatToNumeric <- function(ST, to_numeric_cols){
  return(formatCols(ST, to_numeric_cols, foFormatting = as.numeric))
}

formatDF <- function(ST){
  foDFformatting <- function(X){
    X = formatNumber(X, tostring = F, digits = 2);
    X[is.infinite(X)] = -1
    return(X)
  }
  return(formatCols(ST, c("DF"), foFormatting = foDFformatting))
}

foFormatSTtable <- function(wb, ST, group_name, analysis_level){
  tableparam = foGetTableParameters(analysis_level)
  
  # browser()
  # ST = subset(ST, select = -c(Identifier, MagnitudeAdj, NetworkDataIndex, ID, EffectiveMag))
  ST[is.na(ST)] = NA
  # sorted_cols = c("Protein", "ProteinName", "Position", "InRef", "isSignificant", "ZScore", "TStat", "Phos", "StdErr", "DF", "PValue", "FDR")
  ST = foSortTable(ST, tableparam$sorted_cols)
  ST = formatToNumeric(ST, tableparam$to_numeric)
  ST = formatNumericVariables(ST, tostring = F)
  ST = formatDF(ST)
  # ST$DF = formatNumber(ST$DF, tostring = F, digits = 2);
  # ST$DF[is.infinite(ST$DF)] = -1
  
  ST = formatPValues(ST)
  if(tableparam$absolute_sorting){
    si <- order(abs(ST$ZScore), decreasing = TRUE)
  } else {
    si <- order(ST$ZScore, decreasing = TRUE)
  }
  
  ST <- ST[si,]
  
  styles = report_styles()
  
  addTitle = TRUE
  if(addTitle){
    startRow = 2
    title = tableparam$title
    if(!identical(group_name, "Report")){
      title = paste0(tableparam$title, " [", group_name, "]")
    }
    writeData(wb, group_name, title)
    mergeCells(wb, group_name, 1:ncol(ST), 1)
    addStyle(wb, group_name, styles$worksheetHeaderStyle, rows = 1, cols = 1)
  } else {
    startRow = 1
  }
  writeDataTable(wb, group_name, ST, headerStyle = styles$headerStyle, tableStyle ="none", startRow = startRow)
  
  signedColumns = c("TStat", "ZScore", "Phos", "Activity")
  infoColumns = c("Protein", "ProteinName", "Position", "InRef", "ID", "Name", "Category", "HitRatio", "ObsRatio")
  # infoColumns = c("Protein", "ProteinName", "Position", "InRef")
  binaryColumns = c("isSignificant")
  wb = foExcelDefaultColoring(wb, ST, group_name, startRow = startRow)
  wb = foExcelSignedFormatting(wb, ST, group_name, signedColumns, startRow = startRow)
  wb = foExcelInfoFormatting(wb, ST, group_name, infoColumns, startRow = startRow)
  wb = foExcelBinaryFormatting(wb, ST, group_name, binaryColumns, startRow = startRow)
  if(tableparam$fitlastcolumn){
    startCol = ncol(ST) + 1
    writeData(wb, group_name, rep(NA, nrow(ST)+1), startRow = startRow, startCol = startCol, keepNA = T, na.string = "")
  }
  
  return(wb)
}

report_workbook <- reactive({
  req(current_dataset())
  req(current_metadata())
  ds_initial <- current_dataset_mapped()
  Tmeta_initial <- current_metadata()
  
  norm_by = input$report_worksheet_grouping
  if(is.null(norm_by)){
    norm_by = c()
  }
  # norm_by = c("Timepoint", "Gender")
  nGrouping = length(norm_by)
  
  Tx <- as.data.frame(t(Tmeta_initial$Tsample_metadata))
  Tx$Identifier = rep("", nrow(Tx))
  if(nGrouping > 0){
    for(iGrouping in 1:nGrouping){
      Tx$Identifier = paste(Tx$Identifier, Tx[[norm_by[iGrouping]]], sep = "_")
    }
  }
  
  library(openxlsx)
  wb <- createWorkbook()
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Preparing the report", value = 0)
  
  # analysis_level = "Phosphosites"
  analysis_level = input$report_data_source
  
  unique_groupings = sort(unique(Tx$Identifier))
  nstep = length(unique_groupings)
  progress_step = (1/(nstep+1))
  currentProgress = 0
  for(iGrouping in 1:length(unique_groupings)){
    currentProgress = currentProgress + progress_step
    progress$inc(progress_step, detail = sprintf("%.0f%%", 100*currentProgress))
    
    group_name = unique_groupings[iGrouping]
    group_name_pretty = trimws(gsub("_", "-", group_name))
    if(nchar(group_name_pretty) >= 2){
      group_name_pretty = substring(group_name_pretty, 2)
    }
    if(nGrouping == 0){
      group_name_pretty = "Report"
    }
    validSamples = Tx$Identifier == group_name
    message(paste0(iGrouping, " - ", group_name_pretty))
    tryCatch({
      ds = foFilterDataset(ds_initial, validSamples)
      Tmeta = foFilterMetadata(Tmeta_initial, validSamples)
      
      switch(analysis_level, 
      "Phosphosites" = {
        ds = foPreprocessDataset(fo_process_dataset(ds, Tmeta))
        ST = foProcessSiteTable(foPrepareSiteTable(ds))
      }, 
      "Phosphoproteins" = {
        ds = foPreprocessDataset(fo_process_dataset(ds, Tmeta))
        ST = foProcessSiteTable(foPrepareSiteTable(ds))
        ST = foComputeProteinTable(ST)
      }, 
      "Protein Expression" = {
        ds = foPreprocessDataset(fo_process_dataset(ds, Tmeta, type = "expression"))
        ST = foProcessProtExpressionTable(foPrepareSiteTable(ds, sitetable = FALSE))
      },
      "Kinases" = {
        ds = foPreprocessDataset(fo_process_dataset(ds, Tmeta))
        ST = foComputeKinaseTable(ds)
      }, 
      "Enrichment" = {
        switch(input$enrichment_datasource, 
               "Phosphosites" = {
                 ds = foPreprocessDataset(fo_process_dataset(ds, Tmeta))
                 ST = foProcessSiteTable(foPrepareSiteTable(ds))
               },
               "Phosphoproteins" = {
                 ds = foPreprocessDataset(fo_process_dataset(ds, Tmeta))
                 ST = foProcessSiteTable(foPrepareSiteTable(ds))
                 ST = foComputeProteinTable(ST)
                 ST$Protein = ST$ID; 
               },
               "Protein Expression" = {
                 ds = foPreprocessDataset(fo_process_dataset(ds, Tmeta, type = "expression"))
                 ST = foProcessProtExpressionTable(foPrepareSiteTable(ds, sitetable = FALSE))
               }, 
               stop("Invalid data source for enrichment")
        )
        ST = foComputeEnrichmentTable(ST)
        ST <- ST[ST$numIdentified >= 1, ]
        # ST <- ST[ST$numSignificant >= input$enrichment_minhits, ]
        # cats <- foEnrichmentCategories(input$enrichment_table_categories)
        # # cats <- reactive_enrichment_table_categories()
        # valids = !is.na(match(ST$Category, cats))
        # ST <- ST[valids, ]
        # if(input$enrichment_table_significantonly == TRUE){
        #   ST = ST[ST$isSignificant, ]
        # }
      }, 
      stop("Invalid data source for the report generator")
      )
      addWorksheet(wb, group_name_pretty)
      wb = foFormatSTtable(wb, ST, group_name_pretty, analysis_level)
    }, error = function(e){message(paste0("Skipped ", group_name_pretty, " - ", e))})
  }
  progress$set(message = "Completed", value = 1)
  
  return(wb)
})

output$test_report_generator <- downloadExcelReportFileHandler(report_workbook, 'report.xlsx')


