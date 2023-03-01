
downloadExcelReportFileHandler <- function(wb, file_name){
  downloadHandler(
    filename = function() { paste(file_name, sep='') },
    content = function(file) {
      tryCatch({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Preparing file for download", value = 0)
      progress$inc(0.99, detail = sprintf("%.0f%%", 100*0.99))
      if(is.reactive(wb)){
        wb = wb()
        saveWorkbook(wb, file, TRUE)
      }
      }, 
      error = function(e){message(paste0("An error occurred: ", e))}
      )
      # write.csv(data_file, file = file, row.names = FALSE, quote=FALSE)
    }
  )
}

observeEvent(input$test_xyz, {
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
  return(out)
})

foExcelDefaultColoring <- function(wb, ST, sheet, nmax = 5000){
  styles = report_styles()
  ncols = ncol(ST)
  conditionalFormatting(wb, sheet,
                        cols = 1:ncols,
                        rows = 2:nmax, type = "Contains", rule = "", style = styles$defaultStyle
  )
  return(wb)
}


foExcelInfoFormatting <- function(wb, ST, sheet, infoColumns, nmax = 5000){
  styles = report_styles()
  idx = match(infoColumns, colnames(ST))
  idx = idx[!is.na(idx)]
  if(length(idx) > 0){
    for(iX in idx){
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = 2:nmax, type = "Contains", rule = "", style = styles$infoStyle
      )
    }
  }
  return(wb)
}

foExcelBinaryFormatting <- function(wb, ST, sheet, binaryColumns, nmax = 5000){
  styles = report_styles()
  idx = match(binaryColumns, colnames(ST))
  idx = idx[!is.na(idx)]
  if(length(idx) > 0){
    for(iX in idx){
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = 2:nmax, type = "Contains", rule = "TRUE", style = styles$trueStyle
      )
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = 2:nmax, type = "Contains", rule = "FALSE", style = styles$falseStyle
      )
    }
  }
  return(wb)
}

foExcelSignedFormatting <- function(wb, ST, sheet, signedColumns, nmax = 5000){
  styles = report_styles()
  idx = match(signedColumns, colnames(ST))
  idx = idx[!is.na(idx)]
  if(length(idx) > 0){
    for(iX in idx){
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = 2:nmax, rule = ">0", style = styles$posStyle
      )
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = 2:nmax, rule = "<0", style = styles$negStyle
      )
      conditionalFormatting(wb, sheet,
                            cols = iX,
                            rows = 2:nmax, type = "notContains", rule = "", style = styles$whiteStyle
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



foFormatSTtable <- function(wb, ST, group_name){
  ST = subset(ST, select = -c(Identifier, MagnitudeAdj, NetworkDataIndex, ID, EffectiveMag))
  ST[is.na(ST)] = NA
  sorted_cols = c("Protein", "ProteinName", "Position", "InRef", "isSignificant", "ZScore", "TStat", "Phos", "StdErr", "DF", "PValue", "FDR")
  ST = foSortTable(ST, sorted_cols)
  ST = formatNumericVariables(ST, tostring = F)
  ST = formatPValues(ST)
  si <- order(abs(ST$ZScore), decreasing = TRUE)
  ST <- ST[si,]
  
  styles = report_styles()
  writeDataTable(wb, group_name, ST, headerStyle = styles$headerStyle, tableStyle ="none")
  
  signedColumns = c("TStat", "ZScore", "Phos")
  infoColumns = c("Protein", "ProteinName", "Position", "InRef")
  binaryColumns = c("isSignificant")
  wb = foExcelDefaultColoring(wb, ST, group_name)
  wb = foExcelSignedFormatting(wb, ST, group_name, signedColumns)
  wb = foExcelInfoFormatting(wb, ST, group_name, infoColumns)
  wb = foExcelBinaryFormatting(wb, ST, group_name, binaryColumns)

  return(wb)
}

report_workbook <- reactive({
  req(current_dataset())
  req(current_metadata())
  ds_initial <- current_dataset_mapped()
  Tmeta_initial <- current_metadata()
  norm_by = c("Timepoint", "Gender")
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
  
  unique_groupings = unique(Tx$Identifier)
  nstep = length(unique_groupings)
  progress_step = (1/(nstep+1))
  currentProgress = 0
  for(iGrouping in 1:length(unique_groupings)){
    currentProgress = currentProgress + progress_step
    progress$inc(progress_step, detail = sprintf("%.0f%%", 100*currentProgress))
    
    group_name = unique_groupings[iGrouping]
    group_name_pretty = trimws(gsub("_", " ", group_name))
    validSamples = Tx$Identifier == group_name
    message(paste0(iGrouping, " - ", group_name_pretty))
    addWorksheet(wb, group_name_pretty)
    ds = foFilterDataset(ds_initial, validSamples)
    Tmeta = foFilterMetadata(Tmeta_initial, validSamples)
    ds = foPreprocessDataset(fo_process_dataset(ds, Tmeta))
    ST = foProcessSiteTable(foPrepareSiteTable(ds))
    wb = foFormatSTtable(wb, ST, group_name_pretty)
  }
  progress$set(message = "Completed", value = 1)
  
  return(wb)
})


# test_report <- reactive({
#   Tsamplex = site_table_processed()
#   
#   signedColumns = c("TStat", "ZScore")
#   idx = match(signedColumns, colnames(Tsamplex))
#   idx = idx[!is.na(idx)]
#   
#   start = 5
#   valids = 5:ncol(Tsamplex)
#   # Tsamplex = Tsample
#   # Tsamplex[, valids] = log2(Tsamplex[, valids])
#   # Tsamplex[, valids] = Tsamplex[, valids] - apply(Tsamplex[, 3:ncol(Tsamplex)], 1, function(x) mean(x, na.rm=T))
#   Tsamplex = formatNumericVariables(Tsamplex, tostring = F)
#   # Tsamplex[, valids] = round(Tsamplex[, valids], 2)
#   Tsamplex[is.na(Tsamplex)] = NA
#   
#   library(openxlsx)
#   wb <- createWorkbook()
#   
#   headerStyle = createStyle(fontColour = "#000001", fgFill = "#FFFFFF", textDecoration = "bold")
#   
#   # addWorksheet(wb, "Moving Col")
#   addWorksheet(wb, "Abcd")
#   writeDataTable(wb, "Abcd", Tsamplex, headerStyle = headerStyle, tableStyle ="none")
#   # writeData(wb, "Abcd", Tsamplex, keepNA = TRUE, na.string = "")
#   
#   
#   whiteStyle <- createStyle()
#   # negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
#   # posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
#   negStyle <- createStyle(fontColour = "#2D32D7", bgFill = "#EBF6FF")
#   posStyle = createStyle(fontColour = "#9C0A5F", bgFill = "#FFEBF6")
#   
#   conditionalFormatting(wb, "Abcd",
#                         cols = idx,
#                         rows = 2:5000, rule = ">0", style = posStyle
#   )
#   conditionalFormatting(wb, "Abcd",
#                         cols = idx,
#                         rows = 2:5000, rule = "<0", style = negStyle
#   )
#   conditionalFormatting(wb, "Abcd",
#                         cols = idx,
#                         rows = 2:5000, type = "notContains", rule = "", style = whiteStyle
#   )
#   return(wb)
# })

output$test_report_generator <- downloadExcelReportFileHandler(report_workbook, 'report.xlsx')


