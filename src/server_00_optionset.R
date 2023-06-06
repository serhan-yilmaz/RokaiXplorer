option_set = list(
  "refproteome" = list(type = "pickerInput"),
  "subgroup_select1" = list(type = "selectInput"),
  "subgroup_select2" = list(type = "selectInput"),
  "subgroup_select3" = list(type = "selectInput"),
  "subgroup_select4" = list(type = "selectInput"),
  "subgroup_select5" = list(type = "selectInput"),
  "subgroup_select6" = list(type = "selectInput"),
  "select_group_differences" = list(type = "selectInput"),
  "select_subgroup_A" = list(type = "selectInput"),
  "select_subgroup_B" = list(type = "selectInput"),
  "sitelevel_volcano_maxfdr" = list(type = "sliderInput"),
  "sitelevel_volcano_minlogfc" = list(type = "sliderInput"),
  "sitelevel_volcano_showlegend" = list(type = "materialSwitch"),
  "site_barplot_maxitems" = list(type = "sliderInput"),
  "site_barplot_minzscore" = list(type = "sliderInput"),
  "site_barplot_significant_only" = list(type = "materialSwitch"),
  "site_barplot_yaxis" = list(type = "pickerInput"),
  # "site_barplot_coloring" = list(type = "pickerInput"),
  "site_heatmap_maxitems" = list(type = "sliderInput"),
  "site_heatmap_minzscore" = list(type = "sliderInput"),
  "site_heatmap_significant_only" = list(type = "materialSwitch"),
  "site_heatmap_intensity_fc_style" = list(type = "pickerInput"),
  "site_heatmap_select_group" = list(type = "pickerInput"),
  "site_kinase_network_maxitems" = list(type = "sliderInput"),
  "site_kinase_network_minzscore" = list(type = "sliderInput"),
  "site_kinase_network_significant_only" = list(type = "materialSwitch"),
  "site_kinase_network_single_kinases" = list(type = "materialSwitch"),
  "proteinlevel_volcano_maxfdr" = list(type = "sliderInput"),
  "proteinlevel_volcano_minlogfc" = list(type = "sliderInput"),
  "proteinlevel_volcano_showlegend" = list(type = "materialSwitch"),
  "protein_barplot_maxitems" = list(type = "sliderInput"),
  "protein_barplot_minzscore" = list(type = "sliderInput"),
  "protein_barplot_significant_only" = list(type = "materialSwitch"),
  "protein_barplot_yaxis" = list(type = "pickerInput"),
  # "protein_barplot_coloring" = list(type = "pickerInput"),
  "protein_heatmap_maxitems" = list(type = "sliderInput"),
  "protein_heatmap_minzscore" = list(type = "sliderInput"),
  "protein_heatmap_significant_only" = list(type = "materialSwitch"),
  "protein_heatmap_intensity_fc_style" = list(type = "pickerInput"),
  "protein_heatmap_select_group" = list(type = "pickerInput"),
  "protein_kinase_network_maxitems" = list(type = "sliderInput"),
  "protein_kinase_network_minzscore" = list(type = "sliderInput"),
  "protein_kinase_network_significant_only" = list(type = "materialSwitch"),
  "protein_kinase_network_single_kinases" = list(type = "materialSwitch"),
  "protexpression_volcano_showlegend" = list(type = "materialSwitch"),
  "protexpression_volcano_maxfdr" = list(type = "sliderInput"),
  "protexpression_volcano_minlogfc" = list(type = "sliderInput"),
  "protexpression_barplot_maxitems" = list(type = "sliderInput"),
  "protexpression_barplot_minzscore" = list(type = "sliderInput"),
  "protexpression_barplot_significant_only" = list(type = "materialSwitch"),
  "protexpression_barplot_yaxis" = list(type = "pickerInput"),
  # "protexpression_barplot_coloring" = list(type = "pickerInput"),
  "protexpression_heatmap_maxitems" = list(type = "sliderInput"),
  "protexpression_heatmap_minzscore" = list(type = "sliderInput"),
  "protexpression_heatmap_significant_only" = list(type = "materialSwitch"),
  "protexpression_heatmap_intensity_fc_style" = list(type = "pickerInput"),
  "protexpression_heatmap_select_group" = list(type = "pickerInput"),
  "ksNetwork" = list(type = "pickerInput"),
  "rokaiNetwork" = list(type = "pickerInput"),
  "kinaselevel_minsubs" = list(type = "sliderInput"),
  "rokaiEnabled" = list(type = "checkboxInput"),
  "kinaselevel_minsubs" = list(type = "sliderInput"),
  "kinaselevel_volcano_maxfdr" = list(type = "sliderInput"),
  "kinaselevel_volcano_showlegend" = list(type = "materialSwitch"),
  "kinase_barplot_maxitems" = list(type = "sliderInput"),
  "kinase_barplot_minzscore" = list(type = "sliderInput"),
  "kinase_barplot_significant_only" = list(type = "materialSwitch"),
  "kinase_barplot_yaxis" = list(type = "pickerInput"),
  # "kinase_barplot_coloring" = list(type = "pickerInput"),
  "kinase_barplot_minsubs" = list(type = "sliderInput"),
  "kinase_heatmap_maxitems" = list(type = "sliderInput"),
  "kinase_heatmap_minzscore" = list(type = "sliderInput"),
  "kinase_heatmap_significant_only" = list(type = "materialSwitch"),
  "kinase_heatmap_intensity_fc_style" = list(type = "pickerInput"),
  "kinase_heatmap_select_group" = list(type = "pickerInput"),
  "kinase_table_minsubs" = list(type = "sliderInput"),
  "pathwaylevel_volcano_maxfdr" = list(type = "sliderInput"),
  "pathwaylevel_volcano_showlegend" = list(type = "materialSwitch"),
  "enrichment_apply_yates_correction" = list(type = "materialSwitch"),
  "enrichment_categories" = list(type = "pickerInput"),
  "enrichment_mintargets" = list(type = "sliderInput"),
  "enrichment_minobservedratio" = list(type = "sliderInput"),
  "enrichment_filterbyoverlap" = list(type = "materialSwitch"),
  "enrichment_maxoverlap" = list(type = "sliderInput"),
  "enrichment_datasource" = list(type = "pickerInput"),
  "enrichment_fdrcorrection" = list(type = "materialSwitch"),
  "enrichment_maxpvalue" = list(type = "sliderInput"),
  "enrichment_minlogfc" = list(type = "sliderInput"),
  "enrichment_logfcdirection" = list(type = "pickerInput"),
  "enrichment_table_categories" = list(type = "pickerInput"),
  "enrichment_table_significantonly" = list(type = "materialSwitch"),
  "enrichment_minhits" = list(type = "sliderInput"),
  "enrichment_targets_table_categories" = list(type = "pickerInput"),
  "enrichment_targets_table_significantonly" = list(type = "materialSwitch"),
  "enrichment_targets_minhits" = list(type = "sliderInput"),
  "enrichment_targets_table_hitsonly" = list(type = "materialSwitch"),
  "options_var_across_samples" = list(type = "materialSwitch"),
  "options_moderated_ttest" = list(type = "materialSwitch"),
  "options_var_across_samples" = list(type = "materialSwitch"),
  "options_center_foldchanges" = list(type = "materialSwitch"),
  "options_minsamples" = list(type = "sliderInput"),
  "options_var_stabilization" = list(type = "pickerInput"),
  "cache" = list(type = "cache"),
  "checksum"="320932023409243"
)

null_accepted_list_inputs <- list(
  "site_heatmap_select_group" = list(cache = "cached_site_heatmap_select_group", default = ""),
  "protein_heatmap_select_group" = list(cache = "cached_protein_heatmap_select_group", default = ""),
  "kinase_heatmap_select_group" = list(cache = "cached_kinase_heatmap_select_group", default = ""),
  "protexpression_heatmap_select_group" = list(cache = "cached_protexpression_heatmap_select_group", default = ""),
  "subgroup_select1" = list(cache = "cached_subgroup_select1", default = "1"),
  "subgroup_select2" = list(cache = "cached_subgroup_select2", default = "1"),
  "subgroup_select3" = list(cache = "cached_subgroup_select3", default = "1"),
  "subgroup_select4" = list(cache = "cached_subgroup_select4", default = "1"),
  "subgroup_select5" = list(cache = "cached_subgroup_select5", default = "1"),
  "subgroup_select6" = list(cache = "cached_subgroup_select6", default = "1"),
  "select_group_differences" = list(cache = "cached_select_group_differences", default = "None"),
  "select_subgroup_A" = list(cache = "cached_select_subgroup_A", default = ""),
  "select_subgroup_B" = list(cache = "cached_select_subgroup_B", default = ""),
  "modal_box_protein_tab" = list(cache = "cached_mbox_protein_tab", default = TRUE),
  "mbox_site_plot_show_samples" = list(cache = "cached_mbox_main_showsamples", default = TRUE),
  "mbox_site_plot_datasource" = list(cache = "cached_mbox_main_datasource", default = TRUE),
  "dummy" = list()
)

lapply(
  X = 1:(length(null_accepted_list_inputs)-1),
  FUN = function(i){
    iX = force(i)
    name = names(null_accepted_list_inputs[iX])
    cache_name = null_accepted_list_inputs[[name]]$cache
    cache_default = null_accepted_list_inputs[[name]]$default
    if(is.null(cache_default)){
      cache_default = "";
    }
    if(!is.null(cache_name) && !is.na(cache_name) && !is.empty(cache_name)){
      observeEvent(input[[name]], {
        value = input[[name]]
        cache_val = foGetCacheValue(cache_name)
        if(!is.null(value) || (!identical(cache_val, cache_default))){
          value_txt = value
          if(is.null(value)){
            value_txt = "NULL"
          }
          # message(sprintf("Cache updated %s: %s", cache_name, value_txt))
          isolate(foRestoreCache(cache_name, value))
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    }
  }
)

foSynchCache <- function(){
  for(iX in 1:(length(null_accepted_list_inputs)-1)){
    name = names(null_accepted_list_inputs[iX])
    cache_name = null_accepted_list_inputs[[name]]$cache
    cache_default = null_accepted_list_inputs[[name]]$default
    if(is.null(cache_default)){
      cache_default = "";
    }
    if(!is.null(cache_name) && !is.na(cache_name) && !is.empty(cache_name)){
      value = input[[name]]
      cache_val = foGetCacheValue(cache_name)
      if(!is.null(value) || (!identical(cache_val, cache_default))){
        value_txt = value
        if(is.null(value)){
          value_txt = "NULL"
        }
        isolate(foRestoreCache(cache_name, value))
      }
    }
  }
}

foInputList <- function(){
  foSynchCache()
  out = list()
  for(iX in 1:(length(option_set) - 1)){
    name = names(option_set[iX])
    type = option_set[iX][[name]]$type
    if(type != "cache"){
      value = input[[name]]
    } else {
      value = foGetCacheValues()
      # if(name == "cache"){
      #   
      # } else {
      #   value = foGetCacheValue(name)
      # }
    }
    # if(name == "site_heatmap_select_group"){
    #   browser()
    # }
    # 
    if(!is.null(value)){
      out[[name]] = value
    } else {
      if(any(stringr::str_detect(names(null_accepted_list_inputs), name))){
        cache_name = null_accepted_list_inputs[[name]]$cache
        cache_default = null_accepted_list_inputs[[name]]$default
        if(is.null(cache_default)){
          cache_default = "";
        }
        if(!is.null(cache_name) && !is.na(cache_name) && !is.empty(cache_name)){
          val = foGetCacheValue(cache_name)
          if(identical(val, cache_default)){
            val = NULL
          }
        } else {
          val = NA
        }
        
        # If null is accepted
        out[[name]] = val
      }
    }
  }
  if(nchar(input$config_name) > 0){
    out$config_name = input$config_name
  }
  out
}

foRestoreConfig <- function(inputlist, ignore_input_not_found_error = F){
  out = list()
  stop_at_the_end = c()
  for(iX in 1:(length(option_set) - 1)){
    name = names(option_set[iX])
    type = option_set[iX][[name]]$type
    value = inputlist[[name]]
    if(is.null(value) || is.na(value)){
      if(!any(stringr::str_detect(null_accepted_list_inputs, name))){
        # If null is not accepted
        if(stringr::str_detect(name, "subgroup_select")){ # Ignore
          next; 
        }
        if(stringr::str_detect(name, "select_subgroup")){ # Ignore
          next; 
        }
        if(stringr::str_detect(name, "select_group_differences")){ # Ignore
          next; 
        }
        if(stringr::str_detect(name, "select_group")){ # Ignore
          next; 
        }
        if(stringr::str_detect(name, "refproteome")){ # Ignore
          next; 
        }
        if(length(stop_at_the_end) == 0){
          stop_at_the_end = name;
        }
        next;
      }
    }
    switch(type,
           "numericInput" = {updateNumericInput(session, name, value = value)},
           "selectInput" = {updateSelectInput(session, name, selected = value)},
           "pickerInput" = {updatePickerInput(session, name, selected = value)},
           "sliderInput" = {updateSliderInput(session, name, value = value)},
           "checkboxInput" = {updateCheckboxInput(session, name, value = value)},
           "materialSwitch" = {updateMaterialSwitch(session, name, value = value)},
           "anumericInput" = {updateAutonumericInput(session, name, value = value)},
           "cache" = {foRestoreFullCache(value)},
           stop(paste0("Invalid option type: ", type))
    )
  }
  if(length(stop_at_the_end)>0){
    if(ignore_input_not_found_error == TRUE){
      message(paste0("Input option value not found: ", stop_at_the_end))
    } else {
      stop(paste0("Input option value not found: ", stop_at_the_end))
    }
  }
}

foGetCacheValues <- function(){
  out = list()
  for(iX in 1:length(cache)){
    name = names(cache[iX])
    out[[name]] = foGetCacheValue(name)
  }
  return(out)
}

foRestoreFullCache <- function(values){
  # browser()
  for(iX in 1:length(cache)){
    name = names(cache[iX])
    value = values[[name]]
    if(is.null(value) || is.na(value)){
      next;
    }
    foRestoreCache(name, value)
  }
}

foGetCacheValue <- function(name){
  cache_variable = cache[[name]]
  if(!is.null(cache_variable)){
    cache[[name]]()
  } else {
    NULL
  }
}

foRestoreCache <- function(name, value){
  if(is.null(value)){
    value = NA
  }
  cache[[name]](value)
}

foReadURI <- function(session){
  query <- parseQueryString(session$clientData$url_search)
  query_s = paste(names(query), query, sep = "=", collapse=", ")
  token = query[["token"]]
  if(!is.null(token)){
    foRestoreConfiguration(token)
  }
}

reactive_inputset <- reactive({
  set = foInputList()
})

convertRaw2Str = function(x) paste(x,collapse = '')

convertStr2Raw = function(s){
  sst <- strsplit(s, "")[[1]]
  out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
  as.raw(as.hexmode((out)))
}

foGenerateToken <- function(){
  set = reactive_inputset()
  xs = toJSON(set, indent=0, method="C" )
  token = convertRaw2Str(memCompress(xs, "bzip2")) 
}

observeEvent(input$config_token,
             if(nchar(input$config_token) > 1){
               html("config_link_element", html=paste0("<a href='?token=", input$config_token, "'", ">Link</a>"))
             } else {
               html("config_link_element", html="")
             }
)

observeEvent(input$generate_token_button, {
  token = foGenerateToken()
  updateTextInput(session, "config_token", value = token)
  #writeClipboard(as.character(token))
  if(clipr_available()){
    write_clip(as.character(token))
    alert_txt = "Config token generated and Copied to clipboard"
  } else {
    alert_txt = "Config token generated"
  }
  show_alert(title = alert_txt, showCloseButton = F, type = "success", btn_labels = NA, timer = 1000, showConfirmButton = F)
})

observeEvent(input$upload_config, {
  foRestoreConfiguration("", fromtoken = F, file = input$upload_config)
})

foRestoreConfiguration <- function(token, fromtoken = T, 
                                   file = NULL, silent = F, 
                                   ignore_input_not_found_error = F){
  tryCatch({
    if(fromtoken){
      main_logging(sprintf("Configuration token: %s", token))
      xd = memDecompress(convertStr2Raw(token), "bzip2", asChar = T)
      xx = fromJSON(xd)
    } else {
      if(is.null(file)){
        stop("File is null")
      }
      ext = tools::file_ext(file$datapath)
      if(is.null(ext) || ext != "json"){
        stop("File extension error")
      }
      xx = fromJSON(file = file$datapath)
    }
    foRestoreConfig(xx, ignore_input_not_found_error = ignore_input_not_found_error)
    name = xx[["config_name"]] 
    if(!is.null(name)){
      #html("metadata_description_text", html=paste0("Configuration:", name))
      updateTextInput(session, "config_name", value = name)
      name = paste0("'", name, "'")
      extra_timer = 450
    } else {
      name = ""
      extra_timer = 0
    }
    main_logging("Configuration restored")
    foCollapseSubgroupBoxes()
    if(!silent){
      show_alert(title = paste0("Configuration ", name, " successfully restored."), showCloseButton = F, type = "success", btn_labels = NA, timer = 1000+extra_timer, showConfirmButton = F)
    }
  }, error = function(e){
    if(TRUE){
      show_alert(title = "An error occurred while restoring the config.", showCloseButton = F, type = "error", btn_labels = NA, timer = 1000, showConfirmButton = F)
    }
    message(sprintf("An error occurred while restoring the config: %s", as.character(e)))
    main_logging("Error while restoring configuration")
  }
  )
}

foCollapseSubgroupBoxes <- function(){
  cache_default = "1";
  
  uncollapse = F
  for(iX in 1:6){
    cache_name = paste0("cached_subgroup_select", iX);
    cache_value = foGetCacheValue(cache_name);
    if(!identical(cache_value, cache_default)){
      uncollapse = T
    }
  }
  
  filter_by_collapsed = !DEPLOYMENT_MODE_ENABLED
  if(uncollapse){
    foUncollapseBoxIfNeeeded("optionbox_filter_by_subgroup", collapse = F, nullval = filter_by_collapsed)
  }
  if(!identical("None", foGetCacheValue("cached_select_group_differences"))){
    foUncollapseBoxIfNeeeded("optionbox_subgroup_differences", collapse = F, nullval = T)
  }
}


observeEvent(input$restore_token_button, {
  token = input$config_token
  foRestoreConfiguration(token)
})

output$download_config <- downloadHandler(
  filename = function() { paste('config.json', sep='') },
  content = function(file) {
    jsonlite::write_json(reactive_inputset(), path = file, pretty = TRUE)
  }
)

