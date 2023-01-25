cache$cached_subgroup_select1 = reactiveVal("1")
cache$cached_subgroup_select2 = reactiveVal("1")
cache$cached_subgroup_select3 = reactiveVal("1")
cache$cached_subgroup_select4 = reactiveVal("1")
cache$cached_subgroup_select5 = reactiveVal("1")
cache$cached_subgroup_select6 = reactiveVal("1")

foSubgroupSelectInput <- function(i, T_metadata) {
  q <- T_metadata[i,]
  rowname <- rownames(q)
  values <- unique(as.character(q))
  values = sort(values, decreasing =F)
  li <- list()
  li$All <- 1
  for (j in 1:length(values)) {
    li[[values[j]]] <- j + 1
  }
  
  id = paste("subgroup_select", i, sep="")
  cache_id = paste("cached", id, sep = "_")
  selected = fo_restore_if_applicable(li, isolate(foGetCacheValue(cache_id)))
  
  tags$div(
    style = "margin-bottom: 0px;",
    selectInput(id , rowname[1], 
                choices = li, 
                selected = selected, selectize = F, width = 170)  
  )
}

output$subgroup_controls <- renderUI({
  if(DEPLOYMENT_MODE_ENABLED){
    metadata_not_ready_text = "Loading..."
  } else {
    metadata_not_ready_text = ""
  }
  validate(
    need(metadata_ready(), metadata_not_ready_text)
  )
  x <- current_metadata()
  # tags$div(
  #     style = "margin-top: 8px; ", 
  #     tags$b("Select Subgroup: "), 
  #     tags$div(
  #         style = "padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
  #         
  #     )
  # )
  if(nrow(x$Tsample_metadata) > 0){
    rdiv <- tags$div(
      lapply(1:nrow(x$Tsample_metadata), 
             function(i) {foSubgroupSelectInput(i, x$Tsample_metadata)})
    )
    return(rdiv)
  }
  return(tags$div())
})


subgroup_samples <- reactive({
  req(current_metadata())
  x <- current_metadata()
  validSamples = rep(TRUE, x$nSample)
  if(nrow(x$Tsample_metadata) > 0){
    for (i in 1:nrow(x$Tsample_metadata)){
      q <- input[[paste("subgroup_select", i, sep="")]]
      #if(is.null(q)){ validate(need(FALSE, "")); }
      if(is.null(q)){next}
      qv <- as.numeric(q)
      if(qv > 1){
        subgroups = x$Tsample_metadata[i,]
        values <- unique(as.character(subgroups))
        values = sort(values, decreasing =F)
        validSamples = validSamples & (subgroups == values[qv - 1])
      }
    }
  }
  return(validSamples)
})