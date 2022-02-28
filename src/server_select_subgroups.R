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
  
  tags$div(
    style = "margin-bottom: 0px;",
    selectInput(paste("subgroup_select", i, sep="") , rowname[1], 
                choices = li, 
                selected = 1, selectize = F, width = 170)  
  )
}

output$subgroup_controls <- renderUI({
  validate(
    need(metadata_ready(), "")
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
      if(is.null(q)){ validate(need(FALSE, "")); }
      qv <- as.numeric(q)
      if(qv > 1){
        subgroups = x$Tsample_metadata[i,]
        values <- unique(as.character(subgroups))
        values = sort(values, decreasing =F)
        #message((subgroups == values[qv - 1]))
        validSamples = validSamples & (subgroups == values[qv - 1])
      }
    }
  }
  return(validSamples)
})