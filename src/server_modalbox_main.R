modal_box_selection <- reactive({
  current_selection <- abcd()
  parts <- str_split(current_selection, "_")
  valid <- (length(parts) > 0) & (length(parts[[1]]) > 1)
  validate(need(valid, "Item category is missing."))
  identifier = parts[[1]][[1]]
  category = parts[[1]][[2]]
  isSite = category == "Site"
  isProtein = category == "Protein"
  isKinase = category == "Kinase"
  if(isSite){
    siteidentifier = parts[[1]][[1]]
    site_parts <- str_split(siteidentifier, "-")
    protein = site_parts[[1]][[1]]
    site = site_parts[[1]][[2]]
  } else {
    protein = ""
    site = ""
  }
  return(list("main_identifier" = current_selection, 
              "identifier" = identifier, "category" = category, 
              "isSite" = isSite, "isProtein" = isProtein, 
              "isKinase" = isKinase, "protein" = protein, 
              "site" = site))
})

modal_box_selection_mapped <- reactive({
  ds <- modal_box_selection()
  
  ds$isMapped = FALSE
  if(ds$isKinase){
    net <- reactive_network()
    ds$index = match(ds$identifier, NetworkData$Kinase$KinaseName)
    ds$table = NetworkData$Kinase[ds$index, ]
    ds$isMapped = !is.na(ds$index)
  }
  if(ds$isProtein){
    PT <- protein_table_processed()
    ds$index = match(ds$identifier, PT$Identifier)
    ds$table = PT[ds$index, ]
    ds$isMapped = !is.na(ds$index)
  }
  if(ds$isSite){
    ST <- protein_table_processed()
    ds$index = match(ds$identifier, ST$Identifier)
    ds$table = ST[ds$index, ]
    ds$isMapped = !is.na(ds$index)
  }
  
  return(ds)
})

output$abcd_title <- renderUI({
  ds <- modal_box_selection()
  tags$div(
    style = "display: flex; justify-content: space-between;", 
    ds$main_identifier, 
    uiOutput("modal_box_nav_protein_button", inline = T), 
  )
})

output$modal_box_nav_protein_button <- renderUI({
  ds <- modal_box_selection_mapped()
  protein_button = tags$div(style = "width:0px;")
  if(ds$isSite){
    protein = paste(ds$protein, "_", "Protein", sep = "")
    callback <- sprintf("Shiny.setInputValue('site_kinase_network_doubleclickb', '%s', {priority: 'event'});", protein)
    protein_button =  tags$div(style = "display: inline-block;", 
                               tags$button(id = "modal_nav_protein_select_button", protein, onclick = callback))
    
  }
  if(ds$isKinase & ds$isMapped){
    protein = as.character(ds$table$Gene)
    protein = paste(protein, "_", "Protein", sep = "")
    callback <- sprintf("Shiny.setInputValue('site_kinase_network_doubleclickb', '%s', {priority: 'event'});", protein)
    protein_button =  tags$div(style = "display: inline-block;", 
                               tags$button(id = "modal_nav_protein_select_button", protein, onclick = callback))
  }
  
  return(protein_button)
})


fo_restore_if_applicable <- function(groups, var){
  if(sum(is.na(match(var, groups))) == 0){
    return(var)
  }
  if(length(groups) >= 1){
    return(groups[1])
  }
  return("")
}

output$modal_box_site_plot_controls <- renderUI({
  validate(
    need(metadata_ready(), "")
  )
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  selected = fo_restore_if_applicable(groups, cached_mbox_site_plot_select_group())
  tags$div(
    multiChoicePicker("mbox_site_plot_select_group", "Grouping:", groups, 
                      selected = selected, 
                      isInline = "F", multiple = T, max_opts = 2)
  )
})

cached_mbox_site_plot_select_group <- reactiveVal("")

observeEvent(input$mbox_site_plot_select_group, {
  cached_mbox_site_plot_select_group(input$mbox_site_plot_select_group)
})

cached_mbox_protein_tab <- reactiveVal("")

observeEvent(input$modal_box_protein_tab, {
  cached_mbox_protein_tab(input$modal_box_protein_tab)
})

output$abcd_ui <- renderUI({
  validate(
    need(modal_box_selection(), "")
  )
  ds <- modal_box_selection()
  
  mainDiv <- tags$div(
    shinycssloaders::withSpinner(DT::dataTableOutput("proteinTable_test"))
  )
  if(ds$isKinase){
    mainDiv <- tags$div(
      style = "min-height:200px;", 
      #  style = "margin-top: 8px;", 
      #tags$b("Target Sites: "), 
      tabsetPanel(
        tabPanel("Known Targets", 
                 shinycssloaders::withSpinner(DT::dataTableOutput("modal_kinase_sites_table"))
        )
      )
    )
  }
  if(ds$isProtein){
    mainDiv <- tags$div(
      style = "min-height:200px;", 
      tabsetPanel(id = "modal_box_protein_tab", 
                  tabPanel("Overview", 
                           tags$div(
                             shinycssloaders::withSpinner(plotOutput("modal_protein_samplewise_barplot")), 
                             uiOutput("modal_box_site_plot_controls")
                           )
                  ), 
                  tabPanel("Sites", 
                           shinycssloaders::withSpinner(DT::dataTableOutput("modal_protein_sites_table"))
                  ), 
                  selected = fo_restore_if_applicable(c("Overview", "Sites"), cached_mbox_protein_tab())
      )
    )
  }
  if(ds$isSite){
    mainDiv <- tags$div(
      shinycssloaders::withSpinner(plotOutput("modal_site_samplewise_barplot")), 
      uiOutput("modal_box_site_plot_controls")
    )
  }
  
  tags$div(
    #uiOutput("modal_box_nav_protein_button", inline = T), 
    #tags$hr(), 
    mainDiv
  )
})

## Rename these
abcd <- reactiveVal("")

observeEvent(input$site_kinase_network_doubleclick, {
  #message(input$site_kinase_network_doubleclick)
  abcd(input$site_kinase_network_doubleclick)
  delay(50, showModal(modalDialog(
    uiOutput("abcd_ui"),
    title = uiOutput("abcd_title"),
    footer = modalButton("Close"),
    size = "m",
    easyClose = TRUE
  )))
})

observeEvent(input$site_kinase_network_doubleclickb, {
  abcd(input$site_kinase_network_doubleclickb)
})