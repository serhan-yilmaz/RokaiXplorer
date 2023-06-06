modal_box_selection <- reactive({
  current_selection <- abcd()
  parts <- str_split(current_selection, "_")
  valid <- (length(parts) > 0) & (length(parts[[1]]) > 1)
  validate(need(valid, "Item category is missing."))
  identifier = parts[[1]][[1]]
  category = parts[[1]][[2]]
  isGOTerm = (category == "GOTerm")
  isSite = (category == "Site") || (category == "Phosphosite")
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
              "isGOTerm"= isGOTerm, 
              "site" = site))
})

modal_box_selection_mapped <- reactive({
  ds <- modal_box_selection()
  
  ds$isMapped = FALSE
  if(ds$isGOTerm){
    NetworkData <- reactive_network()
    ds$index = match(ds$identifier, NetworkData$GO$ID)
    ds$table = NetworkData$GO[ds$index, ]
    ds$isMapped = !is.na(ds$index)
  }
  if(ds$isKinase){
    NetworkData <- reactive_network()
    ds$index = match(ds$identifier, NetworkData$Kinase$KinaseName)
    ds$table = NetworkData$Kinase[ds$index, ]
    ds$isMapped = !is.na(ds$index)
  }
  if(ds$isProtein){
    PT <- protein_table_processed()
    ds$index = match(ds$identifier, PT$Identifier)
    ds$table = PT[ds$index, ]
    ds$isMapped = !is.na(ds$index)
    NetworkData <- reactive_network()
    if(ds$isMapped){
      ds$kinindex = match(ds$table$ID, NetworkData$Kinase$KinaseID)
    } else {
      ds$kinindex = match(ds$identifier, NetworkData$Kinase$Gene)
    }
    ds$kintable = NetworkData$Kinase[ds$kinindex, ]
    ds$isKinaseMapped = !is.na(ds$kinindex)
  }
  if(ds$isSite){
    ST <- site_table_processed()
    ds$index = match(ds$identifier, ST$Identifier)
    ds$table = ST[ds$index, ]
    ds$isMapped = !is.na(ds$index)
  }
  
  return(ds)
})

output$abcd_title <- renderUI({
  ds <- modal_box_selection()
  title = ds$main_identifier
  if(ds$isGOTerm){
    ds <- modal_box_selection_mapped()
    cat = ds$table$Namespace
    if(!is.null(cat) && !is.na(cat) && !is.empty(cat)){
      cat <- foGetGoCategoryText(cat)
      title = paste(ds$identifier, cat, sep = "_")
      link_main = "https://www.ebi.ac.uk/QuickGO/term/"
      title = tags$a(href = paste0(link_main, ds$identifier), title, target = "_blank")
    }
  }
  if(ds$isKinase){
    ds <- modal_box_selection_mapped()
    if(ds$isMapped){
      # message(sprintf("Mapped: %s", ds$identifier))
      # kinname <- ds$identifier
      identifier <- ds$table$Gene
      # message(sprintf("Gene Identifier: %s", identifier))
      NetworkData <- reactive_network()
      index <- match(identifier, NetworkData$UniprotGene$Gene)
      # message(paste0("Index: "), index)
      # message(paste0("IsTableNull: ", is.null(NetworkData$UniprotGene$Gene)))
      if(!is.null(index) && !is.na(index)){
        # message(sprintf("Mapped and not null: %s", ds$identifier))
        Tx <- NetworkData$UniprotGene[index, ]
        if(Tx$FromUniprot){
          prot_id = Tx$ID
          # link_main = "https://www.uniprot.org/uniprotkb/"
          # link_extra = "/entry"
          link_main = "https://www.uniprot.org/uniprotkb/"
          link_extra = "/entry#ptm_processing"
          title = tags$a(href = paste0(link_main, prot_id, link_extra), title, target = "_blank")
        }
      }
    }
    
  }
  
  if(ds$isProtein){
    identifier <- ds$identifier
    NetworkData <- reactive_network()
    index <- match(identifier, NetworkData$UniprotGene$Gene)
    if(!is.null(index) && !is.na(index)){
       Tx <- NetworkData$UniprotGene[index, ]
       if(Tx$FromUniprot){
         prot_id = Tx$ID
         link_main = "https://www.uniprot.org/uniprotkb/"
         link_extra = "/entry"
         title = tags$a(href = paste0(link_main, prot_id, link_extra), title, target = "_blank")
       }
    }
  }
  if(ds$isSite){
    identifier <- ds$protein
    NetworkData <- reactive_network()
    index <- match(identifier, NetworkData$UniprotGene$Gene)
    ds <- modal_box_selection_mapped()
    inRef <- ds$table$InRef
    if(is.null(inRef) || !ds$isMapped){
      inRef = FALSE
    }
    if(!is.null(index) && !is.na(index) && inRef){
      Tx <- NetworkData$UniprotGene[index, ]
      if(Tx$FromUniprot){
        prot_id = Tx$ID
        link_main = "https://www.phosphosite.org/uniprotAccAction?id="
        # link_main = "https://www.phosphosite.org/uniprotAccAction?id="
        link_extra = ""
        title = tags$a(href = paste0(link_main, prot_id, link_extra), title, target = "_blank")
      }
    }
  }
  
  tags$div(
    style = "display: flex; justify-content: space-between;", 
    title, 
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
  
  if(ds$isProtein){
    if(ds$isKinaseMapped){
      kinase = as.character(ds$kintable$KinaseName)
      kinase = paste0(kinase, "_", "Kinase")
      callback <- sprintf("Shiny.setInputValue('site_kinase_network_doubleclickb', '%s', {priority: 'event'});", kinase)
      protein_button =  tags$div(style = "display: inline-block;",
                                 tags$button(id = "modal_nav_protein_select_button", kinase, onclick = callback))
    }
  }
  
  return(protein_button)
})

cache_locked = reactiveVal(FALSE)

cache$cached_mbox_main_select_group = reactiveVal("")
cache$cached_mbox_main_casecontrol = reactiveVal("")
cache$cached_mbox_main_plotstyle = reactiveVal("")
cache$cached_mbox_main_datasource = reactiveVal("")
cache$cached_mbox_main_normgroup = reactiveVal(TRUE)
cache$cached_mbox_main_showsamples = reactiveVal(TRUE)

cache$cached_mbox_protein_tab <- reactiveVal("")

model_update_toggle <- reactiveVal(FALSE)

output$modal_box_site_plot_controls <- renderUI({
	modal_box_site_plot_controls_reactive()
})


modal_box_site_plot_controls_reactive <- reactive({
  a <- model_update_toggle()
  validate(
    need(metadata_ready(), "")
  )
  req(modal_box_selection())
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  casecontrol_opts <- c("Case samples", "Control samples","Both case and control")
  plotstyle_opts <- c("Bar plot", "Box plot")
  #casecontrol_opts <- c("Case samples", "Control samples","Both case and control")
  selected_grouping = fo_restore_if_applicable(groups, isolate(cache$cached_mbox_main_select_group()))
  selected_casecontrol = fo_restore_if_applicable(casecontrol_opts, isolate(cache$cached_mbox_main_casecontrol()))
  selected_plotstyle = fo_restore_if_applicable(plotstyle_opts, isolate(cache$cached_mbox_main_plotstyle()))
  selected_normgroup = fo_restore_if_applicable(c(F, T), isolate(cache$cached_mbox_main_normgroup()))
  selected_showsamples = fo_restore_if_applicable(c(F, T), isolate(cache$cached_mbox_main_showsamples()))
  
  # select_subgroup_opts = list()
  # 
  # # Group1 = c("1", "2", "3", "4"),
  # # Group2 = c("A", "B", "C", "D")
  # all = c()
  # if(length(groups) >= 1){
  #   for(iGroup in 1:length(groups)){
  #     values <- unique(as.character(x$Tsample_metadata[iGroup,]))
  #     values = sort(values, decreasing =F)
  #     select_subgroup_opts[[groups[iGroup]]] = values
  #     all = c(all, values)
  #   }
  # }

  selection <- modal_box_selection()
  
  if(selection$isProtein){
    datasource_opts = c("Phosphorylation", "Protein Expression")
    selected_datasource = fo_restore_if_applicable(datasource_opts, isolate(cache$cached_mbox_main_datasource()))
    data_source_div <- 
      multiChoicePicker("mbox_site_plot_datasource", "Data Source:", datasource_opts, 
                      selected = selected_datasource,
                      isInline = "F", width = 175, 
                      style_picker = "display:inline-flex; margin-top:4px;", 
                      picker_inline = F)
  } else {
    data_source_div <- ""
  }
  
  if(!selection$isGOTerm){
    casecontrol_div <- multiChoicePicker("mbox_site_plot_samples_case_control", "Show:", casecontrol_opts, 
                      selected = selected_casecontrol,
                      isInline = "F", width = 175, 
                      style_picker = "display:inline-flex; margin-top:4px;", 
                      picker_inline = F)
  } else {
    casecontrol_div <- tags$div()
  }
  
   plotstyle_div <- multiChoicePicker("mbox_site_plotstyle", "Plot Style:", plotstyle_opts, 
                                       selected = selected_plotstyle,
                                       isInline = "F", width = 175, 
                                       style_picker = "display:inline-flex; margin-top:4px;", 
                                       picker_inline = F)
  
  tags$div(
    fluidRow(
    column(width = 4, 
           tags$div(style = "width: 180px; margin: 4px;", 
                    
                    data_source_div, 
    multiChoicePicker("mbox_site_plot_select_group", "Grouping:", groups, 
                      selected = selected_grouping, 
                      isInline = "F", multiple = T, max_opts = 2, width = 170, 
                      style_picker = "display:inline-flex; margin-top:4px;", 
                      picker_inline = F), 
    tags$div(
      style = "margin-top: 8px;", 
      tags$b("Normalize within group:"), 
      shinyWidgets::materialSwitch(inputId = "mbox_site_plot_norm_by_group", label = "", status = "primary", value = selected_normgroup, inline = T)
    )
               ), 
    ),
    column(width = 4,
           tags$div(style = "width: 180px; margin: 4px;", 
                    plotstyle_div,
                    casecontrol_div, 
    tags$div(
      style = "margin-top: 8px;", 
      tags$b("X-axis labels:"), 
      shinyWidgets::materialSwitch(inputId = "mbox_site_plot_show_samples", label = "", status = "primary", value = selected_showsamples, inline = T)
    )
           )
    ), 
    column(width = 3, 
      style = "padding: 8px; padding-top: 12px;", 
      tags$div(
        downloadButton(paste("modalbox_barplot", "downloadPlotPNG", sep = "_"), 'Download PNG', style = "margin-top:4px;"),
        tags$br(), 
        downloadButton(paste("modalbox_barplot", "downloadPlotPDF", sep = "_"), 'Download PDF', style = "margin-top:4px;"),
        tags$br(), 
        downloadButton(paste("modalbox_barplot", "downloadPlotDataExcel", sep = "_"), 'Plot Data (Excel)', style = "margin-top:4px;")
      )
    )
    
    )
  )
})

observeEvent(input$mbox_site_plot_select_group, {
  if(cache_locked()){return()}
  cache$cached_mbox_main_select_group(input$mbox_site_plot_select_group)
  message(paste("Updated cache:", cache$cached_mbox_main_select_group()))
})

observeEvent(input$mbox_site_plot_samples_case_control, {
  if(cache_locked()){return()}
  cache$cached_mbox_main_casecontrol(input$mbox_site_plot_samples_case_control)
})

observeEvent(input$mbox_site_plotstyle, {
  if(cache_locked()){return()}
  cache$cached_mbox_main_plotstyle(input$mbox_site_plotstyle)
})

observeEvent(input$mbox_site_plot_norm_by_group, {
  if(cache_locked()){return()}
  cache$cached_mbox_main_normgroup(input$mbox_site_plot_norm_by_group)
})


###############

# observeEvent(input$modal_box_protein_tab, {
#   #if(cache_locked()){return()}
#   cache$cached_mbox_protein_tab(input$modal_box_protein_tab)
#   message(sprintf("Cache Updated 'cached_mbox_protein_tab' - %s", cache$cached_mbox_protein_tab()))
# })



output$modal_goterm_parents_chart <- renderUI({
  req(modal_box_selection_mapped())
  ds <- modal_box_selection_mapped()
  validate(need(ds$isMapped && ds$isGOTerm, ""))
  
  name = tools::toTitleCase(ds$table$Name)
  def = ds$table$Definition
  go_id = gsub("GO:", "", ds$identifier);
  main_link = "https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/%7Bids%7D/chart?ids=GO%3A"
  requestURL <- paste0(main_link, go_id);
  # requestURL <- "https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/%7Bids%7D/chart?ids=GO%3A0030170";
  
  style_ = "text-align:justify;"
  tags$div(
    tags$div(style = style_, tags$b("Name: "), name),
    tags$div(style = style_, tags$b("Definition: "), def), 
    tags$div(
      style = "text-align:center;", 
      tags$img(src = requestURL, height = 360)
    )
  )
})

is_protein_modalbox_plot_showing_expression <- reactive({
  val <- input$mbox_site_plot_datasource
  if(is.null(val)){
    return(TRUE)
  }
  if(identical(val, "Protein Expression")){
    return(TRUE)
  }
  return(FALSE)
})

output$modal_box_plot_showing_text <- renderUI({
  validate(
    need(modal_box_selection(), "")
  )
  ds <- modal_box_selection()
  showing_text = ""
  if(ds$isKinase){
    showing_text = "inferred kinase activities"
  }
  if(ds$isProtein){
    if(is_protein_modalbox_plot_showing_expression()){
      showing_text = "protein expression"
    } else {
      showing_text = "mean phosphorylation"
    }
  }
  if(ds$isSite){
    showing_text = "phosphorylation"
  }
  
  if(ds$isGOTerm){
    showing_text = "enrichment of "
    switch(input$enrichment_datasource, 
           "Phosphosites" = {
             showing_text = paste0(showing_text, "phosphosites")
           },
           "Phosphoproteins" = {
             showing_text = paste0(showing_text, "phosphoproteins")
           },
           "Protein Expression" = {
             showing_text = paste0(showing_text, "protein expression")
           }, 
           stop("Invalid data source for enrichment")
    )
  }
  
  tags$div(
    style = "margin-bottom:4px; margin-top:4px; color: #444;", 
    sprintf("Showing the %s:", showing_text),
    # tags$img(src = requestURL, width = 400)
  )
})

output$abcd_ui <- renderUI({
  validate(
    need(modal_box_selection(), "")
  )
  ds <- modal_box_selection()
  plot_height = "365px";
  
  mainDiv <- tags$div(
    shinycssloaders::withSpinner(DT::dataTableOutput("proteinTable_test"))
  )
  
  if(ds$isGOTerm){
    mainDiv <- tags$div(
      style = "min-height:200px;", 
      tabsetPanel(id = "modal_box_kinase_tab",
                  tabPanel("Overview", 
                           mainDiv <- tags$div(
                             # uiOutput("modal_box_plot_showing_text"), 
                             uiOutput("modal_goterm_parents_chart"), 
                             # uiOutput("modal_box_site_plot_controls")
                           )
                  ), 
                  tabPanel("Plot", 
                           tags$div(
                             uiOutput("modal_box_plot_showing_text"),
                             shinycssloaders::withSpinner(plotOutput("modal_goenrichment_samplewise_barplot", height = plot_height)), 
                             uiOutput("modal_box_site_plot_controls")
                           )
                  ), 
                  tabPanel("Related Proteins", 
                           shinycssloaders::withSpinner(DT::dataTableOutput("modal_goterm_related_proteins_table"))
                  )
      )
    )

  }
  
  if(ds$isKinase){
    mainDiv <- tags$div(
      style = "min-height:200px;", 
      tabsetPanel(id = "modal_box_kinase_tab",
        tabPanel("Overview", 
                 tags$div(
                   uiOutput("modal_box_plot_showing_text"), 
                   shinycssloaders::withSpinner(plotOutput("modal_kinase_samplewise_barplot", height = plot_height)), 
                   uiOutput("modal_box_site_plot_controls")
                 )
        ), 
        tabPanel("Known Targets", 
                 shinycssloaders::withSpinner(DT::dataTableOutput("modal_kinase_sites_table"))
        )
      )
    )
  }
  if(ds$isProtein){
    # selected = fo_restore_if_applicable(c("Overview", "Phosphosites", "GO Terms"), isolate(cache$cached_mbox_protein_tab()))
    mainDiv <- tags$div(
      style = "min-height:200px;", 
      tabsetPanel(id = "modal_box_protein_tab", 
                  tabPanel("Overview", 
                           tags$div(
                             uiOutput("modal_box_plot_showing_text"), 
                             shinycssloaders::withSpinner(plotOutput("modal_protein_samplewise_barplot", height = plot_height)), 
                             uiOutput("modal_box_site_plot_controls")
                           )
                  ), 
                  tabPanel("Phosphosites", 
                           shinycssloaders::withSpinner(DT::dataTableOutput("modal_protein_sites_table"))
                  ), 
                  tabPanel("GO Terms", 
                           shinycssloaders::withSpinner(DT::dataTableOutput("modal_protein_goterms_table"))
                  ), 
                  selected = "Overview"
      )
    )
  }
  if(ds$isSite){
    mainDiv <- tags$div(
      uiOutput("modal_box_plot_showing_text"), 
      shinycssloaders::withSpinner(plotOutput("modal_site_samplewise_barplot", height = plot_height)), 
      uiOutput("modal_box_site_plot_controls")
    )
  }
  
  tags$div(
    #uiOutput("modal_box_nav_protein_button", inline = T), 
    #tags$hr(), 
    mainDiv
  )
})

output$abcd_footer <- renderUI({
  # delay(10, isolate(foUpdateBackButtonBasedOnHistory()))
  if(modalbox_history_length() >= 1){
    backbutton = actionButton("modal_back_button", "Back", style = "float:left;")
  } else {
    backbutton = ""
  }
  tags$div(
    backbutton, 
    modalButton("Close")
  )
})

## Rename these
abcd <- reactiveVal("")

modalX <- reactive({
modalDialog(
    uiOutput("abcd_ui"),
    title = uiOutput("abcd_title"),
    footer = uiOutput("abcd_footer"),
    size = "m",
    easyClose = TRUE
  )
})

modalbox_history <- reactiveVal(c())
modalbox_history_length <- reactiveVal(-1)

observeEvent(input$site_kinase_network_doubleclick, {
  cache_locked(TRUE)
  model_update_toggle(!model_update_toggle())
  modalbox_history(c())
  modalbox_history_length(0)
  abcd(input$site_kinase_network_doubleclick)
  b = modal_box_site_plot_controls_reactive()
  delay(50, showModal(modalX()))
  delay(200, cache_locked(FALSE))
})

observeEvent(input$site_kinase_network_doubleclickb, {
  previous = abcd()
  current = input$site_kinase_network_doubleclickb
  
  m_len = modalbox_history_length()
  modalbox_history_length(max(m_len, 0) + 1)
  
  m_hist <- modalbox_history()
  m_hist[modalbox_history_length()] <- previous
  
  if(modalbox_history_length() >= 100){
    m_hist = m_hist[51:modalbox_history_length()]
    modalbox_history_length() = modalbox_history_length() - 1
  }
  modalbox_history(m_hist)
  abcd(current)
})

observeEvent(input$modal_back_button, {
  m_len = modalbox_history_length()
  if(m_len >= 1){
    abcd(modalbox_history()[m_len])
    modalbox_history_length(m_len - 1)
  }
})

foUpdateBackButtonBasedOnHistory <- function(){
  if(modalbox_history_length() >= 1){
    shinyjs::enable("modal_back_button")
  } else {
    shinyjs::disable("modal_back_button")
  }
}

observeEvent(modalbox_history_length(), {
  foUpdateBackButtonBasedOnHistory()
}, ignoreNULL = FALSE)




