cache$cached_select_group_differences = reactiveVal("None")

output$group_difference_controls <- renderUI({
  validate(
    need(metadata_ready(), "")
  )
  x <- current_metadata()
  groups <- rownames(x$Tsample_metadata)
  choices = c("None", groups)
  
  selected = fo_restore_if_applicable(choices, isolate(foGetCacheValue("cached_select_group_differences")))
  
  # tags$div(
  #     style = "margin-top: 8px; ", 
  #     tags$b("Investigate Group Differences: "), 
  #     tags$div(
  #         style = "padding-left: 8px; padding-top: 6px; margin-top: 4px; padding-bottom: 4px; border-style: inset;", 
  #         selectInput("select_group_differences" , "Select Group:", 
  #                     choices = c("None", groups), 
  #                     selected = 1, selectize = F, width = 170),
  #         uiOutput("select_subgroups_for_difference"),
  #     )
  # )
  tags$div(
    selectInput("select_group_differences" , "Select Group:", 
                choices = choices, 
                selected = selected, selectize = F, width = 170),
    uiOutput("select_subgroups_for_difference")
  )
})

cache$cached_select_subgroup_A = reactiveVal("")
cache$cached_select_subgroup_B = reactiveVal("")

output$select_subgroups_for_difference <- renderUI({
  validate(
    need(metadata_ready(), ""),
    need(!is.null(input$select_group_differences), ""),
    need(!is.na(input$select_group_differences), ""),
    need(!(input$select_group_differences == "None"), ""),
  )
  selected_group <- as.character(input$select_group_differences)
  if(selected_group != "None"){
    x <- current_metadata()
    groups <- rownames(x$Tsample_metadata)
    possible_choices <- unique(as.character(x$Tsample_metadata[selected_group, ]))
  } else {
    possible_choices = "Select a group first."
  }
  selected = fo_restore_if_applicable(possible_choices, isolate(foGetCacheValue("cached_select_subgroup_A")))
  selected_two = fo_restore_if_applicable(possible_choices, isolate(foGetCacheValue("cached_select_subgroup_B")))
  if(identical(selected, selected_two)){
    selected_two = possible_choices[min(2, length(possible_choices))]
  }
  # selected_two = possible_choices[min(2, length(possible_choices))]
  tags$div(
    tags$div(style="display:inline-block;", 
             selectInput("select_subgroup_A" , "A:", 
                         choices = possible_choices, 
                         selected = selected, selectize = F, width = 100)
    ),
    tags$div(style="display:inline-block;", 
             selectInput("select_subgroup_B" , "B:", 
                         choices = possible_choices, 
                         selected = selected_two, selectize = F, width = 100)
    )
  )
})

observeEvent(c(input$select_group_differences, input$select_subgroup_A, input$select_subgroup_B), {
  q <- input$select_group_differences
  if(is.null(q) | (as.character(q) == "None")){
    analyze_group_differences(FALSE)
    return();
  }
  qA <- input$select_subgroup_A
  qA <- input$select_subgroup_A
  qB <- input$select_subgroup_B
  if(is.null(qA) | is.null(qB)){
    analyze_group_differences(FALSE)
    return();
  }
  analyze_group_differences(TRUE)
})

selected_group_differences <- reactive({
  req(metadata_ready())
  x <- current_metadata()
  defaultOutput <- list(investigateGroupDifferencesEnabled = FALSE)
  q <- input$select_group_differences
  if(is.null(q) | (as.character(q) == "None")){
    validate(need(FALSE, ""))
    return(defaultOutput)
  }
  qA <- input$select_subgroup_A
  qB <- input$select_subgroup_B
  if(is.null(qA) | is.null(qB)){
    validate(need(FALSE, ""))
    return(defaultOutput)
  }
  selected_group = as.character(x$Tsample_metadata[as.character(q), ])
  selected_A <- as.character(qA)
  selected_B <- as.character(qB)
  samplesA <- selected_group == selected_A
  samplesB <- selected_group == selected_B
  
  validate(
    need(selected_A != selected_B, "Selected subgroups A and B should be different.")
  )
  
  return(list(investigateGroupDifferencesEnabled = TRUE, 
              samplesA = samplesA, samplesB = samplesB))
})