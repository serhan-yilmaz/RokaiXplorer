multiChoicePicker <- function(id, label, choices, selected = choices[1], isInline = "T", multiple = F, max_opts = 2, max_opts_txt = "No more!", width = "fit") {
  picker_ui <- shinyWidgets::pickerInput(id, "", choices, selected = selected, 
                                         width = width, inline = T, 
                                         multiple = multiple,
                                         options =  list(
                                           "max-options" = max_opts,
                                           "max-options-text" = max_opts_txt
                                         ))
  switch(isInline, 
         "T" = R <- tags$div(
           class = "inline-block", id = paste(id, "_div", sep = ""), 
           style = "justify-content: space-between;", 
           tags$b(label),
           picker_ui
         ),
         "F" = R <- tags$div(
           id = paste(id, "_div", sep = ""), 
           tags$b(label),
           #selectInput(id, label, choices, selected = selected, width = "auto")
           picker_ui
         )
  )
  return (R)
}

foAddHelper <- function(el, helper_id = "", tooltip = "", helper_file = NA){
  if(is.na(helper_file)){
    helper_content = ""
    helper_type = "inline"
  } else {
    helper_content = helper_file
    helper_type = "markdown"
  }
  tooltip_txt = paste("<span style='font-size:14px; margin: 0px;'>", tooltip, "<span>")
  tags$div(
    helper(el, id = helper_id, type = helper_type, content = helper_content),
    tippy_this(helper_id, tooltip_txt, allowHTML = TRUE), 
  )
}

foMaxItemsHelper <- function(el, base_id){
  maxitems_helper_id = paste(base_id, "maxitems_helper", sep = "_")
  maxitems_tooltip = "Click to learn how top items are selected."
  maxitems_helper_file = "how_top_items_selected"
  foAddHelper(el, maxitems_helper_id, maxitems_tooltip, maxitems_helper_file)
}

foMinSamplewiseMagnitudeHelper <- function(el, base_id){
  maxitems_helper_id = paste(base_id, "samplewise_magnitude_helper", sep = "_")
  maxitems_tooltip = "Click to learn about the metric."
  maxitems_helper_file = "min_samplewise_magnitude"
  foAddHelper(el, maxitems_helper_id, maxitems_tooltip, maxitems_helper_file)
}

foList <- function(...){
  x <- list(...)
  outList <- list()
  previous = NULL
  for(i in seq(1, length(x), 1)){
    if((i %% 2) == 0){
      outList[[previous]] <- x[[i]]
    }
    previous = x[[i]]
  }
  return(outList)
}

on_ready <- paste(
  "$(function() {",
  "$(document).on('shiny:connected', function(e) {",
  "Shiny.setInputValue('initialized', 1);",
  "});",
  "",
  "});",
  sep = "\n"
)
