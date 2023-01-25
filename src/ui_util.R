multiChoicePicker <- function(id, label, choices, selected = choices[1], 
                              isInline = "T", multiple = F, max_opts = 2, 
                              max_opts_txt = "No more!", width = "fit", 
                              style = NULL, style_label = NULL, style_picker = NULL,
                              picker_inline = T) {
  picker_ui <- shinyWidgets::pickerInput(id, "", choices, selected = selected, 
                                         width = width, inline = picker_inline, 
                                         multiple = multiple,
                                         options = pickerOptions(
                                           maxOptions = max_opts,
                                           maxOptionsText = max_opts_txt
                                         ))
  if(!is.null(style_picker)){
    picker_ui$attribs$style = paste0(picker_ui$attribs$style, style_picker)
  }
  
  switch(isInline, 
         "T" = R <- tags$div(
           class = "inline-block", id = paste(id, "_div", sep = ""), 
           style = "justify-content: space-between;", 
           style = style,
           tags$b(label, style = style_label),
           picker_ui
         ),
         "F" = R <- tags$div(
           id = paste(id, "_div", sep = ""), 
           style = style,
           tags$b(label, style = style_label),
           #selectInput(id, label, choices, selected = selected, width = "auto")
           picker_ui
         )
  )
  return (R)
}

collapseInput <- function(inputId, boxId) {
  tags$script(
    sprintf(
      "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('%s', true);})",
      boxId, inputId
    ),
    sprintf(
      "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('%s', false);})",
      boxId, inputId
    )
  )
}

optionBox <- function(..., title = "", status = "primary", id = "", collapsed = T){
  colid = paste0(id, "_collapse")
  tags$div(
    id = paste0(id, "_wrapper"), 
    box(id = id, status = status, width = NULL, collapsible = T, title = title, solidHeader = T, collapsed = collapsed, 
        ...
    ),
    collapseInput(colid, boxId = id)
  )
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

on_start_collapse <- paste(
  "$(function() {",
  "$(document).on('shiny:connected', function(e) {",
  "shinyjs.collapse('optionbox_filter_by_subgroup');
   shinyjs.collapse('optionbox_subgroup_differences');",
  "});",
  "",
  "});",
  sep = "\n"
)

on_start_hide_contact <- paste(
  "$(function() {",
  "$(document).on('shiny:connected', function(e) {",
  "$('#aboutTabset li a[data-value=\"Contact\"]').hide();",
  "});",
  "",
  "});",
  sep = "\n"
)


jscode_collapse  <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

RokaiXplorer_banner <- tags$div(
  class = "panel panel-default",
  style = "margin:0px; margin-bottom:5px;",
  tags$div(
    class = "panel-body",
    style = "padding-bottom:10px; padding-top:10px; margin:0px;",
    sprintf("This application is made using RokaiXplorer %s. To access it and read instructions on how to deploy RokaiXplorer on other data, please visit: ", version_text()),
    tags$a("http://explorer.rokai.io", href="http://explorer.rokai.io"),
  )
)
