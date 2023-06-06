tipify_customwidth <- function(el, tooltip = NULL, tooltip_width = NULL, identifier = NULL){
  out = el
  if(!is.null(tooltip)){
    out = tipify(out, tooltip)
    if(!is.null(tooltip_width) && !is.null(identifier)){
      tooltip_div = paste0(identifier, "_tooltipdiv")
      css = multigsub(c("ELEMENT_NAME", "TOOLTIP_WIDTH"), c(tooltip_div, tooltip_width), 
                      "#ELEMENT_NAME > .tooltip > .tooltip-inner {
          min-width: TOOLTIP_WIDTH;
        }"
      )
      out = tags$div(
        id = tooltip_div, 
        out, 
        tags$style(HTML(css))
      )
    }
  }
  return(out)
}

multiChoicePicker <- function(id, label, choices, selected = choices[1], 
                              isInline = "T", multiple = F, max_opts = 2, 
                              max_opts_txt = "No more!", width = "fit", 
                              style = NULL, style_label = NULL, style_picker = NULL,
                              style_choices = NULL, tooltip_width = NULL, 
                              picker_inline = T, class_names = NULL, tooltip = NULL) {
  
  if(!is.null(style_choices)){
    choicesOpt = list(style=c(rep(style_choices, length(choices)))) 
  } else {
    choicesOpt = NULL 
  }
  
  picker_ui <- shinyWidgets::pickerInput(id, "", choices, selected = selected, 
                                         width = width, inline = picker_inline, 
                                         multiple = multiple,
                                         choicesOpt = choicesOpt, 
                                         options = pickerOptions(
                                           maxOptions = max_opts,
                                           maxOptionsText = max_opts_txt
                                         ))
  if(!is.null(style_picker)){
    picker_ui$attribs$style = paste0(picker_ui$attribs$style, style_picker)
  }
  
  label_el = tags$b(label, style = style_label)
  label_el = tipify_customwidth(label_el, tooltip, tooltip_width, id)
  # if(!is.null(tooltip)){
  #   label_el = tipify(label_el, tooltip)
  # }
  
  switch(isInline, 
         "T" = R <- tags$div(
           class = "inline-block", 
           class = class_names,
           id = paste(id, "_div", sep = ""), 
           style = "justify-content: space-between;", 
           style = style,
           label_el, 
           picker_ui
         ),
         "F" = R <- tags$div(
           class = class_names, 
           id = paste(id, "_div", sep = ""), 
           style = style,
           label_el, 
           #selectInput(id, label, choices, selected = selected, width = "auto")
           picker_ui
         )
  )
  
  # if(!is.null(tooltip)){
  #   R = tipify(R, tooltip)
  # }
  
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

optionBox <- function(..., title = "", status = "primary", id = "", collapsed = T, collapsible = T){
  colid = paste0(id, "_collapse")
  tags$div(
    id = paste0(id, "_wrapper"), 
    box(id = id, status = status, width = NULL, collapsible = collapsible, title = title, solidHeader = T, collapsed = collapsed, 
        ...
    ),
    collapseInput(colid, boxId = id)
  )
}

fancyCheckbox <- function(identifier, label, default = F, status = "warning", tooltip = NULL, style = NULL, tooltip_width = NULL){
  out = tags$div(
    style = "margin-top: 8px;margin-bottom:8px;", 
    style = style, 
    tags$b(label), 
    shinyWidgets::materialSwitch(inputId = identifier, label = "", status = status, value = default, inline = T)
  )
  
  if(!is.null(tooltip)){
    out = tipify(out, tooltip)
    if(!is.null(tooltip_width)){
      tooltip_div = paste0(identifier, "_tooltipdiv")
      css = multigsub(c("ELEMENT_NAME", "TOOLTIP_WIDTH"), c(tooltip_div, tooltip_width), 
        "#ELEMENT_NAME > .tooltip > .tooltip-inner {
          min-width: TOOLTIP_WIDTH;
        }"
      )
      out = tags$div(
        id = tooltip_div, 
        out, 
        tags$style(HTML(css))
      )
    }
  }
  return(out)
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

asliderInput <- function(identifier, label, min, max, default, step = 1, post = NULL, tooltip = NULL, style_div = NULL, width = NULL){
  slider <- sliderInput(identifier, label, min, max, default, step = step, post = post, width = width)
  
  if(!is.null(tooltip)){
    out = tipify(slider, tooltip);
  } else {
    out = slider;
  }
  
  out = tags$div(style = "margin-top:4px;", style = style_div, out)
  
  return(out)
}

dropdown_options_alt <- function(content, titletxt = "Options", tooltip = "Click to see options.", width = NULL){
  tags$div(style = "max-width: 70px; margin-left: auto; vertical-align: top; right: 0%; top:0px;", 
           dropdown(
             size = "md", 
             icon = icon("cog"), #status = "info", 
             right = T, 
             up = F, 
             width = width, 
             tooltip = tooltipOptions(title = tooltip, placement = "top"), 
             tags$h4(style = "font-weight:bold; margin-bottom: 10px; white-space: nowrap;", titletxt), 
             content, 
             tags$p(style = "margin-bottom: 10px;", "")
           ))
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
    "This application is made using ",
    tags$a(sprintf("RokaiXplorer %s.", version_text()), href="http://explorer.rokai.io"),
    "For instructions on how to deploy RokaiXplorer on other data, please check out",
    tags$a("the Github page.", href="https://github.com/serhan-yilmaz/RokaiXplorer/tree/main/deploy"),
    # tags$a(sprintf("RokaiXplorer"), href="http://explorer.rokai.io"), 
    # sprintf("%s.", version_text()), 
    # sprintf("This application is made using RokaiXplorer %s. For instructions on how to deploy RokaiXplorer on other data, please check out ", version_text()),
    # tags$a("http://explorer.rokai.io", href="http://explorer.rokai.io"),
  )
)

generate_scenario_area <- function(boxes, nRow = 3){
  outer = tags$div();
  
  iRow = 1;
  iCol = 0;
  cur_row = fluidRow();
  for(i in 1:length(boxes)){
    iCol = iCol + 1;
    box = boxes[[i]]
    
    cur_row$children[[iCol]] = column(12/nRow, box);
    # cur_row$children[[iCol]] = tags$div(class = "col", box);
    # column(12/nRow, box);
    if(iCol == nRow){
      iCol = 0;
      outer$children[[iRow]] = cur_row
      cur_row = fluidRow();
      iRow = iRow + 1;
    }
  }
  if(iCol != 0){
    outer$children[[iRow]] = cur_row
  }
  return(outer);
}
