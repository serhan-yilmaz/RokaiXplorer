barplot_ui <- function(identifier, minzscore = 2, minzscore_max = 4, showminsubs = F, yaxis_option = "Log2-FC"){
  el_minsubs <- sliderInput(paste(identifier, "minsubs", sep = "_"), "Min. number of substrates", 1, 10, 3, step = 1, width = "220px")
  if(!showminsubs){
    el_minsubs = ""
  }
  
  item_txt = strsplit(identifier, "_",)[[1]][1]
  tags$div(
    shinycssloaders::withSpinner(plotOutput(paste(identifier, "plot", sep = "_"), height = "360px")), 
    fluidRow(
      column(width = 6, style = "padding: 8px;", 
             fluidRow(id = paste(identifier, "sliders_div", sep = "_"), 
                      
                      column(width = 6, style = "padding: 8px;", 
                             el_minsubs, 
                             foMaxItemsHelper(
                               sliderInput(paste(identifier, "maxitems", sep = "_"), "Number of items shown", 5, 50, 20, step = 1, width = "220px")
                               , identifier),
                      ),
                      column(width= 6, style = "padding: 8px;", 
                             sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. absolute z-score", 0, minzscore_max, minzscore, step = 0.05, width = "220px"),
                             tags$div(
                               style = "margin-top: 8px;", 
                               tags$b(paste("Significant only", sep = "")), 
                               shinyWidgets::materialSwitch(inputId = paste(identifier, "significant_only", sep = "_"), label = "", status = "warning", value = F, inline = T)
                             )
                      )
             )),
      column(width = 3, style = "padding: 8px; padding-left: 16px;", 
             multiChoicePicker(paste(identifier, "yaxis", sep = "_"), "Plot Y-Axis:", c(yaxis_option, "Z-Score"), isInline = "F"),
             tags$div(
               style = "margin-top: 4px;", 
               #uiOutput("site_heatmap_select_group_ui"), 
               multiChoicePicker(paste(identifier, "coloring", sep = "_"), "Coloring:", c("Z-Score", "Significance"), isInline = "F")
             )
      ),
      column(width = 3, style = "padding: 8px;", tags$div(downloadButton(paste(identifier, "downloadPlotPNG", sep = "_"), 'Download PNG'),
                                                          tags$br(), 
                                                          downloadButton(paste(identifier, "downloadPlotPDF", sep = "_"), 'Download PDF')
      )
      )
    )
  )
}

heatmap_ui <- function(identifier, significant_only = F, showminsubs = F, minsamplewise_magnitude = 0.5){
  el_minsubs <- sliderInput(paste(identifier, "minsubs", sep = "_"), "Min. number of substrates", 1, 10, 3, step = 1, width = "220px")
  if(!showminsubs){
    el_minsubs = ""
  }
  item_txt = strsplit(identifier, "_",)[[1]][1]
  tags$div(
    shinycssloaders::withSpinner(plotOutput(paste(identifier, "", sep = ""), height = "360px")), 
    fluidRow(
      column(width = 6, style = "padding: 8px;", 
             fluidRow(id = paste(identifier, "sliders_div", sep = "_"), 
                      
                      column(width = 6, style = "padding: 8px;",
                             el_minsubs, 
                             foMaxItemsHelper(
                               sliderInput(paste(identifier, "maxitems", sep = "_"), "Number of items shown", 10, 100, 30, step = 1, width = "220px")
                               , identifier),
                      ),
                      column(width= 6, style = "padding: 8px;", 
                             foMinSamplewiseMagnitudeHelper(sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. Samplewise Magnitude", 0, 2, minsamplewise_magnitude, step = 0.02, width = "220px"),
                              identifier),
                             tags$div(
                               style = "margin-top: 8px;", 
                               tags$b(paste("Significant only", sep = "")), 
                               shinyWidgets::materialSwitch(inputId = paste(identifier, "significant_only", sep = "_"), label = "", status = "warning", value = significant_only, inline = T)
                             )
                      )
             )),
      column(width = 3, style = "padding: 8px; padding-left: 16px; padding-top: 12px;",
             multiChoicePicker(paste(identifier, "intensity_fc_style", sep = "_"), "Show:", c("Case samples", "Both case and control"), isInline = "F"),
             tags$div(
               style = "margin-top: 6px;",
               uiOutput(paste(identifier, "select_group_ui", sep = "_")), 
               #  multiChoicePicker(paste(identifier, "coloring", sep = "_"), "Coloring:", c("Z-Score", "Significance"), isInline = "F", multiple = T)
             )
      ),
      column(width = 3, style = "padding: 8px; padding-top: 12px;", tags$div(downloadButton(paste(identifier, "downloadPlotPNG", sep = "_"), 'Download PNG'),
                                                                             tags$br(), 
                                                                             downloadButton(paste(identifier, "downloadPlotPDF", sep = "_"), 'Download PDF')
      )
      )
    )
  )
}

network_ks_ui <- function(identifier, defaultSingleKinases = F){
  item_txt = strsplit(identifier, "_",)[[1]][1]
  footer_txt = "This network is interactive! You can drag & drop nodes to adjust the view and hover to see more information. Double click on a node to inspect it in detail. "
  
  tags$div(
    style = "padding: 4px;", 
    fluidRow(
      column(width = 9, 
             tags$div(
               style = "margin-bottom:8px;",
               shinycssloaders::withSpinner(visNetworkOutput(identifier, height = "480px")),
               tags$p(style = "text-align:justify; font-size:14px;", footer_txt)
               )
             ), 
      column(width = 3, 
             tags$div(id = paste(identifier, "sliders_div", sep = "_"), 
                      tags$div(style="max-width:240px;", foMaxItemsHelper(sliderInput(paste(identifier, "maxitems", sep = "_"), "Number of items shown", 5, 50, 20, step = 1, width = "220px")
                                       , identifier)),
                      sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. absolute z-score", 0, 4, 2, step = 0.05, width = "220px"),
                      tags$div(
                        style = "margin-top: 8px;", 
                        tags$b(paste("Significant only", sep = "")), 
                        shinyWidgets::materialSwitch(inputId = paste(identifier, "significant_only", sep = "_"), label = "", status = "warning", value = F, inline = T)
                      ),
                      tags$div(
                        style = "margin-top: 13px;", 
                        tags$b("Single Kinases"), 
                        shinyWidgets::materialSwitch(inputId = paste(identifier, "single_kinases", sep = "_"), label = "", status = "danger", value = defaultSingleKinases, inline = T)
                      ),
                      # tags$div(style = "margin-top: 8px;",
                      #          downloadButton(paste(identifier, "downloadPlotPNG", sep = "_"), 'Download PNG'),
                      #          #tags$br(),
                      #          #downloadButton(paste(identifier, "downloadPlotPDF", sep = "_"), 'Download PDF')
                      # )
             ))
    )
  )
}

volcanoplot_ui <- function(identifier, minlogfc = 0.32){
  tags$div(
    style = "padding: 0px;", 
    fluidRow(
      column(width = 9, 
             tags$div(
               shinycssloaders::withSpinner(plotlyOutput(identifier, height = "420px"), hide.ui = FALSE),
               "Click on a point to inspect it in detail."
               )
             ), 
      column(width = 3, 
             tags$div(id = paste(identifier, "sliders_div", sep = "_"), 
                      sliderInput(paste(identifier, "maxfdr", sep = "_"), "Max. FDR", 0.01, 0.25, 0.1, step = 0.01, width = "220px"),
                      #sliderInput(paste(identifier, "minzscore", sep = "_"), "Min. absolute z-score", 0, 4, 2, step = 0.05, width = "220px"),
                      sliderInput(paste(identifier, "minlogfc", sep = "_"), "Min. absolute log2-FC", 0, 1, minlogfc, step = 0.01, width = "220px"), 
                      tags$div(style = "margin-top:8px;",
                               textOutput(paste(identifier, "summary", sep = "_"))
                      )
                      # tags$div(
                      #   style = "margin-top: 8px;",
                      #   tags$b("Single Kinases"),
                      #   shinyWidgets::materialSwitch(inputId = paste(identifier, "single_kinases", sep = "_"), label = "", status = "danger", value = F, inline = T)
                      # )
             ))
    )
  )
}