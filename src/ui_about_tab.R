source("src/ui_about_tab_content.R", local = TRUE)
source("src/ui_about_contact_tab.R", local = TRUE)
source("src/ui_about_references_tab.R", local = TRUE)
source("src/ui_about_versions_tab.R", local = TRUE)
source("src/ui_about_deployment_tab.R", local = TRUE)
if(DEPLOYMENT_MODE_ENABLED){
  config_about_page_path = deployment_options$description_file_path
  aboutTabContent <- withMathJax(includeMarkdown(config_about_page_path))
  aboutTabExtraContent <- tags$div(
    tags$hr(style = "margin-bottom:8px; margin-top:14px;"),
    tags$p(style = "font-size:15px;", 
           "* This application is made using",
           " RokaiXplorer",
           # tags$a("RokaiXplorer", href="http://explorer.rokai.io"),
           "to perform interactive analysis of phospho-proteomic data. You can use this tool to identify significant changes in phosphorylation and protein expression, to infer kinase activities and to perform pathway enrichment.")
  )
  # For more information, please visit the FAQ page.
} else {
  aboutTabExtraContent = ""
  # aboutTabExtraContent = tags$div(
  #   style = "text-align: center; margin-bottom:4px; background-color:#fff;border:1px;", 
  #   tags$img(id = "imgx", src = "rokaixplorer_7_wonders.png", width = "750", style = "max-width:100%"), 
  # )
}

tabPanel("About", 
   tabsetPanel(id = "aboutTabset", 
       tabPanel(
         "Welcome",
         tags$div(
           class = "panel-body", id = "about_main_div", 
           style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
           style = "font-size: 16px; text-align: justify;",
           aboutTabContent, 
           aboutTabExtraContent, 
         ),
       ),
       aboutContactTab,
       referencesTab,
       versionsTab,
       deploymentTab
   ) 
)