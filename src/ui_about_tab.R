source("src/ui_about_tab_content.R", local = TRUE)
source("src/ui_about_contact_tab.R", local = TRUE)
source("src/ui_about_references_tab.R", local = TRUE)
if(DEPLOYMENT_MODE_ENABLED){
  config_about_page_path = deployment_options$config_about_page_path
  aboutTabContent <- withMathJax(includeMarkdown(config_about_page_path))
  aboutTabExtraContent <- tags$div(
    tags$hr(style = "margin-bottom:8px; margin-top:14px;"),
    tags$p(style = "font-size:15px;", "* This application is made using RokaiXplorer to perform interactive analysis of phospho-proteomic data. You can use this tool to identify significant changes in phosphorylation and protein expression, to infer kinase activities and to perform pathway enrichment. For more information, please visit the FAQ page.")
  )
} else {
  aboutTabExtraContent = ""
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
       referencesTab
   ) 
)