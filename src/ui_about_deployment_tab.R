

deploymentTab <- tabPanel(
  "Deployment",
  tags$div(
    class = "panel-body",
    style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
    #tags$p(),
    tags$h3("Deployment", style="font-weight:bold;"),
    tags$div(
      style = "font-size: 16px; text-align: justify;", 
      withMathJax(includeMarkdown('helpfiles/deployment_tab.md'))
    )
    
  )
)