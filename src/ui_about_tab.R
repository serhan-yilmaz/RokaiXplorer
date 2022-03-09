about_question <- function(qtxt, atxt, href, actLink = FALSE,  postfix = ""){
  if(actLink){
    link <- actionLink(style = "font-size: large;", href, atxt)
  } else {
    link <- tags$a(style = "font-size: large;", atxt, href=href)
  }
  
  tags$div(
    class = "inline",
    style = "max-width:800px; text-align: justify;", 
    style = "margin-bottom: 8px; margin-top; 8px;", 
    tags$text(style = "font-size: large;", qtxt),
    link, 
    tags$text(style = "font-size: large; ", postfix)
  )
}

contact_question <- function(qtxt, atxt1, href1, atxt2, href2, actLink = FALSE, postfix = ""){
  if(actLink){
    link <- actionLink(style = "font-size: large;", href1, atxt1)
  } else {
    link <- tags$a(style = "font-size: large;", atxt1, href=href1)
  }
  
  if(actLink){
    link2 <- actionLink(style = "font-size: large;", href2, atxt2)
  } else {
    link2 <- tags$a(style = "font-size: large;", atxt2, href=href2)
  }
  
  tags$div(
    class = "inline", 
    style = "margin-bottom: 8px; margin-top; 8px;", 
    tags$text(style = "font-size: large;", qtxt),
    link,
    tags$text(style = "font-size: large;", " or "), 
    link2,
    tags$text(style = "font-size: large;", postfix)
  )
}

about_desc_item <- function(txt, type = "h4"){
  #styling <- "max-width:800px; text-align: justify;"
  styling <- ""
  tags$div(
    class = "inline", 
    style = "max-width:800px; text-align: justify;", 
    switch(type, "h4" = tags$h4(style=styling, txt), 
           "footnote" = tags$text(style=styling, style = "font-size:small;", txt),
           "medium" = tags$text(style=styling, style = "font-size:medium;", txt))
  )
}

desc_text <- function(qtxt){
  tags$div(
    class = "inline", 
    style = "margin-bottom: 6px; margin-top; 6px;", 
    tags$text(style = "font-size: medium;", qtxt)
  )
}

tabPanel("About", 
   tabsetPanel(id = "aboutTabset", 
       tabPanel(
         "Welcome",
         tags$div(
           class = "panel-body", id = "about_main_div", 
           style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
           #tags$p(),
           tags$h3("Welcome!", style="font-weight:bold;"),
           about_desc_item("RokaiXplorer is an interactive environment for exploratory analysis of phospho-proteomic data with a particular focus on biomarker discovery."),
           contact_question("If you have any suggestions or comments, feel free to", "give feedback", "leaveCommentLink", "contact us", "contactLink", actLink = T, postfix = ""),
           about_question("Also, if you would like to impact future developments, please fill a", "5-question survey", "https://forms.gle/JMZY1WeR3appegFt9", postfix = "to help us prioritize which additional features to implement next!"),
           about_desc_item("* The application is still in development! New features may be added or there may be changes in the way analysis are conducted as time goes on.", type = "medium"),
           )
       ),
       tabPanel(
         "Contact",
         id = "Contact", 
         tags$div(
           class = "panel-body",
           style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
           #tags$p(),
           tags$h3("Contact", style="font-weight:bold;"),
           # desc_text("RoKAI is designed by Serhan Yilmaz and Mehmet Koyuturk at Case Western Reserve University."),
           tags$div(
             class = "inline", 
             style = "font-size: medium; margin-bottom: 6px; margin-top; 6px;", 
             "RoKAI is designed by ", 
             tags$a("Serhan Yilmaz", href = "http://www.serhanyilmaz.com/", target="_blank"), " and ", tags$a("Mehmet Koyuturk", href = "http://compbio.case.edu/koyuturk/", target="_blank"), " at Case Western Reserve University.",
           ),
           desc_text("If you have any questions, please contact <serhan.yilmaz@case.edu>"),
           
           tags$h4("Giving feedback", style="font-weight:bold;"),
           desc_text("To make comments, suggestions or to report a problem, please use the form below:"),
           tags$div(
             tags$div(style="display:inline-block; margin: 2px 0px 2px 0px;",
                      textInput("textinput_name", "Name", value = "", width = 220, placeholder = "(Optional)")
             ),
             tags$div(style="display:inline-block; margin: 2px 8px 2px 8px; ",
                      textInput("textinput_org", "Organization", value = "", width = 220, placeholder = "(Optional)"),
             ),
           ),
           tags$div(
             tags$div(style="display:inline-block; margin: 2px 0px 2px 0px;",
                      textInput("textinput_email", "Contact Email", value = "", width = 270, placeholder = "(Optional)"),
             ), 
             tags$div(style="display:inline-block; margin: 2px 8px 2px 8px;",
                      selectInput("message_type", "Category", 
                                  choices = c("Comment", "Suggestion", "Bug Report"), 
                                  selected = "Comment", selectize = F, width = 170)    
             ),
           ),
           tags$div(style = "margin: 2px 0px 2px 0px;",
                    textAreaInput("textinput_message", "Message", height = 150, value = "", width = 460),
                    actionButton("buttonLeaveFeedback", "Submit", style = "margin-top: 4px;"),
           ),
           #desc_text("The name, organization and email fields are optional. Please enter a contact information if you would like to be notified about future updates (e.g., if the requested feature is implemented). "),
         )
       ),
   ) 
)