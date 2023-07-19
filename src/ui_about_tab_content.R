style_font_size_question <- "font-size: 17px;"
style_font_size_main <- "font-size: 17px;"
# style_font_size_main <- ""
style_font_size_footnote <- "font-size: 16px;"

about_question <- function(qtxt, atxt, href, actLink = FALSE,  postfix = "", style_font_size = style_font_size_main){
  if(actLink){
    link <- actionLink(href, atxt, style = "margin-right:0px; padding-right:0px;")
  } else {
    link <- tags$a(atxt, href=href)
  }
  
  tags$div(
    # class = "inline",
    style = style_font_size,
    style = "max-width:800px; text-align: justify;", 
    style = "margin-bottom: 8px; margin-top: 8px;", 
    tags$text(qtxt),
    link, 
    tags$text(postfix)
  )
}

contact_question <- function(qtxt, atxt1, href1, atxt2, href2, actLink = FALSE, postfix = "", style_font_size = style_font_size_main){
  if(actLink){
    link <- actionLink(href1, atxt1)
  } else {
    link <- tags$a(atxt1, href=href1)
  }
  
  if(actLink){
    link2 <- actionLink(href2, atxt2)
  } else {
    link2 <- tags$a(atxt2, href=href2)
  }
  
  tags$div(
    # class = "inline", 
    style = style_font_size, 
    style = "margin-bottom: 8px; margin-top: 8px;", 
    tags$text(qtxt),
    link,
    tags$text(" or "), 
    link2,
    tags$text(postfix)
  )
}

about_desc_item <- function(txt, type = "medium", style_font_size = style_font_size_main, inline = T){
  #styling <- "max-width:800px; text-align: justify;"
  styling <- ""
  if(inline == TRUE){
    inline_txt = "inline"
  } else {
    inline_txt = ""
  }
  
  tags$div(
    class = inline_txt, 
    style = "max-width:800px; text-align: justify;", 
    style = style_font_size, 
    switch(type, "h4" = tags$h4(style=styling, txt), 
           "footnote" = tags$text(style=styling, style = style_font_size_footnote, txt),
           "medium" = tags$text(style=styling, txt))
  )
}

desc_text <- function(qtxt){
  tags$div(
    class = "inline", 
    style = "margin-bottom: 6px; margin-top; 6px;", 
    tags$text(style = "font-size: medium;", qtxt)
  )
}

txt1 <- "RokaiXplorer is a tool to perform interactive analysis of phospho-proteomic data. "

# txt1 <- "RokaiXplorer is an interactive tool to perform exploratory analysis on phospho-proteomic data with a particular focus on biomarker discovery."
txt2 <- "You can use this tool to identify significant changes in phosphorylation and infer kinase activities using RoKAI."
aboutTabIntroContent <- withMathJax(includeMarkdown("helpfiles/RokaiXplorer_welcome_intro.md"))
aboutTabIntroFootnote <- withMathJax(includeMarkdown("helpfiles/RokaiXplorer_welcome_footnote.md"))

main_features_image <- tags$div(
  style = "text-align: center; margin-bottom:8px; margin-top:4px; padding-bottom:4px; padding-top:4px; background-color:#fff;border:solid 0.75px #777;", 
  tags$img(src = "rokaixplorer_7_wonders.png", width = "750", style = "max-width:100%"), 
)

aboutTabContent <- tags$span(
  style = style_font_size_main, 
  style = "text-align:justify;", 
  tags$h3("Welcome!", style="font-weight:bold;"),
  aboutTabIntroContent,
  # about_desc_item(paste(txt1, sep = " "), inline = T),
  # about_desc_item(txt2, inline = T),
  # tags$br(),
  tags$div(id = "about_tutorial_main_div",
  about_question("Need help getting started? Try our ", "Interactive Tutorial!", "interactiveTutorialLink", actLink = T, postfix = "", style_font_size = style_font_size_question),
  ),
  tags$div(id = "about_interactive_data_browser_div",
    about_question("Interested in exploring more? Learn about our ", "Interactive Data Browser", "deploymentLink", actLink = T, postfix = " ", style_font_size = style_font_size_question),
  ),
  contact_question("Have questions or comments?", "Contact us", "contactLink", "Leave Feedback", "leaveCommentLink", actLink = T, postfix = " ", style_font_size = style_font_size_question),
  # contact_question("If you have any suggestions or comments, feel free to", "give feedback", "leaveCommentLink", "contact us.", "contactLink", actLink = T, postfix = " "),
  
  main_features_image, 
  tags$p("Thank you for using RokaiXplorer!", style = style_font_size_question),
  tags$hr(style = "margin-bottom:4px;margin-top:2px;"),
  tags$span(style = style_font_size_footnote, aboutTabIntroFootnote),
  # tags$br(),
  # about_question("If you would like to impact future developments, please fill a", "5-question survey", "https://forms.gle/JMZY1WeR3appegFt9", postfix = "to help us prioritize which additional features to implement next!"),
  # tags$br(),
  # about_desc_item("* The application is still in development. New features may be added or there may be changes in the way analysis are conducted.", type = "medium", style_font_size = style_font_size_footnote),
  # 
)