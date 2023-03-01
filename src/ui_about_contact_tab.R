aboutContactTab <- tabPanel(
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
      "RokaiXplorer is designed by ", 
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
)