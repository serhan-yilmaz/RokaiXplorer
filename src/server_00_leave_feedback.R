set_enabled_feature_suggestion_box <- function(enabled){
  if(enabled){
    shinyjs::enable("textinput_name")
    shinyjs::enable("textinput_org")
    shinyjs::enable("textinput_email")
    shinyjs::enable("message_type")
    shinyjs::enable("textinput_message")
    shinyjs::enable("buttonLeaveFeedback")
  } else {
    shinyjs::disable("textinput_name")
    shinyjs::disable("textinput_org")
    shinyjs::disable("textinput_email")
    shinyjs::disable("message_type")
    shinyjs::disable("textinput_message")
    shinyjs::disable("buttonLeaveFeedback")
  }
}

foSendEmail <- function(category, name, org, email, message, version){
  if(!EMAIL_AVAILABLE){
    message(sprintf("Email credentials are not available. Skipping the email."))
    main_logging("Email credentials are not available.")
    return();
  }
  tryCatch({
    app_name = "RokaiXplorer"
    
    time_ = format(Sys.time(), "%Y-%m-%d %H:%M", tz="America/New_York", usetz=TRUE)
    
    foWrapMessage = function(message){
      s = str_split_1(message, "\n")
      out = "<div style = 'padding:10px; padding-top:2px; color:#741B47; text-align:justify;'>"
      for(str in s){
        out = paste0(out, "<div>- ", str, "</div>")
      }
      out = paste0(out, "</div>")
      return(out)
    }
    
    foEmailLine <- function(name, value){
      paste0("<div>", "<b>", name, "</b>", ": ", value, "</div>")
    }
    
    body = paste0(
      paste0("<div style = 'font-size:14px; margin-bottom:4px;'>Received new feedback for ", app_name, " ", version, " on ", time_, ".</div>"),
      foEmailLine("Category", category), 
      foEmailLine("Name", name), 
      foEmailLine("Organization", org), 
      foEmailLine("Email", email), 
      foEmailLine("Message", foWrapMessage(message))
    )
    
    library(mailR)
    send.mail(from = paste0("RokaiServer <", email_credentials$username, "@gmail.com>"),
              to = email_credentials$to,
              subject = paste0("New ", app_name, " feedback at ", time_),
              body = body,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = email_credentials$username, passwd = email_credentials$password, ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              debug = FALSE,
              html = TRUE)
  }, error = function(e){
    message(sprintf("An error occurred while sending email: %s", as.character(e)))
    main_logging("Error occurred while sending email")
  })
}

observeEvent(input$buttonLeaveFeedback, {
  if(nchar(input$textinput_message) > 0){
    category = input$message_type
    name = input$textinput_name
    org = input$textinput_org
    email = input$textinput_email
    message = input$textinput_message
    main_logging(paste("Left a Feedback - Category: ", category, sep = ""))
    feedback_logging(paste("Category: ", category, " - Name: ", name, ", Org: ", org, ", Email: ", email, "\nMessage: ", message, sep = ""))
    set_enabled_feature_suggestion_box(enabled=F)
    delay(300, set_enabled_feature_suggestion_box(enabled=T))
    delay(300, toastr_success("Your response has been saved. Thank you!", closeButton = F))
    
    version = version_text()
    foSendEmail(category, name, org, email, message, version)
  } else {
    toastr_warning("The message field cannot be empty.", closeButton = F)
  }
})