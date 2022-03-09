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

observeEvent(input$buttonLeaveFeedback, {
  if(nchar(input$textinput_message) > 0){
    main_logging(paste("Left a Feedback - Category: ", input$message_type, sep = ""))
    feedback_logging(paste("Category: ", input$message_type, " - Name: ", input$textinput_name, ", Org: ", input$textinput_org, ", Email: ", input$textinput_email, "\nMessage: ", input$textinput_message, sep = ""))
    set_enabled_feature_suggestion_box(enabled=F)
    delay(300, set_enabled_feature_suggestion_box(enabled=T))
    delay(300, toastr_success("Your response has been saved. Thank you!", closeButton = F))
  } else {
    toastr_warning("The message field cannot be empty.", closeButton = F)
  }
})