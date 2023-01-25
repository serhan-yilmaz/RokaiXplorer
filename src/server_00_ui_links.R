observeEvent(input$contactLink, {
  updateTabsetPanel(session, "aboutTabset", "Contact")
})

observeEvent(input$leaveCommentLink, {
  updateTabsetPanel(session, "aboutTabset", "Contact")
})