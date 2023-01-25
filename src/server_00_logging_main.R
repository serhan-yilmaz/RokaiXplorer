main_logging <- function(message){
  filepath = paste("logs/combined_log_", version_text(), ".txt", sep = "")
  if(dir.exists("logs/")){
    cat(paste(as.character(Sys.time()), " - " ,  session_id(), ": ", message, "\n", sep = ""), file = filepath, append = T)
  }
}

feedback_logging <- function(message){
  filepath = paste("logs/feedback_log_", version_text(), ".txt", sep = "")
  if(dir.exists("logs/")){
    cat(paste(as.character(Sys.time()), " - " ,  session_id(), ": ", message, "\n", sep = ""), file = filepath, append = T)
  }
}

sessionTimeStart <- reactiveVal(Sys.time())

session$onSessionEnded(function() {
  abcd <- round(as.double(difftime(Sys.time(), isolate(sessionTimeStart()), units = "mins")), 1)
  isolate(main_logging(paste("Session Ended: ", abcd, " minutes passed", sep = "")))
})