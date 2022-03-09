main_logging <- function(message){
  filepath = paste("logs/combined_log_", version_text(), ".txt", sep = "")
  cat(paste(as.character(Sys.time()), " - " ,  session_id(), ": ", message, "\n", sep = ""), file = filepath, append = T)
}

feedback_logging <- function(message){
  filepath = paste("logs/feedback_log_", version_text(), ".txt", sep = "")
  cat(paste(as.character(Sys.time()), " - " ,  session_id(), ": ", message, "\n", sep = ""), file = filepath, append = T)
}