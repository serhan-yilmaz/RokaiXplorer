downloadPlotDLHandler <- function(plot, file_name, file_type, bg_color = 'white', w_mult = 3, h_mult = 1){
  downloadHandler(
    filename = function() {
      if(is.reactive(file_name)){
        file_name = file_name()
      }
      paste(file_name, file_type, sep='.') 
      },
    content = function(file) {
      h = 4.6
      ggsave(file, plot = plot(), device = file_type, width=w_mult*h, height=h_mult*h, bg=bg_color)
    },
    contentType = paste("application/", file_type, sep = "")
  )
}

downloadCSVFileHandler <- function(data_file, file_name){
  downloadHandler(
    filename = function() { paste(file_name, sep='') },
    content = function(file) {
      if(is.reactive(data_file)){
        data_file = data_file()
      }
      write.csv(data_file, file = file, row.names = FALSE, quote=FALSE)
    }
  )
}