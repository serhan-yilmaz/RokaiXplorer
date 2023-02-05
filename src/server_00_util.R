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

formatNumber <- function(X, digits = 3){
  inner <- paste0("%.", digits, "f");
  out <- sprintf(inner, X);
}

formatNumericVariables <- function(GT, exclude = c("PValue", "FDR")){
  cols <- colnames(GT)
  nCol = ncol(GT)
  
  for(iCol in 1:nCol){
    Q = GT[[iCol]]
    cname <- cols[iCol]
    if(is.numeric(Q) && is.na(match(cname, exclude))){
      GT[[iCol]] <- formatNumber(Q);
    }
  }
  # GT$LogOdds = formatNumber(GT$LogOdds);
  # GT$StdErr = formatNumber(GT$StdErr);
  # GT$EffectiveMag = formatNumber(GT$EffectiveMag);
  return(GT)
}

ceil <- function(v, digits = 0, sigdigits = c()){
  expo = floor(log10(abs(v)));
  val = abs(v)
  if(length(digits) > 0){
    val = ceiling(val * 10^(digits))/10^(digits)
  }
  remainder = val / 10^expo
  
  if(length(sigdigits) > 0){
    remainder = ceiling(remainder * 10^(sigdigits-1))/10^(sigdigits-1)
  }
  vx = return(sign(v) * remainder * 10^expo)
}