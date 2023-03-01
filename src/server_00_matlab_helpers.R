library(pracma)

sub2ind <- function(nRow, nCol, rowIndices, columnIndices, mat = c()){
  if(length(mat) != 0){
    nRow = nrow(mat)
    nCol = ncol(mat)
  }
  indices = (columnIndices-1)*nRow + rowIndices
}

ind2sub <- function(nRow, nCol, indices, mat = c()){
  if(length(mat) != 0){
    nRow = nrow(mat)
    nCol = ncol(mat)
  }
  r = ((indices-1) %% nRow) + 1
  c = floor((indices-1) / nRow) + 1
  return(list(r = r, c = c))
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

round_m <- function(v, digits = 0, sigdigits = c()){
  expo = floor(log10(abs(v)));
  val = abs(v)
  if(length(digits) > 0){
    val = round(val * 10^(digits))/10^(digits)
  }
  remainder = val / 10^expo
  
  if(length(sigdigits) > 0){
    remainder = round(remainder * 10^(sigdigits-1))/10^(sigdigits-1)
  }
  vx = return(sign(v) * remainder * 10^expo)
}

floor_m <- function(v, digits = 0, sigdigits = c()){
  expo = floor(log10(abs(v)));
  val = abs(v)
  if(length(digits) > 0){
    val = floor(val * 10^(digits))/10^(digits)
  }
  remainder = val / 10^expo
  
  if(length(sigdigits) > 0){
    remainder = floor(remainder * 10^(sigdigits-1))/10^(sigdigits-1)
  }
  vx = return(sign(v) * remainder * 10^expo)
}

sparseDiag <- function(diagVector){
  sparseMatrix(
    i = 1:length(diagVector),
    j = 1:length(diagVector),
    x = diagVector, 
    dims = c(length(diagVector), length(diagVector))
  )
}

getNonZero <- function(W){
  W[which(W != 0)]
}