loadedPackagesList <- function(){
  A = sessioninfo::package_info()
  A$package
  
  cran_indices = grepl("CRAN", A$source)
  bioconductor_indices = grepl("Bioconductor", A$source)
  out = list()
  out$CRAN = A$package[cran_indices]
  out$Bioconductor = A$package[bioconductor_indices]
  out$Other = A$package[!cran_indices & !bioconductor_indices]
  return(out)
}

printPackageList <- function(){
  A = loadedPackagesList()
  types = c("CRAN", "Bioconductor", "Other")
  var_names = c("cran_packages", "bioconductor_packages", "other_packages")
  txt = ''
  for(iType in (1:length(types))){
    type = types[[iType]]
    var_name = var_names[[iType]]
    txt_start = sprintf('%s <- c(', var_name);
    
    packages = A[[type]]
    if(length(packages) > 0){
      txt = paste0(txt, txt_start, '\n')
      for(iPackage in (1:length(packages))){
        package = packages[[iPackage]]
        txt = paste0(txt, '\t', '"', package, '"')
        if(iPackage != (length(packages))){
          txt = paste0(txt, ',')
        }
        txt = paste0(txt, '\n')
      }
    } else {
      txt = paste0(txt, txt_start)
    }
    txt_end = ')\n'
    txt = paste0(txt, txt_end, '\n')
  }
  cat(txt, file = "packages/packages_list.R", append = FALSE)
}

printPackageList()
