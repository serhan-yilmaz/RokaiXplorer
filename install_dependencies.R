#!/usr/bin/env Rscript

install_dependencies <- function(verbose = F){
  # Install our standard set of R packages
  
  # Should all packages be reinstalled, regardless of whether they already are?
  reinstall <- FALSE
  
  # Packages installed should be added to this list
  # under CRAN / bioconductor / github
  cran_packages <- c(
  	"anytime",
  	"askpass",
  	"assertthat",
  	"backports",
  	"BiocManager",
  	"bit",
  	"bit64",
  	"broom",
  	"bslib",
  	"cachem",
  	"cellranger",
  	"cicerone",
  	"cli",
  	"clipr",
  	"colorspace",
  	"crayon",
  	"crosstalk",
  	"data.table",
  	"DBI",
  	"dbplyr",
  	"digest",
  	"dplyr",
  	"DT",
  	"ellipsis",
  	"fansi",
  	"fastmap",
  	"fontawesome",
  	"forcats",
  	"fs",
  	"gargle",
  	"generics",
  	"ggplot2",
	"ggthemes", 
  	"glue",
  	"googledrive",
  	"googlesheets4",
  	"gtable",
  	"haven",
  	"hms",
  	"htmltools",
  	"htmlwidgets",
  	"httpuv",
  	"httr",
  	"ids",
  	"jquerylib",
  	"jsonlite",
  	"knitr",
  	"later",
  	"lattice",
  	"lazyeval",
  	"lifecycle",
  	"lubridate",
  	"magrittr",
  	"mailR", 
  	"markdown",
  	"Matrix",
  	"memoise",
  	"mime",
  	"modelr",
  	"munsell",
  	"nanotime",
  	"openssl",
	"openxlsx", 
  	"pillar",
  	"pkgconfig",
  	"plotly",
  	"plyr",
	"pracma", 
  	"promises",
  	"purrr",
  	"R6",
  	"Rcpp",
  	"RcppCCTZ",
  	"readr",
  	"readxl",
  	"reprex",
  	"reshape",
  	"rjson",
  	"rlang",
  	"rsconnect",
  	"rstudioapi",
  	"rvest",
  	"sass",
  	"scales",
  	"sessioninfo",
  	"shiny",
  	"shinyBS",
  	"shinycssloaders",
  	"shinydashboard",
  	"shinyhelper",
  	"shinyjs",
  	"shinylogs",
  	"shinytoastr",
  	"shinyWidgets",
  	"stringi",
  	"stringr",
  	"tibble",
  	"tidyr",
  	"tidyselect",
  	"tidyverse",
  	"tippy",
  	"tzdb",
  	"utf8",
  	"uuid",
  	"vctrs",
	"viridis", 
  	"viridisLite",
  	"visNetwork",
  	"withr",
	"writexl", 
  	"xfun",
  	"xml2",
  	"xtable",
  	"yaml",
  	"zoo",
	"statmod"
  )
  
  bioconductor_packages <- c(
  	"preprocessCore",
	"limma"
  )
  
  # Packages only available from a GitHub repository - not on CRAN
  github_packages <- c()
  
  install_opts <- "--byte-compile"
  
  # Installation functions
  installed <- data.frame(installed.packages())$Package
  # source("http://bioconductor.org/biocLite.R")
  
  is.installed <- function(package.name) package.name %in% installed
  
  pastemsg <- function(...) message(paste(...))
  
  # Create an installer function that that calls install.fn.
  # Additional arguments are passed to install.fn
  installer <- function(install.fn, ...) {
    do.install <- function(package.name, check.installed=!reinstall) {
      if(check.installed && is.installed(package.name)) {
        if(verbose){
          pastemsg("Package", package.name, "is already installed.")
        }
      } else {
        pastemsg("Installing", package.name)
        install.fn(c(package.name), ...)
      }
    }
  }
  
  install_cran <- installer(install.packages, repos="http://cran.fhcrc.org",
                            clean=TRUE, dependencies=TRUE,
                            INSTALL_opts=install_opts)
  install_bioconductor <- installer(BiocManager::install, update = FALSE, ask = FALSE)
  
  
  ## Actually do the installation
  # Bioconductor
  # Base install - packages installed by biocLite()
  # are returned by biocinstallPkgGroups('lite')
  # if(all(sapply(c("Biobase", "IRanges", "AnnotationDbi"), is.installed)) && !reinstall) {
  #   pastemsg("Bioconductor base installed")
  # } else {
  #   biocLite()
  # }
  
  if (!require("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
  
  # And all the extras
  for(p in bioconductor_packages) {
    tryCatch(install_bioconductor(p), error=warning)
  }
  
  # CRAN packages
  for(p in cran_packages) {
    tryCatch(install_cran(p), error=warning)
  }
  
  # GitHub packages
  for(p in github_packages) {
    pastemsg("Installing", p, "from GitHub")
    p <- list(p)
    p$args <- install_opts
    tryCatch(do.call('install_github', p), error=warning)
  }
}
