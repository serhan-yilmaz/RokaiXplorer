if(!require("devtools"))
	install.packages("devtools")
	
devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/src/common_util.R?raw=TRUE")

downloadRokaiXplorer <- function(forceDownload = FALSE, runApp = FALSE){
  github_account = "serhan-yilmaz";
  github_repository = "RokaiXplorer"
  target_folder = github_repository
  
  if(file.exists(target_folder) ){
    if(forceDownload == TRUE){
      unlink(target_folder, recursive = TRUE)
      warning_msg = sprintf("'%s' folder is deleted and the contents are replaced from Github.", target_folder);
      warning(warning_msg);
    } else {
      stop(sprintf("'%s' folder already exists. The operation is cancelled. Please remove the folder or set forceDownload = TRUE.", target_folder))
    }
  }
  
  if(!file.exists(target_folder)){
    # download a .zip file of the repository
    # from the "Clone or download - Download ZIP" button
    # on the GitHub repository of interest
    url = paste0("https://github.com/", github_account, "/", github_repository, "/archive/refs/heads/master.zip")
    
    zip_filename = paste0(github_repository, ".zip")
    download.file(url = url, destfile = zip_filename)
    
    # unzip the .zip file
    unzip(zipfile = zip_filename)
    file.rename(sprintf("%s-main", github_repository), target_folder)
  }
}

installDependencies <- function(){
  if(!require("devtools"))
    install.packages("devtools")
  devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/install_dependencies.R?raw=TRUE")
  install_dependencies()
}

runRokaiXplorer <- function(){
  library("shiny")
  runApp(appDir = "RokaiXplorer")
}

deployRokaiXplorer <- function(shinyapps_title = NULL, shinyapps_account = NULL, forceUpdate = TRUE){
  Sys.setenv(LANG = "en")
  library(shiny)
  library(rsconnect)
  library(BiocManager)
  
  options(repos = BiocManager::repositories())
  
  deployment_options <- readDeploymentOptions("RokaiXplorer")
  saveDeploymentOptions()
  
  if(is.null(shinyapps_title)){
	shinyapps_title = deployment_options$shinyapps_title
	if(is.na(shinyapps_title)){
		shinyapps_title = NULL
	}
  }
  if(is.null(shinyapps_account)){
	shinyapps_account = deployment_options$shinyapps_account
	if(is.na(shinyapps_account)){
		shinyapps_account = NULL
	}
  }

  deployApp(appDir = "RokaiXplorer", appTitle = shinyapps_title, account = shinyapps_account, forceUpdate = forceUpdate)
}

clearRokaiXplorerOptions <- function(){
  opts <- names(deploymentOptionSpecs)
  for(iOpt in (1:(length(deploymentOptionSpecs) - 1))){
    name = opts[[iOpt]]
    global_name = deploymentOptionSpecs[[iOpt]]$global
    l <- list()
    l[[global_name]] = NA
    options(l)
  }
}

saveDeploymentOptions <- function(){
  deployment_options <- readDeploymentOptions("RokaiXplorer")
  
  line1 = 'deployment_options <- list(';
  line2 = '\t#### Do not change anything before this line ####'
  opt_lines = '';
  
  opts <- names(deployment_options)
  for(iOpt in (1:length(deployment_options))){
    name = opts[[iOpt]]
    value = deployment_options[[iOpt]]
    txt = paste0('\t', name, ' = ')
    if(!is.null(value) && !is.na(value)){
      if(is.logical(value)){
        value_txt = paste0(value)
      } else {
        value_txt = paste0('"', value, '"')
      }
    } else {
      value_text = "NULL"
    }
    txt = paste0(txt, value_txt)
    if(iOpt != length(deployment_options)){
      txt = paste0(txt, ',', '\n')
    }
    opt_lines = paste0(opt_lines, txt)
  }
  
  line_end1 = '\t#### Do not change anything after this line ####'
  line_end2 = ')'
  
  content = paste(line1, line2, opt_lines, line_end1, line_end2, sep = "\n");  
  cat(content, file = "RokaiXplorer/deploy/deploy_options.R", append = FALSE)
}





