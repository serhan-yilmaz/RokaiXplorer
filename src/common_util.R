
is.empty <- function(x){
  return(length(x) == 0)
}

readDeploymentOptions <- function(){
  source(file = "deploy/deploy_options.R", local=TRUE)
  return(deployment_options)
}