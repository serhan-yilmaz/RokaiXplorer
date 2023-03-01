foUncollapseBoxIfNeeeded <- function(id, collapse = F, nullval = T){
  colid = paste0(id, "_collapse");
  val = input[[colid]]
  if(is.null(val)){
    val = nullval
  }
  if(val == !collapse){
    js$collapse(id)
  }
}

fo_restore_if_applicable <- function(groups, var){
  if(!any(is.na(match(var, groups)))){
    return(var)
  }
  if(length(groups) >= 1){
    return(groups[1])
  }
  return("")
}

foGetGoCategoryText <- function(cat){
  switch(cat, 
         "molecular_function" = cat <- "Molecular_Function",
         "biological_process" = cat <- "Biological_Process",
         "cellular_component" = cat <- "Cellular_Component"
  )
  return(cat)
}



# foCollapseAtStart <- function(){
#   # foUncollapseBoxIfNeeeded("optionbox_subgroup_differences", collapse = T, nullval = F)
#   # foUncollapseBoxIfNeeeded("optionbox_filter_by_subgroup", collapse = T, nullval = F)
# }