reactive_dataset <- reactive({
  req(initialized())
  if(myvalue() == "upload"){
    validate(
      need(upload_data_ready(), "Waiting for data upload...")
    )
  }
  switch (myvalue(),
          "sample" = D <- Tsample,
          "upload" = D <- upload_dataset(),
          "deploymentdata" = D <- Tdeployment_data,
          validate(
            need(FALSE, "Waiting for data...")
          )
  )
  return (D)
})

reactive_expression_dataset <- reactive({
  req(initialized())
  if(myvalue() == "upload"){
    if(!upload_expression_data_ready()){
      return(NULL)
    }
  }
  switch (myvalue(),
          "sample" = D <- Tsample_expression,
          "upload" = D <- upload_expression_dataset(),
          "deploymentdata" = D <- Tdeployment_expression_data,
          validate(
            need(FALSE, "Waiting for data...")
          )
  )
  return (D)
})

# reactive_expression_dataset <- reactive({
#   folder = "data/"
#   Tsample <- read.csv(paste0(folder, "pexpression_alz_explorer_data_withoutpos.csv"))
#   # NULL
# })

reactive_metadata <- reactive({
  req(initialized())
  metadata_ready(FALSE)
  if(myvalue() == "upload"){
    validate(
      need(upload_metadata_ready(), "Waiting for metadata upload...")
    )
  }
  switch (myvalue(),
          "sample" = D <- Tsample_metadata,
          "upload" = D <- upload_metadata(),
          "deploymentdata" = D <- Tdeployment_metadata,
          validate(
            need(FALSE, "Waiting for data...")
          )
  )
  # foOnMetadataUpdate()
  return (D)
})

# observe({
#   if(metadata_ready() == TRUE){
#     isolate(foOnMetadataUpdate())
#   }
# })
# 
# foOnMetadataUpdate <- function(){
#   uiOutput("site_heatmap_select_group_ui")
# }