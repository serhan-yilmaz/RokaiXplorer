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
          validate(
            need(FALSE, "Waiting for data...")
          )
  )
  return (D)
})

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
          validate(
            need(FALSE, "Waiting for data...")
          )
  )
  return (D)
})