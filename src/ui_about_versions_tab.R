dataset_version_text <- function(dataset, date, link){
  id = paste(dataset, "txt", sep = "_");
  dataset <- paste(dataset, ":", sep = "")
  tags$tr(
    tags$td(tags$li(tags$a(dataset, href = link))), 
    tags$td(tags$text(date, id = id))
  )
}

versionsTab <- tabPanel(
  "Versions",
  tags$div(
    class = "panel-body",
    style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
    #tags$p(),
    tags$h3("Dataset Versions", style="font-weight:bold;"),
    #desc_text("Last updated dates for the datasets used are as follows:")
    tags$div(
      style = "max-width: 300px;", 
      helper(
        selectInput("dataset_version_selection", "NetworkData: ", 
                    ####, "v2.1.4 (May 2021)", 2
                    choices = foList("Latest (November 2022)", 1), 
                    selected = 1, selectize = F),
        type = "markdown", id = "include_networkdata_version_helper", content = "networkdata_version"
      ),
      tippy_this("include_networkdata_version_helper", "<span style='font-size:14px; margin: 0px;'>Click to learn about the NetworkData.<span>", allowHTML = TRUE), 
    ),
    desc_text("The versions (last modified dates) of the datasets used are as follows:"),
    tags$div(
      style="max-width:300px;",
      tags$table(
        style="font-size: 16px; width: 100%; margin-left: 22px;",
        dataset_version_text("Uniprot", "2022-10-04", "https://www.uniprot.org/"),
        dataset_version_text("PhosphoSitePlus", "2021-04-19", "https://www.phosphosite.org/"),
        dataset_version_text("Signor", "2021-05-21", "https://signor.uniroma2.it/"),
        dataset_version_text("STRING", "2018-12-20", "https://string-db.org/"),
        dataset_version_text("PTMcode", "2014-09-17", "https://ptmcode.embl.de/"),
        dataset_version_text("DEPOD", "2019-03-01", "http://www.depod.org/"),
        dataset_version_text("GO", "2023-01-19", "http://geneontology.org/"),
      ))
  )
)