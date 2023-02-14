paper_txt <- function(authors, date, title, journal, link, misc){
  tags$div(
    class = "inline", 
    style = "margin-bottom: 6px; margin-top: 6px;", 
    tags$ul(
      style = "margin-bottom: 0px; margin-top: 0px;", 
      tags$li(
        tags$text(style = "font-size: medium;", paste(authors, " (", date, ") ", sep = "")),
        tags$text(style = "font-size: medium; font-style: italic;", title),
        tags$a(style = "font-size: medium;", journal, href = link),
        tags$text(style = "font-size: medium;", misc)
      ))
  )
}

referencesTab <- tabPanel(
  "References",
  tags$div(
    class = "panel-body",
    style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
    #tags$p(),
    tags$h3("References", style="font-weight:bold;"),
    # desc_text("Please cite the following paper(s) if you use RoKAI in your research:"),
    tags$p("", style = "margin: 0px; padding-bottom: 2px; padding-top: 0px;"),
    desc_text("This application uses the following resources:"),
    paper_txt("Yilmaz S., Ayati M., Schlatzer D., Cicek A. E., Chance M. R., Koyuturk M.", "2021", "Robust inference of kinase activity using functional networks", "Nature Communications", "https://doi.org/10.1038/s41467-021-21211-6", "12 (1117)"),
    paper_txt("Hornbeck, P. V. et al.", "2015", "Phosphositeplus, 2014: mutations, ptms and recalibrations.", "Nucleic acids research", "https://doi.org/10.1093/nar/gku1267", "43(D1), D512-D520"),
    paper_txt("Licata, L. et al.", "2020", "SIGNOR 2.0, the SIGnaling network open resource 2.0: 2019 update.", "Nucleic acids research", "https://doi.org/10.1093/nar/gkz949", "48(D1), D504-D510"),
    paper_txt("Minguez, P. et al.", "2012", "PTMcode: a database of known and predicted functional associations between post-translational modifications in proteins.", "Nucleic acids research", "https://doi.org/10.1093/nar/gks1230", "41(D1), D306-D311"),
    paper_txt("Szklarczyk, D. et al.", "2014", "STRING v10: protein–protein interaction networks, integrated over the tree of life.", "Nucleic acids research", "https://doi.org/10.1093/nar/gku1003", "43(D1), D447-D452"),
    paper_txt("Damle, N. P., & Köhn, M.", "2019", "The human DEPhOsphorylation Database DEPOD: 2019 update.", "Database", "https://doi.org/10.1093/database/baz133", ""),
    paper_txt("The Gene Ontology resource: enriching a GOld mine", "2021", "", "Nucleic acids research", "https://doi.org/10.1093/nar/gkaa1113", "49.D1: D325-D334"),
    # tags$h3("Acknowledgement", style="font-weight:bold;"),
    # desc_text("This work was supported by National Institute of Health (NIH) grant R01-LM012980 from the National Libraries of Medicine.")
  )
)