# RokaiXplorer
RokaiXplorer is an interactive tool to analyze proteomics and phospho-proteomics data. 

You can use this tool to:
- Identify significant changes in phosphorylation and protein expression,
- Perform enrichment of gene ontology (GO) terms,
- Infer kinase activities using RoKAI

To access the application through a user-friendly interface, please visit: [RokaiXplorer application](http://explorer.rokai.io). 

## Running Locally
To run RoKAI App locally on your R installation, simply run:
```
library(shiny)
runGitHub("rokaixplorer", "serhan-yilmaz")
```
### Running an earlier version of the application
If desired, you can also run an earlier version of the application. For this purpose, specify the ref parameter to be one of the [release versions](https://github.com/serhan-yilmaz/RokaiApp/releases), e.g.:
```
library(shiny)
runGitHub("rokaixplorer", "serhan-yilmaz", ref = "v0.6.0")
```
