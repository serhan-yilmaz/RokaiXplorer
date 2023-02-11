# RokaiXplorer
RokaiXplorer is a tool to perform interactive analysis of phospho-proteomic data. You can use this tool to identify significant changes in phosphorylation and infer kinase activities using RoKAI. To access it, please visit: [RokaiXplorer application](http://explorer.rokai.io). 

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
