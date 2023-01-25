## Deploying RokaiXplorer with input data already preloaded


### Step 1: Install R, Rtools & RStudio

- Install R: [Download Link](https://cran.r-project.org/). [Instructions on how to download and install R](https://rstudio-education.github.io/hopr/starting.html).
- Install Rstudio: [Download Link](https://posit.co/download/rstudio-desktop/). [Instructions on installation](https://rstudio-education.github.io/hopr/starting.html#rstudio).
- Install Rtools (required for Windows): [Download Link](https://cran.r-project.org/bin/windows/Rtools/). [Instructions on installation](https://cran.r-project.org/bin/windows/Rtools/).

### Step 2: Create a project in RStudio


### Step 3: Download the RokaiXplorer source code
Next, you will need to download the source code of RokaiXplorer. For this purpose, you can run the following code in the RStudio console, which will download the source code from Github and place it under ``RokaiXplorer`` directory. 

```
if(!require("devtools"))
  install.packages("devtools")
devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/src/deploy_from_github.R?raw=TRUE")
downloadRokaiXplorer()
```
