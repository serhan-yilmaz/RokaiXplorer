## Deploying RokaiXplorer with input data already preloaded


### Step 1: Install R, Rtools & RStudio

- Install R: [Download Link](https://cran.r-project.org/). [Instructions on how to download and install R](https://rstudio-education.github.io/hopr/starting.html).
- Install Rstudio: [Download Link](https://posit.co/download/rstudio-desktop/). [Instructions on installation](https://rstudio-education.github.io/hopr/starting.html#rstudio).
- Install Rtools (required for Windows): [Download Link](https://cran.r-project.org/bin/windows/Rtools/). [Instructions on installation](https://cran.r-project.org/bin/windows/Rtools/).

### Step 2: Create a project in RStudio


### Step 3: Download the RokaiXplorer source code
Next, you will need to download the source code of RokaiXplorer. For this purpose, you can run the following code in the RStudio console, which will download the source code from Github and place it under ``/RokaiXplorer`` folder in the currect working directory: 

```
if(!require("devtools"))
  install.packages("devtools")
devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/src/deploy_from_github.R?raw=TRUE")
downloadRokaiXplorer()
```

### Step 4: Install the required R libraries
To install the required libraries to run and deploy RokaiXplorer, please run the following code: 
```
if(!require("devtools"))
  install.packages("devtools")
devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/src/deploy_from_github.R?raw=TRUE")
installDependencies()
```

### Step 5: Running RokaiXplorer under deployment mode
If everything works up to this point and all libraries are installed properly, the following code should start the RokaiXplorer with preloaded sample data, similar to how it is in the [ExampleApp](https://yilmazs.shinyapps.io/ADXplorer/). 
```
options(RokaiXplorer_deployment_mode = TRUE)
runRokaiXplorer()
```



