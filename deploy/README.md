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
If everything works up to this point and all libraries are installed properly, the following code should start the RokaiXplorer application with preloaded sample data, identical t the [Example Application](https://yilmazs.shinyapps.io/ADXplorer/). 
```
options(RokaiXplorer_deployment_mode = TRUE)
runRokaiXplorer()
```
Note that, after running the ```runRokaiXplorer()``` command, the application will stay open until it is closed. Make sure to close the application before moving on the next steps and running other code. 

### Step 6: Customizing the application for user data & configuration
To customize the application to load your data instead of the sample dataset, first place your data under the ```/RokaiXplorer/data/``` folder in the current directory and run the following code after setting the file paths appropriately:
```
options(RokaiXplorer_data_filepath = "data/<DATA>.csv")
options(RokaiXplorer_metadata_filepath = "data/<METADATA>.csv")
```
In the above code, please make sure to update ```<DATA>``` and ```<METADATA>``` with the names of your data files. 

Next, run the following code to set the appropriate reference proteome for your data. The available options are ```Uniprot Human```, ```Uniprot Mouse```, and ```Uniprot Rat```.
```
options(RokaiXplorer_reference_proteome = "Uniprot Mouse")
runRokaiXplorer()
```

Optionally, you can use a configuration file to set up the analysis options for your data. For this purpose, visit [RokaiXplorer website](http://explorer.rokai.io/), upload your input datasets and set your desired analysis options. Then, on the left panel, under the "Import/Export Config" section, press the ```Download Config``` button to download the ```config.json``` file which contains a list of all options set within the application. In your current directory, place this file under ```/RokaiXplorer/deploy/``` folder (and rename it to ```config.json``` if necessary). Then, run the following command in RStudio:
```
options(RokaiXplorer_config_filepath = "deploy/config.json")
runRokaiXplorer()
```
With this option preset, the results that you obtain should be identical to what you observe in the RokaiXplorer application. 

### Step 7: Customizing the application title & descriptions

```
options(RokaiXplorer_application_title = "NewTitle")
options(RokaiXplorer_application_subtitle = "This will be the new subtitle to display")
runRokaiXplorer()
```








