## Deploying RokaiXplorer with input data already preloaded

You can use [RokaiXplorer](http://explorer.rokai.io/) to share your datasets and analysis results online in the form of an interactive web application! This tutorial will teach you how prepare and deploy your application in a few easy steps using the provided scripts on R. 

To see some examples on deployed applications, check out the following: 
- A sample application: [RokaiXplorer-ExampleApp](https://serhan-yilmaz.shinyapps.io/exampleapp/)
- A live version on Alzheimer's data: [ADXplorer](https://yilmazs.shinyapps.io/ADXplorer/)

### Step 1: Install R, Rtools & RStudio

- Install R: [Download Link](https://cran.r-project.org/). [Instructions on how to download and install R](https://rstudio-education.github.io/hopr/starting.html).
- Install Rstudio: [Download Link](https://posit.co/download/rstudio-desktop/). [Instructions on installation](https://rstudio-education.github.io/hopr/starting.html#rstudio).
- Install Rtools (required for Windows): [Download Link](https://cran.r-project.org/bin/windows/Rtools/). [Instructions on installation](https://cran.r-project.org/bin/windows/Rtools/).

While installing Rtools, please make sure that its version matches with the downloaded R version. For example, if you download R version v4.2.2, install the Rtool version 4.2 (not the latest 4.3). You can use the following command to check if Rtools is installed properly:
```
if(!require("devtools"))
  install.packages("devtools")
if(!require("pkgbuild"))
  devtools::install_github("r-lib/pkgbuild")
pkgbuild::find_rtools()
```
This will return `TRUE` if Rtools is installed properly.

#### Requirements for Ubuntu/Debian Systems
Installation of some additional system level packages are required to use RokaiXplorer on Ubuntu. For this purpose, please check out the [Required Package List for Debian Systems](ubuntu_debian_requirements.md).

### Step 2: Create a project in RStudio
For getting started, open RStudio and create a project in a new directory with a desired name. For more information, check out [instructions on creating a project in RStudio](https://swcarpentry.github.io/r-novice-gapminder/02-project-intro/#a-possible-solution).

### Step 3: Download the RokaiXplorer source code
Next, you will need to download the source code of RokaiXplorer. For this purpose, open the RStudio project and run the following code in the console, which will download the source code from Github and place it under ``/RokaiXplorer`` folder in the currect working directory: 

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
If everything works up to this point and all libraries are installed properly, the following code should start the RokaiXplorer application with preloaded sample data, identical to the [Example Application](https://serhan-yilmaz.shinyapps.io/exampleapp/). 
```
devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/src/deploy_from_github.R?raw=TRUE")
options(RokaiXplorer_deployment_mode = TRUE)
runRokaiXplorer()
```
Note that, after running the ```runRokaiXplorer()``` command, the application **will stay open until it is closed**. So, please make sure to **close the application before moving on the next steps and running the next code snippets** (this applies to other steps that include ```runRokaiXplorer()``` command as well). 

### Step 6: Customizing the application for user data & configuration
To customize the application to load your data instead of the sample dataset, first place your data under the ```/RokaiXplorer/data/``` folder in the current directory and run the following code after setting the file paths appropriately:
```
options(RokaiXplorer_data_filepath = "data/<DATA>.csv")
options(RokaiXplorer_metadata_filepath = "data/<METADATA>.csv")
options(RokaiXplorer_use_expression_data = FALSE)
```
In the above code, please make sure to update ```<DATA>``` and ```<METADATA>``` with the names of your data files. 

Additionally, if you would like to use a protein expression data, place the data file under the ```/RokaiXplorer/data/``` and run the following:
```
options(RokaiXplorer_use_expression_data = TRUE)
options(RokaiXplorer_expression_data_filepath = "data/<EXPRESSION_DATA>.csv")
```
Again, please make sure to replace ```<EXPRESSION_DATA>``` with the name of your data file. 

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
To change the application title and subtitle (displayed in the top left corner of the application), run the following code:
```
options(RokaiXplorer_application_title = "NewTitle")
options(RokaiXplorer_application_subtitle = "This will be the new subtitle to display")
runRokaiXplorer()
```
This will change the title to be "NewTitle" and display some description under the title. Change these as you see fit according to your application. 

Next, to specify what to display on the about page, you will need to provide a markdown file. For this purpose, the easiest way is to modify the provided markdown file under ```RokaiXplorer/deploy/description.md``` and write some descriptive text about your application and dataset. If you wish to display any images, make sure to place the image files under ```RokaiXplorer/www/``` folder (otherwise the images will not display properly when the application is deployed online). After setting up the description file, run the following command to view the changes:
```
options(RokaiXplorer_description_filepath = "deploy/description.md")
runRokaiXplorer()
```
For more information on preparing a markdown file, check out the [Markdown cheatsheet](https://www.markdownguide.org/cheat-sheet/). Note that, you can open and edit the .md file in any text file editor.

### Step 8: Deploying the application to Shinyapps.io
#### Creating a shinyapps.io account
To deploy your application to Shinyapps similar to the Example Application, first you need create an account. For this purpose, visit [Shinyapps Sign-up page](https://www.shinyapps.io/admin/#/signup) and set up your account. At the time of this writing (January 2023), setting up shinyapps.io is free and allows up to 5 applications. 

#### Setting up the shinyapps account in RStudio
After setting up your account, visit the [Shinyapps Dashboard](https://www.shinyapps.io/admin/#/dashboard) and log in. Here, you will need to obtain the security token to connect your account in RStudio. Under ```Account -> Tokens``` tabs, select the "Show Token" option which will display the necessary R code for this purpose. Click ```Copy to clipboard``` button to copy the code and run it in RStudio. The copied code should be in the following form: 
```
rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
```
where the ```"<ACCOUNT>"```, ```"<TOKEN>"```, ```"<SECRET>"``` fields are filled according to your account details.
  
For more information on this step, you can read the [instructions on deploying to shinyapps.io](https://docs.posit.co/shinyapps.io/getting-started.html). 

#### Deploying your application 
Finally, to deploy your application, set up the following options based on your shinyapps account and desired application link:
```
  options(RokaiXplorer_shinyapps_account = "<ACCOUNT>")
  options(RokaiXplorer_shinyapps_title = "<TITLE>")
```
Here, ```"<ACCOUNT>"``` should be your shinyapps account name (same as in the above step). When created, the application will have the link:  ```<ACCOUNT>.shinyapps.io/<TITLE>```, so set the ```"<TITLE>"``` variable based on what you wish to display in the link. 
  
After setting all the desired options, please run: 
```
  deployRokaiXplorer()
```
After a couple minutes, this should deploy the application to shinyapps.io and open it in your browser.
  
  
### Putting it all together
- Install [R](https://cran.r-project.org/), [RStudio](https://posit.co/download/rstudio-desktop/), and [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (for Windows). 
- Download the source code and install the necessary libraries:
```
if(!require("devtools"))
  install.packages("devtools")
devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/src/deploy_from_github.R?raw=TRUE")
downloadRokaiXplorer()
installDependencies()
```
- Place your data/metadata under ```RokaiXplorer/data/``` your config file under ```RokaiXplorer/deploy/config.json``` folders (if you decide to use one). Modify the ```RokaiXplorer/deploy/description.md``` file to add a description of your application in the front page. Place any resources (e.g., images if used in the description) under ```RokaiXplorer/www/``` folder.
- Set the deployment options for RokaiXplorer. Here is what they look like for the [Example application](https://serhan-yilmaz.shinyapps.io/ExampleApp/):
```
options(RokaiXplorer_deployment_mode = TRUE)
options(RokaiXplorer_data_filepath = "data/rokaiXplorer_sample_data.csv")
options(RokaiXplorer_metadata_filepath = "data/rokaiXplorer_sample_metadata.csv")
options(RokaiXplorer_use_expression_data = FALSE)
options(RokaiXplorer_reference_proteome = "Uniprot Mouse")
options(RokaiXplorer_config_filepath = "deploy/config.json")
options(RokaiXplorer_application_title = "ExampleApp")
options(RokaiXplorer_application_subtitle = "This is a sample application generated by RokaiXplorer")
options(RokaiXplorer_description_filepath = "deploy/description.md")
options(RokaiXplorer_allow_data_download = TRUE)
```
- Run ```runRokaiXplorer()``` command to see if your application works as expected
- To deploy your application online, set up a [Shinyapps account](https://www.shinyapps.io/admin/#/signup) and [connect it to RStudio](https://docs.posit.co/shinyapps.io/getting-started.html):
```
rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
```
- Next, set up the final details and deploy your application. Here is what it look like for the sample application:
```
  devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/src/deploy_from_github.R?raw=TRUE")
  options(RokaiXplorer_shinyapps_account = "serhan-yilmaz")
  options(RokaiXplorer_shinyapps_title = "ExampleApp")
  deployRokaiXplorer()
```
This makes the application accessible via the link: [https://serhan-yilmaz.shinyapps.io/ExampleApp](https://serhan-yilmaz.shinyapps.io/ExampleApp/)
