#### Tutorial 
- Welcome! This is a quick tutorial to help you get started.

### Input & Options
- This is main area to specify the options for the analysis. We will quickly walk through each of them.

#### Sample Data
- Use this area to load the sample data or download it to view the input file format.

#### Upload Data
- Use this area to upload an input data. For this purpose, you will need at least two data files: 
 - Phosphorylation Data: Intensities for each sample and phosphosite. 
 - Metadata: Specifies the groups of each sample (e.g., Case/Control)
 
##### (Optional) Protein Expression
- Additionally, you can also upload protein expression data to include in the analysis: 
 - Expression Data: Intensities for each sample and protein.
 
##### Phosphorylation Data Format
- The phosphorylation data is a csv file with the following columns: 
 - <b>Protein</b>: Uniprot ID of the protein. 
 - <b>Position</b>: Position of the phosphosite on the protein. 
 - <b>Samples (multiple columns)</b> Intensities for each sample.
 
##### Expression Data Format
- The protein expresssion data is a csv file with the following columns: 
 - <b>Protein</b>: Uniprot ID of the protein. 
 - <b>Samples (multiple columns)</b> Intensities for each sample.

##### Metadata Format
- The metadata is a csv file with the following rows and columns: 
 - <b>RowName (first column):</b> The name of the group specifier. 
 - <b>Samples (multiple columns)</b> The group identities for each sample. 
 - <b>Group (first row):</b> Main group specifying the <em>Case</em>/<em>Control</em> status of the samples. 
 - <b>Other Groups (multiple rows):</b> Optional rows specifying other groups.
 
##### Reference Proteome
- Make sure to select the correct reference proteome before uploading the data. 

#### Subgroup Analysis 
- You can use this area to filter the samples to focus on particular subgroup. The group variables from metadata will appear here.

#### Load Sample Data
- To continue the tutorial, click on the load sample data button. 

#### Grouping Variables
- The grouping variables from sample data now appears here. There are three for sample data: 
 - Timepoint, Gender, and Replicate. 
You can choose any combination of them to customize the analysis.

#### Subgroup Differences
- You can use this area to further customize the analysis, specifying two subgroups to identify ptms or proteins that exhibit the largest difference between those subgroups. 

#### (Optional) Analysis Options
- You can use this area to modify the options regarding data pre-processing and statistical inference.

#### Configuration
- Here, you can save the selected options for future use or generate a link to share them with others.

### Analysis Results
- The analysis results are displayed in this section. Each tab contains the results of a different analysis module with different granularity 
- (e.g., PTM/Protein/Kinase/Pathway-level)

#### Analysis Modules
- Overall, RokaiXplorer supports analyses at four different levels: 
 - <b>PTM</b>: Identify phosphosites with significant dysregulation. 
 - <b>Protein</b>: Identify dysregulated proteins based on protein expression 
   (in Expression tab) or mean phosphorylation (in Phosphoprotein tab). 
 - <b>Enrichment</b>: Perform an over-representation analysis based on the significant phosphosites, phosphoproteins, or expressed proteins to identify enriched gene ontology terms to help interpret the results 
 - <b>Kinase</b>: Infer kinase activities using RoKAI to identify potential drug targets

#### Analysis Views
- The results of each analysis are presented in the form of five types of views: 
 - Volcano plots, Bar plots, Heatmaps, Tables, as well as an Interactive Network!

##### Interactivity and Inspection Window
- Among these different views, the <b> Volcano Plots</b>, <b>Tables</b>, and <b>Network Views</b> are interactive, such that hovering over an item displays additional information, and double clicking brings out the inspection window for further details and plots about the selection 
Note that, opening the inspection window interrupts the interactive tutorial. 

##### Volcano plot tab
- Select this tab to view the volcano plots.
- Volcano plots are the first view for each analysis, providing an overview of the results and about the significance of the findings.
- Volcano plot tab also harbors a panel containing the analysis specific options, such as cutoffs on p-value or other filtering options. These options also affect the results presented in other views.

##### Bar plot tab
- Select this tab to view the bar plots.
- This view with the bar plot focuses on visualizing the top findings. You can use the options below to further customize the plot and download it as an image or PDF.

##### Heatmap tab
- Select this tab to view the heatmaps.
- This view with the heatmap again focuses on the top findings, but displays more detailed, sample-specific information. Similar to bar plots, you can use the options below to further customize the plot and download it as an image or PDF.

##### Table tab - Phosphosites Table
- Select this tab to view the results in the form of a table.
- The table displays detailed information about the statistical analysis and the significance, sorted by phosphosites exhibiting highest dysregulation to the least. Using the button on the top, you can export this table to Excel or as a CSV file. For more information on a phosphosite, you can double click on a row to display the inspection window.

##### Interactive Network Tab
- Select this tab to view the results in the form of an interactive network.
- This network displays the top phosphosites with highest dysregulation and the kinases that are known to target these phosphosites. This network is interactive, so you can drag & drop the nodes to adjust the view, scroll to zoom in and out, hover to display additional information, and double click to display the inspection window. You can further use the options on the right to customize what is shown in the network.

##### Views Summary
- To summarize, use <b> volcano plots</b> to determine the significant items, <b> bar plots</b> and <b>heatmaps</b> to visualize top findings, use <b>tables</b> for detailed view and to export the results, <b>interactive network</b> to visualize the top findings in the form of a network with kinases. 
- The Phosphoproteins and Expression tabs are structured exactly the same way and provides these five views. Thus, next we will focus on Kinases.

#### Kinases Tab
- Select this tab to view the kinase inference results.

##### Views for Kinase Analysis
- The kinase analysis provides the same set of views: <b> Volcano plot</b>, <b>Bar plot</b>, <b>Heatmap</b>, and <b>Table</b> view for further details. 
- In addition, <b>Targets</b> table contains information about the known substrates of the kinases that are identified (having quantifications in the dataset).

##### RoKAI Inference Options
- (Optional) Use this area to customize the options for kinase activity inference, in addition to specifying the cutoffs to determine the statistical significance. These options will also affect the results presented in other views.

##### Kinase Targets Tab
- Select this tab to view the known kinase substrates.
- This table contains information on known substrates of kinases. Using the search bar on the top, you can search for a specific kinase or phosphosite.

#### Enrichment Tab
- Select this tab to view the enrichment results.

##### Views for Enrichment Analysis
- The enrichment analysis provides a subset of the same views: <b> Volcano plot</b>, <b>Table</b> for displaying and downloading the analysis results, and <b>Targets</b> to display information about the proteins related to the enrichment term. 
- In addition, <b>Settings</b> tab contains various options on how the enrichment analysis should be performed, as well as based on which data source (i.e., phosphosites/phosphoproteins/protein expression).

##### Enrichment Settings - Inclusion Criteria
- The options in this panel determines the inclusion criteria for the enrichment terms. This can be based on category of the terms, number of identified proteins related to a term, or the ratio of the observed proteins related to a term. Additionally, it includes filtering options exclude highly similar terms from the analysis.

##### Enrichment Settings - Gene List for Enrichment
- The options in this panel determines the list of proteins deemed as significant based on various cutoffs. When an enrichment term includes a significant protein in its set, it is considered a <em>Hit</em>, otherwise it is considered a <em>Miss</em>.

#### Report Generator Tab
- Select this tab to display the report generator.
- The Report Generator in RokaiXplorer simplifies the process of analyzing data for multiple subgroups and exporting the results as formatted excel tables. Whether you want to investigate the impact of variables like gender or tissue of the sample, this feature enables you to perform separate analyses for each subgroup effortlessly.

##### Report Generator Options
- Use this area to choose analysis type (Phosphorylation, Expression, Kinase Inference, Enrichment) and define grouping variables for subgroup analysis.

##### Exporting the results
- Once you set the desired options for report generator, click 'Run' and download the generated Excel report.

#### End of Tutorial
- This is the end of the tutorial. Hope you enjoyed it! 
- Click on the About tab to return to the home page.

##### Interactive Data Browser
- After finalizing your analysis, if you would like to share your results with others, check out the deployment section to learn about how to create your own interactive data browser with RokaiXplorer. 
- Thank you for sticking with the tutorial until the end!
















