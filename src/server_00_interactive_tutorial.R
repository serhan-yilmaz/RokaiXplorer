guide <- Cicerone$
  new()$ 
  step("about_tutorial_main_div", 
       "Welcome",
       "This is a quick tutorial to help you get started. Click next to continue."
  )$
  step("main_control_div", 
       "Input & Options",
       "This is main area to specify the options for the analysis. ", 
       position = "right"
  )$
  step("main_control_div2", 
       "Input & Options",
       "We will quickly walk through each of them.",
       position = "right"
  )$
  step(
    "sample_data_div",
    "Sample Data",
    "Use this area to load the sample data or download it to view the input file format.", 
  )$
  step(
    "upload_data_div",
    "Upload Data",
    "Use this area to upload an input data. For this purpose, you will need at least two data files: <br> - Phosphorylation Data: Intensities for each sample and phosphosite. <br> - Metadata: Specifies the groups of each sample (e.g., Case/Control)",
  )$
  step(
    "upload_data_div2",
    "(Optional) Protein Expression",
    "Additionally, you can also upload protein expression data to include in the analysis: <br> - Expression Data: Intensities for each sample and protein.",
  )$
  step(
    "phospho_data_div",
    "Phosphorylation Data Format",
    "The phosphorylation data is a csv file with the following columns: <br> - <b>Protein</b>: Uniprot ID of the protein. <br> - <b>Position</b>: Position of the phosphosite on the protein. <br> - <b>Samples (multiple columns)</b> Intensities for each sample. ",
  )$
  step(
    "expression_data_div",
    "Expression Data Format",
    "The protein expresssion data is a csv file with the following columns: <br> - <b>Protein</b>: Uniprot ID of the protein. <br> - <b>Samples (multiple columns)</b> Intensities for each sample. ",
  )$
  step(
    "metadata_upload_div",
    "Metadata Format",
    "The metadata is a csv file with the following rows and columns: <br> - <b>RowName (first column):</b> The name of the group specifier. <br> - <b>Samples (multiple columns)</b> The group identities for each sample. <br> - <b>Group (first row):</b> Main group specifying the <em>Case</em>/<em>Control</em> status of the samples. <br> - <b>Other Groups (multiple rows):</b> Optional rows specifying other groups.",
  )$
  step(
    "refproteome_div", 
    "Reference Proteome",
    "Make sure to select the correct reference proteome before uploading the data."
  )$
  step("optionbox_filter_by_subgroup_wrapper", 
       "Subgroup Analysis",
       "You can use this area to filter the samples to focus on particular subgroup. The group variables from metadata will appear here."
  )$
  step("load_sample_data_div", 
       "Load Sample Data",
       "To continue the tutorial, click on the load sample data button."
  )$
  step("optionbox_filter_by_subgroup_wrapper2", 
       "Grouping Variables",
       "The grouping variables from sample data now appears here. There are three for sample data: <br> - Timepoint, Gender, and Replicate <br> You can choose any combination of them to customize the analysis.",
       position = "right", 
  )$
  step("optionbox_subgroup_differences_wrapper", 
       "Subgroup Differences",
       "You can use this area to further customize the analysis, specifying two subgroups to identify ptms or proteins that exhibit the largest difference between those subgroups. ",
       position = "right", 
  )$
  step("optionbox_options_wrapper", 
       "(Optional) Analysis Options",
       "You can use this area to modify the options regarding data pre-processing and statistical inference."
  )$
  step("config_optionbox_wrapper", 
       "Configuration",
       "Here, you can save the selected options for future use or generate a link to share them with others."
  )$
  step("main_output_div", 
       "Analysis Results",
       "The analysis results are displayed in this section. Each tab contains the results of a different analysis module with different granularity <br> (e.g., PTM/Protein/Kinase/Pathway-level)",
       class = "analysis_results_tutorial_cicerone1", 
       position = "bottom", 
  )$
  step("main_output_div2", 
       "Analysis Modules",
       "Overall, RokaiXplorer supports analyses at four different levels: <br> - <b>PTM</b>: Identify phosphosites with significant dysregulation. <br> - <b>Protein</b>: Identify dysregulated proteins based on protein expression <br> (in Expression tab) or mean phosphorylation (in Phosphoprotein tab). <br> - <b>Enrichment</b>: Perform an over-representation analysis based on <br> the significant phosphosites, phosphoproteins, or expressed proteins to identify enriched gene ontology terms to help interpret the results <br> - <b>Kinase</b>: Infer kinase activities using RoKAI to identify potential drug targets",
       class = "analysis_results_tutorial_cicerone", 
       position = "bottom", 
  )$
  step("main_output_div", 
       "Analysis Views",
       "The results of each analysis are presented in the form of five types of views: Volcano plots, Bar plots, Heatmaps, Tables, as well as an Interactive Network!",
       class = "analysis_results_tutorial_cicerone1", 
       position = "bottom", 
  )$
  step("main_output_div2", 
       "Interactivity and Inspection Window",
       "Among these different views, the <b> Volcano Plots</b>, <b>Tables</b>, and <b>Network Views</b> are interactive, such that hovering over an item displays additional information, and double clicking brings out the inspection window for further details and plots about the selection. <br> (Note that, opening the inspection window will interrupt the tutorial)",
       class = "analysis_results_tutorial_cicerone1", 
       position = "bottom", 
  )$
  step(
    "[data-value='Volcano Plot']",
    "Volcano plot tab",
    "Select this tab to view the volcano plots.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_volcanoplot', {priority: 'event'});}", 
    is_id = FALSE
  )$
  step("sitelevel_volcano_wrapper_div", 
       "Volcano plots",
       "Volcano plots are the first view for each analysis, providing an overview of the results and about the significance of the findings.",
  )$
  step("sitelevel_volcano_analysisopts_div", 
       "Analysis Specific Options",
       "Volcano plot tab also harbors a panel containing the analysis specific options, such as cutoffs on p-value or other filtering options. These options also affect the results presented in other views.",
  )$
  step(
    "[data-value='Bar Plot']",
    "Bar plot tab",
    "Select this tab to view the bar plots.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_barplot', {priority: 'event'});}", 
    is_id = FALSE
  )$
  step(
    el = "site_barplot_wrapper_div",
    title = "Bar Plot",
    description = "This view with the bar plot focuses on visualizing the top findings. You can use the options below to further customize the plot and download it as an image or PDF."
    #on_highlight_started = "Shiny.setInputValue('foo2', 'qfds', {priority: 'event'});",
    #tab = "Plot",
    #tab_id = "mainTabset"
  )$
  step(
    "[data-value='Heatmap']",
    "Heatmap tab",
    "Select this tab to view the heatmaps.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_heatmap', {priority: 'event'});}", 
    is_id = FALSE
  )$
  step(
    el = "site_heatmap_wrapper_div",
    title = "Heatmap",
    description = "This view with the heatmap again focuses on the top findings, but displays more detailed, sample-specific information. Similar to bar plots, you can use the options below to further customize the plot and download it as an image or PDF.",
    position = "bottom", 
  )$
  step(
    "[data-value='Table']",
    "Table tab",
    "Select this tab to view the results in the form of a table.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_table', {priority: 'event'});}", 
    is_id = FALSE,
  )$
  step(
    el = "site_table_div",
    title = "Phosphosites Table",
    description = "The table displays detailed information about the statistical analysis and the significance, sorted by phosphosites exhibiting highest dysregulation to the least. Using the button on the top, you can export this table to Excel or as a CSV file. For more information on a phosphosite, you can double click on a row to display the inspection window.",
    class = "analysis_results_tutorial_cicerone", 
    position = "bottom", 
  )$
  step(
    "[data-value='Interactive Network']",
    "Interactive Network Tab",
    "Select this tab to view the results in the form of an interactive network.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_network', {priority: 'event'});}", 
    is_id = FALSE,
  )$
  step(
    el = "site_kinase_network_wrapper_div",
    title = "Interactive Network",
    description = "This network displays the top phosphosites with highest dysregulation and the kinases that are known to target these phosphosites. This network is interactive, so you can drag & drop the nodes to adjust the view, scroll to zoom in and out, hover to display additional information, and double click to display the inspection window. You can further use the options on the right to customize what is shown in the network.",
    class = "analysis_results_tutorial_cicerone", 
    position = "bottom", 
  )$
  step(
    el = "siteTabset",
    title = "Views Summary",
    description = "To summarize, use <b> volcano plots</b> to determine the significant items, <b> bar plots</b> and <b>heatmaps</b> to visualize top findings, use <b>tables</b> for detailed view and to export the results, <b>interactive network</b> to visualize the top findings in the form of a network with kinases. <br> The Phosphoproteins and Expression tabs are structured exactly the same way and provides these five views. Thus, next we will focus on Kinases.",
    class = "analysis_results_tutorial_cicerone", 
    position = "bottom", 
  )$
  step(
    "[data-value='Kinase']",
    "Kinases Tab",
    "Select this tab to view the kinase inference results.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_kinases', {priority: 'event'});}", 
    is_id = FALSE,
  )$
  step(
    el = "kinaseTabset",
    title = "Views for Kinase Analysis",
    description = "The kinase analysis provides the same set of views: <b> Volcano plot</b>, <b>Bar plot</b>, <b>Heatmap</b>, and <b>Table</b> view for further details. In addition, <b>Targets</b> table contains information about the known substrates of the kinases that are identified (having quantifications in the dataset).",
    class = "analysis_results_tutorial_cicerone", 
    position = "bottom", 
  )$
  step("kinaselevel_volcano_analysisopts_div", 
       "RoKAI Inference Options",
       "(Optional) Use this area to customize the options for kinase activity inference, in addition to specifying the cutoffs to determine the statistical significance. These options will also affect the results presented in other views.",
       class = "analysis_results_tutorial_cicerone1", 
  )$
  # step(
  #   "[data-value='Table']",
  #   "Kinases Tab",
  #   "Select this tab to get more detailed information about the kinases.",
  #   on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_kinase_table', {priority: 'event'});}", 
  #   is_id = FALSE
  # )$
  # step(
  #   el = "kinase_table_div",
  #   title = "Kinase Table",
  #   description = "This table contains information on the inferred activies. Using the control panel on the top, you can search for a specific kinase or download the results. "
  # )$
  step(
    "[data-value='Targets']",
    "Kinase Targets Tab",
    "Select this tab to view the known kinase substrates.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_kinase_targets', {priority: 'event'});}", 
    is_id = FALSE,
  )$
  step(
    el = "kinase_targets_table_div",
    title = "Kinase Targets",
    description = "This table contains information on known substrates of kinases. Using the search bar on the top, you can search for a specific kinase or phosphosite. "
  )$
  step(
    "[data-value='Enrichment']",
    "Enrichment Tab",
    "Select this tab to view the enrichment results.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_enrichment', {priority: 'event'});}", 
    is_id = FALSE,
  )$
  step(
    el = "enrichmentTabset",
    title = "Views for Enrichment Analysis",
    description = "The enrichment analysis provides a subset of the same views: <b> Volcano plot</b>, <b>Table</b> for displaying and downloading the analysis results, and <b>Targets</b> to display information about the proteins related to the enrichment term. In addition, <b>Settings</b> tab contains various options on how the enrichment analysis should be performed, as well as based on which data source (i.e., phosphosites/phosphoproteins/protein expression).",
    class = "analysis_results_tutorial_cicerone", 
    position = "bottom", 
  )$
  step(
    "optionbox_enrichment_inclusion_criteria",
    "Enrichment Settings - Inclusion Criteria",
    "The options in this panel determines the inclusion criteria for the enrichment terms. This can be based on category of the terms, number of identified proteins related to a term, or the ratio of the observed proteins related to a term. Additionally, it includes filtering options exclude highly similar terms from the analysis.",
    class = "analysis_results_tutorial_cicerone1", 
  )$
  step(
    "optionbox_enrichment_background_set_proteins",
    "Enrichment Settings - Background Set",
    "The options in this panel determines the background set of proteins deemed as significant based on various cutoffs. When an enrichment term includes a significant protein in its set, it is considered a <em>Hit</em>, otherwise it is considered a <em>Miss</em>.",
    class = "analysis_results_tutorial_cicerone1", 
  )$step(
    "[data-value='Report Generator']",
    "Report Generator Tab",
    "Select this tab to display the report generator.",
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_reportgenerator', {priority: 'event'});}", 
    is_id = FALSE,
  )$step(
    "report_generator_div",
    "Report Generator",
    "The Report Generator in RokaiXplorer simplifies the process of analyzing data for multiple subgroups and exporting the results as formatted excel tables. Whether you want to investigate the impact of variables like gender or tissue of the sample, this feature enables you to perform separate analyses for each subgroup effortlessly.",
    class = "analysis_results_tutorial_cicerone1", 
  )$step(
    "report_generator_options",
    "Report Generator Options",
    "Use this area to choose analysis type (Phosphorylation, Expression, Kinase Inference, Enrichment) and define grouping variables for subgroup analysis.",
  )$step(
    "report_generator_buttons",
    "Exporting the results",
    "Once you set the desired options for report generator, click 'Run' and download the generated Excel report.",
  )$
  step(
    "[data-value='About']",
    "End of Tutorial",
    "This is the end of the tutorial. Hope you enjoyed it! Click on the About tab to return to the home page.",
    is_id = FALSE,
    on_highlighted = "function(element){Shiny.setInputValue('foo2', 'step_about_end', {priority: 'event'});}", 
  )$
  step(
    "about_interactive_data_browser_div",
    "Interactive Data Browser",
    "After finalizing your analysis, if you would like to share your results with others, check out the deployment section to learn about how to create your own interactive data browser with RokaiXplorer. <br> Thank you for sticking with the tutorial until the end!",
    class = "analysis_results_tutorial_cicerone1", 
  )

  # step("about_main_div", 
  #      "End of Tutorial",
  #      "This is the end of the tutorial. Hope you enjoyed it!"
  # )

observeEvent(input$interactiveTutorialLink, {
  main_logging("Interactive Tutorial")
  guide$init()$start()
})

observeEvent(guide$get_next(), {
  a <- guide$get_next()
  if(!is.null(a)){
    b <- a$highlighted
    if(b == "load_sample_data_div"){
      foLoadSampleData()
    }
    if(b == "main_control_div2"){
      foUncollapseBoxIfNeeeded("optionbox_data_input", nullval = F)
      foUncollapseBoxIfNeeeded("optionbox_filter_by_subgroup")
      foUncollapseBoxIfNeeeded("optionbox_subgroup_differences")
      foUncollapseBoxIfNeeeded("optionbox_options")
      foUncollapseBoxIfNeeeded("config_optionbox")
      # val = input[["mortgage_optionbox_collapse"]]
      # if(!is.null(val)){
      #   js$collapse("mortgage_optionbox")
      # }
    }
    if(b == "config_optionbox_wrapper"){
      foUncollapseBoxIfNeeeded("optionbox_filter_by_subgroup", collapse = T)
      foUncollapseBoxIfNeeeded("optionbox_subgroup_differences", collapse = T)
      foUncollapseBoxIfNeeeded("optionbox_options", collapse = T)
      foUncollapseBoxIfNeeeded("config_optionbox", collapse = T)
    }
  }
})

observeEvent(input$foo2, {
  switch(input$foo2,
         "step_volcanoplot" = {
           updateTabsetPanel(session, "mainTabset", "Phosphosite");
           updateTabsetPanel(session, "siteTabset", "Volcano Plot")},
         "step_barplot" = {
           updateTabsetPanel(session, "mainTabset", "Phosphosite");
           updateTabsetPanel(session, "siteTabset", "Bar Plot")},
         "step_heatmap" = {
           updateTabsetPanel(session, "mainTabset", "Phosphosite");
           updateTabsetPanel(session, "siteTabset", "Heatmap")},
         "step_table" = {
           updateTabsetPanel(session, "mainTabset", "Phosphosite");
           updateTabsetPanel(session, "siteTabset", "Table")},
         "step_network" = {
           updateTabsetPanel(session, "mainTabset", "Phosphosite");
           updateTabsetPanel(session, "siteTabset", "Interactive Network")},
         "step_kinases" = {
           updateTabsetPanel(session, "mainTabset", "Kinase");
           updateTabsetPanel(session, "kinaseTabset", "Volcano Plot")},
         "step_kinase_table" = {
           updateTabsetPanel(session, "mainTabset", "Kinase");
           updateTabsetPanel(session, "kinaseTabset", "Table")},
         "step_kinase_targets" = {
           updateTabsetPanel(session, "mainTabset", "Kinase");
           updateTabsetPanel(session, "kinaseTabset", "Targets")},
         "step_enrichment" = {
           updateTabsetPanel(session, "mainTabset", "Enrichment");
           updateTabsetPanel(session, "kinaseTabset", "Settings")},
         "step_reportgenerator" = {
           updateTabsetPanel(session, "mainTabset", "Report Generator");
         },
         "step_about_end" = {
           updateTabsetPanel(session, "mainTabset", "About");
           updateTabsetPanel(session, "aboutTabset", "Welcome")}
  )
  #updateTabsetPanel(session, "aboutTabset", "How to cite us?")
  #message(paste("xyzds - ", input$foo2, sep = ""))
})