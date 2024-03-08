### Input Data Format
***
The phosphosite quantification data should be a csv file having the following columns:
- <b>Protein (first column):</b> The Uniprot protein identifier. 
- <b>Position (second column):</b> The position of the modified phosphosite on the protein, accepting representation with or without the amino acid (e.g., S236 or 236). 
- <b>Samples (multiple columns):</b> The phosphorylation of the site for the corresponding sample. Note that, the intensities <b>should not be log-transformed</b>, as this step is already performed within the application.

Please see the provided sample data file to see an example.

Note that, in addition, you will also need to upload a metadata file to specify which samples are in case or control groups.
