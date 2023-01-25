### Input Data Format
***
The phosphosite quantification data should be a csv file having the following columns:
- <b>Protein (first column):</b> The Uniprot protein identifier. 
- <b>Position (second column):</b> The position of the modified phosphosite on the protein.
- <b>Samples (multiple columns):</b>, The phosphorylation of the site for the corresponding sample. The intensities are not expected to be log-transformed (this step is performed within the app).

Please see the provided sample data file to see an example.

Note that, in addition, you will also need to upload a metadata file to specify which samples are in case or control groups.
