### Protein Expression Data Format
***
The protein expression data should be a csv file having the following columns:
- <b>Protein (first column):</b> The Uniprot protein identifier. 
- <b>Samples (multiple columns):</b>, The expression of the protein for the corresponding sample. The intensities are not expected to be log-transformed (this step is performed within the app).

Note that, in addition, you will also need to upload a metadata file to specify which samples are in case or control groups.
