### Data File Format
***
The phosphosite quantification data is a csv file having the following columns:
- <b>Protein (first column):</b> The Uniprot protein identifier. 
- <b>Position (second column):</b> The position of the modified site on the protein.
- <b>Samples (multiple columns):</b>, The phosphorylation of the site for the corresponding sample. The intensities are not  log-transformed (this step is performed within the app).

Similarly, protein expression data is a csv file having the following columns:
- <b>Protein (first column):</b> The Uniprot protein identifier. 
- <b>Samples (multiple columns):</b>, The expression of the protein for the corresponding sample. The intensities are not  log-transformed (this step is performed within the app).

In addition, the metadata file specifies which samples are in case or control groups. This is a csv file having the following rows and columns:
- <b>RowName (first column):</b> The name of the group specifier.
- <b>Samples (multiple columns):</b> The group identities for each sample.
- <b>Group (first row):</b> Main group specifying the <em>Case</em>/<em>Control</em> status of the samples.
- <b>Other Groups (multiple rows):</b> Optional rows specifying other groups of the samples. 
- <b>Tissue (example row):</b> An example row specifying the tissue of the samples.

Note that, the groups can be anything and can be used to specify a subset of samples. Only the main group specifying case/control status is necessary, the remaining are optional.
