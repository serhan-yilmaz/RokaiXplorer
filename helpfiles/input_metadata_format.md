### Input Metadata Format
***
The aim of the metadata is to specify the groups of the samples (e.g., their case/control status). This should be a csv file having the following rows and columns:
- <b>RowName (first column):</b> The name of the group specifier.
- <b>Samples (multiple columns):</b> The group identities for each sample.
- <b>Group (first row):</b> Main group specifying the <em>Case</em>/<em>Control</em> status of the samples.
- <b>Other Groups (multiple rows):</b> Optional rows specifying other groups of the samples. 
- <b>Tissue (example row):</b> An example row specifying the tissue of the samples.

Note that, the groups can be anything and can be used to specify a subset of samples. Only the main group specifying case/control status is necessary, the remaining are optional.

Please see the provided sample metadata file to see an example.

