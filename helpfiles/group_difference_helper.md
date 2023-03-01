### Differential Group Analysis
***
This is an optional setting to perform a specialized differential analysis between two subgroups A and B. 
Here, the aim is to uncover modifications with a higher fold change in A compared to B. 

As a first step in this analysis, log2 fold changes are computed separately for each group:
- \\(Q_A\\): Log fold changes in subgroup A (comparing case & control samples).
- \\(Q_B\\): Log fold changes in subgroup B (comparing case & control samples).

Then, their difference is computed:
- \\(Q_{AB} = Q_A - Q_B\\)

After that, rest of the analysis continues as usual using \(Q_{AB}\).

Note that, the selected group for differential analysis should be different than the groups selected to filter the samples: 
- For example, if 'Gender: Male' is selected for filtering and Gender is also selected for differential analysis, this would result in an invalid query. 
- Whereas, if 'Timepoint: 3 Month' is selected for filtering and Gender is selected for differential analysis, the resulting query would investigate the differences in Male/Females samples on the timepoint = 3 month data (after filtering). 