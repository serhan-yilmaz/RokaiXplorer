### Differential Group Analysis
***
In this section, you can specify two subgroups A and B to perform a differential analysis. Here, the aim is to uncover modifications with a higher fold change in A compared to B. 

As a first step in this analysis, log2 fold changes are computed separately for each group:
- \(Q_A\): Log fold changes in subgroup A (comparing case & control samples).
- \(Q_B\): Log fold changes in subgroup B (comparing case & control samples).

Then, their difference is computed:
- \(Q_{AB} = Q_A - Q_B\)

After that, rest of the analysis continues as usual using \(Q_{AB}\).