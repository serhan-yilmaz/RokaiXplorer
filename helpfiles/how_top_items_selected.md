### How are top items determined?
***
Given we have an estimate of log2-FC and the corresponding standard error for each item:
 - \\(q_i\\): Log fold change for item \\(i\\)
 - \\(s_i\\): Standard error of the log fold change for item \\(i\\)

We compute an adjusted score to quantify the effective magnitude: 
- \\(q'_i = |q_i| - 3 s_i\\)

Then, we sort the terms according to \\(q'_i\\) and obtain the top items.

The logic behind here is that we would like to put forward items with high effect size \(|q_i|\), but consider the first 3 standard errors to be unreliable (thus, we subtract it). 

The advantage of this scoring compared to z-score (\\( |q_i|/s_i) \\) is that, for items with \\(|q_i| \gg s_i\\), z-score can skimp on items with high effect size in favor of those with negligible effect size, but near-zero standard error. Whereas, in \\(|q'_i|\\), the effect size is prioritized beyond the expected deviation. 

Note that, we adjust for 3 standard errors as opposed to the 95% level (\\(\approx\\)1.96) to account for the inflated error due to multiple comparisons. We estimate that this can roughly adjust for the false discovery rate of 500 comparisons at 0.05 level. 